class Qap {
    constructor(DIM) {
        this.points = new Set();
        this.exclude = {}
        this.dim = DIM;
    }

    add(point) {
        if (point >= Math.pow(2, this.dim))
            throw new RangeError(`Point too large for dimension ${this.dim}`);
        if (this.quadComplete(point))
            throw new RangeError("Point excluded");
        // If we already have the point, just silently return
        // If we try to create excludes with a point we already have, we'll mess up our cap
        if (this.points.has(point))
            return;

        this.points.add(point)
        // Combinations of k-size with the newly added point
        for (let sumLen = 3; sumLen <= this.points.size; sumLen += 2) {
            for (let combination of this.point_combinations(point, sumLen)) {
                let sum = 0;
                for (let p of combination)
                    sum ^= p;
                if (sum == point)
                    continue;
                if (!this.exclude[sum]) {
                    this.exclude[sum] = {}
                }
                if (!this.exclude[sum][sumLen]) {
                    this.exclude[sum][sumLen] = []
                }
                let arr = this.exclude[sum][sumLen];
                arr.push(combination);
                this.exclude[sum][sumLen] = arr;
            }
        }
    }

    remove(point) {
        if (!this.points.has(point))
            return;
        this.points.delete(point);

        // Trim the excludes.
        for (let exc in this.exclude) {
            // The affine combinations of a certain length
            for (let sumLen in this.exclude[exc]) {
                let affCombos = this.exclude[exc][sumLen];
                // Trim the affine combinations that include the point
                var i = 0
                while (i < affCombos.length) {
                    let affCombo = affCombos[i];
                    // If the affine combination includes our target point, remove affine combination
                    if (affCombo.includes(point))
                        affCombos.splice(i, 1)
                    // Otherwise, move to the next
                    else
                        i++
                }
                // If there does not exist anymore affine combinations of this length, delete key
                if (affCombos.length == 0) {
                    delete this.exclude[exc][sumLen];
                }
            }
        }
    }
    contains(point) {
        return this.points.has(point);
    }
    excludesCount(point) {
        if (!this.exclude[point])
            return 0;
        let toReturn = []
        for (const sumLen in this.exclude[point]) {
            toReturn.push(sumLen)
        }
        return toReturn;
    }
    /**
     * Returns if a point is a sum of three cap-points
     */
    quadComplete(point) {
        let excArr = this.excludesCount(point)
        if (excArr == 0 || excArr.length == 0)
            return false;
        return excArr.includes('3')
    }

    excludesTriples(point) {
        return this.exclude[point];
    }
    clear() {
        this.points = new Set();
        this.exclude = {};
    }
    isComplete() {
        // This should be cached
        for (let i = 0; i < Math.pow(2, this.dim); i++) {
            if (!this.points.has(i) && !this.excludesCount(i))
                return false
        }
        return true;
    }
    complete() {
        for (let i = 0; i < Math.pow(2, this.dim); i++) {
            if (!this.points.has(i) && !this.excludesCount(i))
                this.add(i);
        }
    }
    random(next, done) {
        // I am defining "a random qap" in the naive way:
        //   a qap constructed by randomly following the contruction tree, from the current qap.
        // This function adds a random point, then calls the callback.
        // Normally, clear() is called before this.
        const nP = Math.pow(2, this.dim) -
            (
                this.points.size +
                Object.values(this.exclude).filter(x => x.length).length
            )
        if (nP == 0) {
            setTimeout(done);
            return;
        }
        const indexToAdd = Math.floor(Math.random() * nP);
        let found = 0;
        for (let i = 0; i < Math.pow(2, this.dim); i++) {
            if (!this.points.has(i) && !this.excludesCount(i)) {
                if (found == indexToAdd) {
                    this.add(i);
                    setTimeout(next);
                    return;
                }
                found++;
            }
        }
        throw "ops"
    }
    size() {
        return this.points.size;
    }
    changeDim(newDim) {
        for (let p of this.points) {
            if (p > Math.pow(2, newDim))
                this.remove(p);
        }
        this.dim = newDim;
    }

    // function for exchanging two rows of a matrix
    swap(mat, row1, row2, col) {
        for (var i = 0; i < col; i++) {
            var temp = mat[row1][i];
            mat[row1][i] = mat[row2][i];
            mat[row2][i] = temp;
        }
    }
    transpose(matrix) {
        return matrix[0].map((col, i) => matrix.map(row => row[i]));
    }

    rank() {
        if (this.size() < 2)
            return this.size();
        var M = [];
        let p1 = null;
        for (let point of this.points) {
            if (p1 == null) {
                p1 = point;
                continue;
            }
            var translate = point ^ p1;

            var paddedBinary = this.pad(parseInt(translate, 10).toString(2), this.dim);
            let arrOfNum = paddedBinary.split('').map(str => {
                return Number(str);
            });
            M.push(arrOfNum);
        }
        let transpose = this.transpose(M);
        return this.rankOfMatrix(transpose, this.dim, M.length) + 1;
    }

    // function for finding rank of matrix
    // R = number of rows
    // C = number of columns
    rankOfMatrix(mat, R, C) {
        var rank = C;
        for (let row = 0; row < rank; row++) {
            if (mat[row] != undefined && mat[row][row] != 0) {
                for (let col = 0; col < R; col++) {
                    if (col == row)
                        continue;
                    // This makes all entries
                    // of current column
                    // as 0 except entry
                    // 'mat[row][row]'
                    var mult = mat[col][row] / mat[row][row];
                    mult %= 2;
                    for (i = 0; i < rank; i++) {
                        mat[col][i] -= mult * mat[row][i];
                        mat[col][i] %= 2;
                    }

                }
            } else {
                var reduce = true;
                // Find the non-zero element in current column
                for (var i = row + 1; i < R; i++) {
                    // Swap the row with non-zero
                    // element with this row.
                    if (mat[i][row] == 0)
                        continue;
                    this.swap(mat, row, i, rank);
                    reduce = false;
                    break;
                }
                // If we did not find any row with non-zero element in current column, 
                // then all values in this column are 0.
                if (reduce) {
                    // Reduce number of columns
                    rank--;
                    // Copy the last column here
                    for (var i = 0; i < R; i++)
                        mat[i][row] = mat[i][rank];
                }
                row--;
            }
        }
        return rank;
    }

    pad(s, size) {
        while (s.length < size) s = "0" + s;
        return s;
    }
    point_combinations(point, k) {
        this.points.delete(point)
        let combs = this.k_combinations(Array.from(this.points), k - 1)
        this.points.add(point)
        for (let comb of combs) {
            comb.push(point)
        }
        return combs;
    }

    /**
     * Gets k-sized combinations of a given array
     * Copyright 2012 Akseli Palén.
     * Created 2012-07-15.
     * Licensed under the MIT license.
     */
    k_combinations(set, k) {
        var i, j, combs, head, tailcombs;
        if (k > set.length || k <= 0) {
            return [];
        }
        if (k == set.length) {
            return [set];
        }
        if (k == 1) {
            combs = [];
            for (i = 0; i < set.length; i++) {
                combs.push([set[i]]);
            }
            return combs;
        }
        combs = [];
        for (i = 0; i < set.length - k + 1; i++) {
            head = set.slice(i, i + 1);
            tailcombs = this.k_combinations(set.slice(i + 1), k - 1);
            for (j = 0; j < tailcombs.length; j++) {
                combs.push(head.concat(tailcombs[j]));
            }
        }
        return combs;
    }
}
