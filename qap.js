class Qap {
    constructor(DIM) {
        this.points = new Set();
        this.exclude = {}
        this.dim = DIM;
    }

    add(point) {
        if (point >= Math.pow(2, this.dim))
            throw new RangeError(`Point too large for dimension ${this.dim}`);
        if (this.excludesCount(point))
            throw new RangeError("Point excluded");
        // If we already have the point, just silently return
        // If we try to create excludes with a point we already have, we'll mess up our cap
        if (this.points.has(point))
            return;
        // Update the exclude
        for (let p1 of this.points) {
            for (let p2 of this.points) {
                if (p1 < p2) {
                    let exc = point ^ p1 ^ p2;
                    if (!this.exclude[exc])
                        this.exclude[exc] = [];
                    // None of these points can be in this.exclude[exc] yet.
                    // Multiple excludes requires disjoint triples.
                    // We put the smallest point first for simpler locating
                    if (point < p1)
                        this.exclude[exc].push(point, p1, p2)
                    else
                        this.exclude[exc].push(p1, point, p2)
                }
            }
        }
        // Add the point
        this.points.add(point)
    }

    remove(point) {
        if (!this.points.has(point))
            return;
        this.points.delete(point);
        // Trim the excludes.
        for (let p1 of this.points) {
            for (let p2 of this.points) {
                if (p1 < p2) {
                    let exc = point ^ p1 ^ p2;
                    let del_index = this.exclude[exc].indexOf(Math.min(point, p1));
                    if (del_index == -1)
                        throw "Trying to remove point that wasn't included"
                    this.exclude[exc].splice(del_index, 3);
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
        return this.exclude[point].length / 3;
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
}
