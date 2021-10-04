import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import scipy.integrate as integrate

def dx(x, y):
    return y-x**3+x

def dy(x):
    return -x

def vector(state, t):
    x, y = state
    nextx = dx(x,y)
    nexty = dy(y)
    return nextx, nexty

#初期値は不安定固定点
x0 = 0.0
y0 = 0.0

t = np.arange(0.0, 2.0, 0.01)

v = integrate.odeint(vector, [x0, y0], t)

x_vec = v[:,0] 
y_vec = v[:,1]

plt.figure()
plt.plot(x_vec, y_vec)
plt.xlabel("x")
plt.ylabel("y")
plt.title("Nullcline")

#作図範囲を設定するパラメータ
p = 2.0
xmax, xmin = x_vec.max() + p,  x_vec.min()- p
ymax, ymin = y_vec.max() + p,  y_vec.min() - p

X, Y = np.meshgrid(np.arange(xmin, ymax, 0.1), np.arange(xmin, ymax, 0.1))
dX = dx(X, Y)
dY = dy(Y)
plt.quiver(X, Y, dX, dY)

plt.contour(X, Y, dY, levels=[0], colors="red")
plt.contour(X, Y, dX, levels=[0], colors="Blue")
plt.xlim([xmin, xmax])
plt.ylim([ymin, ymax])
plt.grid()
plt.savefig('test.png')
plt.show()