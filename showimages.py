import numpy as np
import matplotlib.pyplot as plt

data = np.loadtxt("semeion.data.data", delimiter=" ")

for i in range(1, 11):
	index = np.where(data[:,-i] == 1)[0][0]
	im = data[index]
	im = im[0:256]
	
	image = np.reshape(im, (16, 16))
	
	row = (i % 2) + 1
	col = (i % 5) + 1
	
	print row, col
	plt.subplot(3, 4, i)
	plt.imshow(image, cmap="binary")
	plt.axis('off')
	
plt.suptitle("Examples of resampled digit images", fontsize=18)
plt.savefig("ImageExamples.pdf")

	