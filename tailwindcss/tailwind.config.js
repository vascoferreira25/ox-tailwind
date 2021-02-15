module.exports = {
  purge: [],
  darkMode: 'class', // or 'media' or 'class'
  theme: {
    extend: {
		colors: {
			midgray: "#232729",
			darkgray: "#131516"
		}
	},
  },
  variants: {
    extend: {
		backgroundColor: ['checked'],
		borderColor: ['checked'],
	},
  },
  plugins: [],
}
