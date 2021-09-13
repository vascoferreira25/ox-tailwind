module.exports = {
  purge: [],
  darkMode: 'class', // or 'media' or 'class'
  theme: {
    extend: {
		colors: {
			midgray: "#2B3033",
			darkgray: "#1E1E1E"
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
