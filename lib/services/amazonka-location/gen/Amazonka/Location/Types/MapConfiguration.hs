{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Location.Types.MapConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.MapConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the map tile style selected from an available provider.
--
-- /See:/ 'newMapConfiguration' smart constructor.
data MapConfiguration = MapConfiguration'
  { -- | Specifies the political view for the style. Leave unset to not use a
    -- political view, or, for styles that support specific political views,
    -- you can choose a view, such as @IND@ for the Indian view.
    --
    -- Default is unset.
    --
    -- Not all map resources or styles support political view styles. See
    -- <https://docs.aws.amazon.com/location/latest/developerguide/map-concepts.html#political-views Political views>
    -- for more information.
    politicalView :: Prelude.Maybe Prelude.Text,
    -- | Specifies the map style selected from an available data provider.
    --
    -- Valid
    -- <https://docs.aws.amazon.com/location/latest/developerguide/esri.html Esri map styles>:
    --
    -- -   @VectorEsriDarkGrayCanvas@ – The Esri Dark Gray Canvas map style. A
    --     vector basemap with a dark gray, neutral background with minimal
    --     colors, labels, and features that\'s designed to draw attention to
    --     your thematic content.
    --
    -- -   @RasterEsriImagery@ – The Esri Imagery map style. A raster basemap
    --     that provides one meter or better satellite and aerial imagery in
    --     many parts of the world and lower resolution satellite imagery
    --     worldwide.
    --
    -- -   @VectorEsriLightGrayCanvas@ – The Esri Light Gray Canvas map style,
    --     which provides a detailed vector basemap with a light gray, neutral
    --     background style with minimal colors, labels, and features that\'s
    --     designed to draw attention to your thematic content.
    --
    -- -   @VectorEsriTopographic@ – The Esri Light map style, which provides a
    --     detailed vector basemap with a classic Esri map style.
    --
    -- -   @VectorEsriStreets@ – The Esri Street Map style, which provides a
    --     detailed vector basemap for the world symbolized with a classic Esri
    --     street map style. The vector tile layer is similar in content and
    --     style to the World Street Map raster map.
    --
    -- -   @VectorEsriNavigation@ – The Esri Navigation map style, which
    --     provides a detailed basemap for the world symbolized with a custom
    --     navigation map style that\'s designed for use during the day in
    --     mobile devices.
    --
    -- Valid
    -- <https://docs.aws.amazon.com/location/latest/developerguide/HERE.html HERE Technologies map styles>:
    --
    -- -   @VectorHereContrast@ – The HERE Contrast (Berlin) map style is a
    --     high contrast detailed base map of the world that blends 3D and 2D
    --     rendering.
    --
    --     The @VectorHereContrast@ style has been renamed from
    --     @VectorHereBerlin@. @VectorHereBerlin@ has been deprecated, but will
    --     continue to work in applications that use it.
    --
    -- -   @VectorHereExplore@ – A default HERE map style containing a neutral,
    --     global map and its features including roads, buildings, landmarks,
    --     and water features. It also now includes a fully designed map of
    --     Japan.
    --
    -- -   @VectorHereExploreTruck@ – A global map containing truck
    --     restrictions and attributes (e.g. width \/ height \/ HAZMAT)
    --     symbolized with highlighted segments and icons on top of HERE
    --     Explore to support use cases within transport and logistics.
    --
    -- -   @RasterHereExploreSatellite@ – A global map containing high
    --     resolution satellite imagery.
    --
    -- -   @HybridHereExploreSatellite@ – A global map displaying the road
    --     network, street names, and city labels over satellite imagery. This
    --     style will automatically retrieve both raster and vector tiles, and
    --     your charges will be based on total tiles retrieved.
    --
    --     Hybrid styles use both vector and raster tiles when rendering the
    --     map that you see. This means that more tiles are retrieved than when
    --     using either vector or raster tiles alone. Your charges will include
    --     all tiles retrieved.
    --
    -- Valid
    -- <https://docs.aws.amazon.com/location/latest/developerguide/grab.html GrabMaps map styles>:
    --
    -- -   @VectorGrabStandardLight@ – The Grab Standard Light map style
    --     provides a basemap with detailed land use coloring, area names,
    --     roads, landmarks, and points of interest covering Southeast Asia.
    --
    -- -   @VectorGrabStandardDark@ – The Grab Standard Dark map style provides
    --     a dark variation of the standard basemap covering Southeast Asia.
    --
    -- Grab provides maps only for countries in Southeast Asia, and is only
    -- available in the Asia Pacific (Singapore) Region (@ap-southeast-1@). For
    -- more information, see
    -- <https://docs.aws.amazon.com/location/latest/developerguide/grab.html#grab-coverage-area GrabMaps countries and area covered>.
    --
    -- Valid
    -- <https://docs.aws.amazon.com/location/latest/developerguide/open-data.html Open Data map styles>:
    --
    -- -   @VectorOpenDataStandardLight@ – The Open Data Standard Light map
    --     style provides a detailed basemap for the world suitable for website
    --     and mobile application use. The map includes highways major roads,
    --     minor roads, railways, water features, cities, parks, landmarks,
    --     building footprints, and administrative boundaries.
    --
    -- -   @VectorOpenDataStandardDark@ – Open Data Standard Dark is a
    --     dark-themed map style that provides a detailed basemap for the world
    --     suitable for website and mobile application use. The map includes
    --     highways major roads, minor roads, railways, water features, cities,
    --     parks, landmarks, building footprints, and administrative
    --     boundaries.
    --
    -- -   @VectorOpenDataVisualizationLight@ – The Open Data Visualization
    --     Light map style is a light-themed style with muted colors and fewer
    --     features that aids in understanding overlaid data.
    --
    -- -   @VectorOpenDataVisualizationDark@ – The Open Data Visualization Dark
    --     map style is a dark-themed style with muted colors and fewer
    --     features that aids in understanding overlaid data.
    style :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MapConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'politicalView', 'mapConfiguration_politicalView' - Specifies the political view for the style. Leave unset to not use a
-- political view, or, for styles that support specific political views,
-- you can choose a view, such as @IND@ for the Indian view.
--
-- Default is unset.
--
-- Not all map resources or styles support political view styles. See
-- <https://docs.aws.amazon.com/location/latest/developerguide/map-concepts.html#political-views Political views>
-- for more information.
--
-- 'style', 'mapConfiguration_style' - Specifies the map style selected from an available data provider.
--
-- Valid
-- <https://docs.aws.amazon.com/location/latest/developerguide/esri.html Esri map styles>:
--
-- -   @VectorEsriDarkGrayCanvas@ – The Esri Dark Gray Canvas map style. A
--     vector basemap with a dark gray, neutral background with minimal
--     colors, labels, and features that\'s designed to draw attention to
--     your thematic content.
--
-- -   @RasterEsriImagery@ – The Esri Imagery map style. A raster basemap
--     that provides one meter or better satellite and aerial imagery in
--     many parts of the world and lower resolution satellite imagery
--     worldwide.
--
-- -   @VectorEsriLightGrayCanvas@ – The Esri Light Gray Canvas map style,
--     which provides a detailed vector basemap with a light gray, neutral
--     background style with minimal colors, labels, and features that\'s
--     designed to draw attention to your thematic content.
--
-- -   @VectorEsriTopographic@ – The Esri Light map style, which provides a
--     detailed vector basemap with a classic Esri map style.
--
-- -   @VectorEsriStreets@ – The Esri Street Map style, which provides a
--     detailed vector basemap for the world symbolized with a classic Esri
--     street map style. The vector tile layer is similar in content and
--     style to the World Street Map raster map.
--
-- -   @VectorEsriNavigation@ – The Esri Navigation map style, which
--     provides a detailed basemap for the world symbolized with a custom
--     navigation map style that\'s designed for use during the day in
--     mobile devices.
--
-- Valid
-- <https://docs.aws.amazon.com/location/latest/developerguide/HERE.html HERE Technologies map styles>:
--
-- -   @VectorHereContrast@ – The HERE Contrast (Berlin) map style is a
--     high contrast detailed base map of the world that blends 3D and 2D
--     rendering.
--
--     The @VectorHereContrast@ style has been renamed from
--     @VectorHereBerlin@. @VectorHereBerlin@ has been deprecated, but will
--     continue to work in applications that use it.
--
-- -   @VectorHereExplore@ – A default HERE map style containing a neutral,
--     global map and its features including roads, buildings, landmarks,
--     and water features. It also now includes a fully designed map of
--     Japan.
--
-- -   @VectorHereExploreTruck@ – A global map containing truck
--     restrictions and attributes (e.g. width \/ height \/ HAZMAT)
--     symbolized with highlighted segments and icons on top of HERE
--     Explore to support use cases within transport and logistics.
--
-- -   @RasterHereExploreSatellite@ – A global map containing high
--     resolution satellite imagery.
--
-- -   @HybridHereExploreSatellite@ – A global map displaying the road
--     network, street names, and city labels over satellite imagery. This
--     style will automatically retrieve both raster and vector tiles, and
--     your charges will be based on total tiles retrieved.
--
--     Hybrid styles use both vector and raster tiles when rendering the
--     map that you see. This means that more tiles are retrieved than when
--     using either vector or raster tiles alone. Your charges will include
--     all tiles retrieved.
--
-- Valid
-- <https://docs.aws.amazon.com/location/latest/developerguide/grab.html GrabMaps map styles>:
--
-- -   @VectorGrabStandardLight@ – The Grab Standard Light map style
--     provides a basemap with detailed land use coloring, area names,
--     roads, landmarks, and points of interest covering Southeast Asia.
--
-- -   @VectorGrabStandardDark@ – The Grab Standard Dark map style provides
--     a dark variation of the standard basemap covering Southeast Asia.
--
-- Grab provides maps only for countries in Southeast Asia, and is only
-- available in the Asia Pacific (Singapore) Region (@ap-southeast-1@). For
-- more information, see
-- <https://docs.aws.amazon.com/location/latest/developerguide/grab.html#grab-coverage-area GrabMaps countries and area covered>.
--
-- Valid
-- <https://docs.aws.amazon.com/location/latest/developerguide/open-data.html Open Data map styles>:
--
-- -   @VectorOpenDataStandardLight@ – The Open Data Standard Light map
--     style provides a detailed basemap for the world suitable for website
--     and mobile application use. The map includes highways major roads,
--     minor roads, railways, water features, cities, parks, landmarks,
--     building footprints, and administrative boundaries.
--
-- -   @VectorOpenDataStandardDark@ – Open Data Standard Dark is a
--     dark-themed map style that provides a detailed basemap for the world
--     suitable for website and mobile application use. The map includes
--     highways major roads, minor roads, railways, water features, cities,
--     parks, landmarks, building footprints, and administrative
--     boundaries.
--
-- -   @VectorOpenDataVisualizationLight@ – The Open Data Visualization
--     Light map style is a light-themed style with muted colors and fewer
--     features that aids in understanding overlaid data.
--
-- -   @VectorOpenDataVisualizationDark@ – The Open Data Visualization Dark
--     map style is a dark-themed style with muted colors and fewer
--     features that aids in understanding overlaid data.
newMapConfiguration ::
  -- | 'style'
  Prelude.Text ->
  MapConfiguration
newMapConfiguration pStyle_ =
  MapConfiguration'
    { politicalView = Prelude.Nothing,
      style = pStyle_
    }

-- | Specifies the political view for the style. Leave unset to not use a
-- political view, or, for styles that support specific political views,
-- you can choose a view, such as @IND@ for the Indian view.
--
-- Default is unset.
--
-- Not all map resources or styles support political view styles. See
-- <https://docs.aws.amazon.com/location/latest/developerguide/map-concepts.html#political-views Political views>
-- for more information.
mapConfiguration_politicalView :: Lens.Lens' MapConfiguration (Prelude.Maybe Prelude.Text)
mapConfiguration_politicalView = Lens.lens (\MapConfiguration' {politicalView} -> politicalView) (\s@MapConfiguration' {} a -> s {politicalView = a} :: MapConfiguration)

-- | Specifies the map style selected from an available data provider.
--
-- Valid
-- <https://docs.aws.amazon.com/location/latest/developerguide/esri.html Esri map styles>:
--
-- -   @VectorEsriDarkGrayCanvas@ – The Esri Dark Gray Canvas map style. A
--     vector basemap with a dark gray, neutral background with minimal
--     colors, labels, and features that\'s designed to draw attention to
--     your thematic content.
--
-- -   @RasterEsriImagery@ – The Esri Imagery map style. A raster basemap
--     that provides one meter or better satellite and aerial imagery in
--     many parts of the world and lower resolution satellite imagery
--     worldwide.
--
-- -   @VectorEsriLightGrayCanvas@ – The Esri Light Gray Canvas map style,
--     which provides a detailed vector basemap with a light gray, neutral
--     background style with minimal colors, labels, and features that\'s
--     designed to draw attention to your thematic content.
--
-- -   @VectorEsriTopographic@ – The Esri Light map style, which provides a
--     detailed vector basemap with a classic Esri map style.
--
-- -   @VectorEsriStreets@ – The Esri Street Map style, which provides a
--     detailed vector basemap for the world symbolized with a classic Esri
--     street map style. The vector tile layer is similar in content and
--     style to the World Street Map raster map.
--
-- -   @VectorEsriNavigation@ – The Esri Navigation map style, which
--     provides a detailed basemap for the world symbolized with a custom
--     navigation map style that\'s designed for use during the day in
--     mobile devices.
--
-- Valid
-- <https://docs.aws.amazon.com/location/latest/developerguide/HERE.html HERE Technologies map styles>:
--
-- -   @VectorHereContrast@ – The HERE Contrast (Berlin) map style is a
--     high contrast detailed base map of the world that blends 3D and 2D
--     rendering.
--
--     The @VectorHereContrast@ style has been renamed from
--     @VectorHereBerlin@. @VectorHereBerlin@ has been deprecated, but will
--     continue to work in applications that use it.
--
-- -   @VectorHereExplore@ – A default HERE map style containing a neutral,
--     global map and its features including roads, buildings, landmarks,
--     and water features. It also now includes a fully designed map of
--     Japan.
--
-- -   @VectorHereExploreTruck@ – A global map containing truck
--     restrictions and attributes (e.g. width \/ height \/ HAZMAT)
--     symbolized with highlighted segments and icons on top of HERE
--     Explore to support use cases within transport and logistics.
--
-- -   @RasterHereExploreSatellite@ – A global map containing high
--     resolution satellite imagery.
--
-- -   @HybridHereExploreSatellite@ – A global map displaying the road
--     network, street names, and city labels over satellite imagery. This
--     style will automatically retrieve both raster and vector tiles, and
--     your charges will be based on total tiles retrieved.
--
--     Hybrid styles use both vector and raster tiles when rendering the
--     map that you see. This means that more tiles are retrieved than when
--     using either vector or raster tiles alone. Your charges will include
--     all tiles retrieved.
--
-- Valid
-- <https://docs.aws.amazon.com/location/latest/developerguide/grab.html GrabMaps map styles>:
--
-- -   @VectorGrabStandardLight@ – The Grab Standard Light map style
--     provides a basemap with detailed land use coloring, area names,
--     roads, landmarks, and points of interest covering Southeast Asia.
--
-- -   @VectorGrabStandardDark@ – The Grab Standard Dark map style provides
--     a dark variation of the standard basemap covering Southeast Asia.
--
-- Grab provides maps only for countries in Southeast Asia, and is only
-- available in the Asia Pacific (Singapore) Region (@ap-southeast-1@). For
-- more information, see
-- <https://docs.aws.amazon.com/location/latest/developerguide/grab.html#grab-coverage-area GrabMaps countries and area covered>.
--
-- Valid
-- <https://docs.aws.amazon.com/location/latest/developerguide/open-data.html Open Data map styles>:
--
-- -   @VectorOpenDataStandardLight@ – The Open Data Standard Light map
--     style provides a detailed basemap for the world suitable for website
--     and mobile application use. The map includes highways major roads,
--     minor roads, railways, water features, cities, parks, landmarks,
--     building footprints, and administrative boundaries.
--
-- -   @VectorOpenDataStandardDark@ – Open Data Standard Dark is a
--     dark-themed map style that provides a detailed basemap for the world
--     suitable for website and mobile application use. The map includes
--     highways major roads, minor roads, railways, water features, cities,
--     parks, landmarks, building footprints, and administrative
--     boundaries.
--
-- -   @VectorOpenDataVisualizationLight@ – The Open Data Visualization
--     Light map style is a light-themed style with muted colors and fewer
--     features that aids in understanding overlaid data.
--
-- -   @VectorOpenDataVisualizationDark@ – The Open Data Visualization Dark
--     map style is a dark-themed style with muted colors and fewer
--     features that aids in understanding overlaid data.
mapConfiguration_style :: Lens.Lens' MapConfiguration Prelude.Text
mapConfiguration_style = Lens.lens (\MapConfiguration' {style} -> style) (\s@MapConfiguration' {} a -> s {style = a} :: MapConfiguration)

instance Data.FromJSON MapConfiguration where
  parseJSON =
    Data.withObject
      "MapConfiguration"
      ( \x ->
          MapConfiguration'
            Prelude.<$> (x Data..:? "PoliticalView")
            Prelude.<*> (x Data..: "Style")
      )

instance Prelude.Hashable MapConfiguration where
  hashWithSalt _salt MapConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` politicalView
      `Prelude.hashWithSalt` style

instance Prelude.NFData MapConfiguration where
  rnf MapConfiguration' {..} =
    Prelude.rnf politicalView
      `Prelude.seq` Prelude.rnf style

instance Data.ToJSON MapConfiguration where
  toJSON MapConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PoliticalView" Data..=) Prelude.<$> politicalView,
            Prelude.Just ("Style" Data..= style)
          ]
      )
