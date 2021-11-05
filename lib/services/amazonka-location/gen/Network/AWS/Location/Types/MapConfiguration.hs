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
-- Module      : Network.AWS.Location.Types.MapConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Location.Types.MapConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the map tile style selected from an available provider.
--
-- /See:/ 'newMapConfiguration' smart constructor.
data MapConfiguration = MapConfiguration'
  { -- | Specifies the map style selected from an available data provider.
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
    -- -   @VectorEsriStreets@ – The Esri World Streets map style, which
    --     provides a detailed vector basemap for the world symbolized with a
    --     classic Esri street map style. The vector tile layer is similar in
    --     content and style to the World Street Map raster map.
    --
    -- -   @VectorEsriNavigation@ – The Esri World Navigation map style, which
    --     provides a detailed basemap for the world symbolized with a custom
    --     navigation map style that\'s designed for use during the day in
    --     mobile devices.
    --
    -- Valid
    -- <https://docs.aws.amazon.com/location/latest/developerguide/HERE.html HERE Technologies map styles>:
    --
    -- -   @VectorHereBerlin@ – The HERE Berlin map style is a high contrast
    --     detailed base map of the world that blends 3D and 2D rendering.
    --
    --     When using HERE as your data provider, and selecting the Style
    --     @VectorHereBerlin@, you may not use HERE Technologies maps for Asset
    --     Management. See the
    --     <https://aws.amazon.com/service-terms/ AWS Service Terms> for Amazon
    --     Location Service.
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
-- -   @VectorEsriStreets@ – The Esri World Streets map style, which
--     provides a detailed vector basemap for the world symbolized with a
--     classic Esri street map style. The vector tile layer is similar in
--     content and style to the World Street Map raster map.
--
-- -   @VectorEsriNavigation@ – The Esri World Navigation map style, which
--     provides a detailed basemap for the world symbolized with a custom
--     navigation map style that\'s designed for use during the day in
--     mobile devices.
--
-- Valid
-- <https://docs.aws.amazon.com/location/latest/developerguide/HERE.html HERE Technologies map styles>:
--
-- -   @VectorHereBerlin@ – The HERE Berlin map style is a high contrast
--     detailed base map of the world that blends 3D and 2D rendering.
--
--     When using HERE as your data provider, and selecting the Style
--     @VectorHereBerlin@, you may not use HERE Technologies maps for Asset
--     Management. See the
--     <https://aws.amazon.com/service-terms/ AWS Service Terms> for Amazon
--     Location Service.
newMapConfiguration ::
  -- | 'style'
  Prelude.Text ->
  MapConfiguration
newMapConfiguration pStyle_ =
  MapConfiguration' {style = pStyle_}

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
-- -   @VectorEsriStreets@ – The Esri World Streets map style, which
--     provides a detailed vector basemap for the world symbolized with a
--     classic Esri street map style. The vector tile layer is similar in
--     content and style to the World Street Map raster map.
--
-- -   @VectorEsriNavigation@ – The Esri World Navigation map style, which
--     provides a detailed basemap for the world symbolized with a custom
--     navigation map style that\'s designed for use during the day in
--     mobile devices.
--
-- Valid
-- <https://docs.aws.amazon.com/location/latest/developerguide/HERE.html HERE Technologies map styles>:
--
-- -   @VectorHereBerlin@ – The HERE Berlin map style is a high contrast
--     detailed base map of the world that blends 3D and 2D rendering.
--
--     When using HERE as your data provider, and selecting the Style
--     @VectorHereBerlin@, you may not use HERE Technologies maps for Asset
--     Management. See the
--     <https://aws.amazon.com/service-terms/ AWS Service Terms> for Amazon
--     Location Service.
mapConfiguration_style :: Lens.Lens' MapConfiguration Prelude.Text
mapConfiguration_style = Lens.lens (\MapConfiguration' {style} -> style) (\s@MapConfiguration' {} a -> s {style = a} :: MapConfiguration)

instance Core.FromJSON MapConfiguration where
  parseJSON =
    Core.withObject
      "MapConfiguration"
      ( \x ->
          MapConfiguration' Prelude.<$> (x Core..: "Style")
      )

instance Prelude.Hashable MapConfiguration

instance Prelude.NFData MapConfiguration

instance Core.ToJSON MapConfiguration where
  toJSON MapConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Style" Core..= style)]
      )
