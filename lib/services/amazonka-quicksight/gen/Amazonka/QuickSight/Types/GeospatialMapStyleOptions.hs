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
-- Module      : Amazonka.QuickSight.Types.GeospatialMapStyleOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GeospatialMapStyleOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.BaseMapStyleType

-- | The map style options of the geospatial map.
--
-- /See:/ 'newGeospatialMapStyleOptions' smart constructor.
data GeospatialMapStyleOptions = GeospatialMapStyleOptions'
  { -- | The base map style of the geospatial map.
    baseMapStyle :: Prelude.Maybe BaseMapStyleType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GeospatialMapStyleOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baseMapStyle', 'geospatialMapStyleOptions_baseMapStyle' - The base map style of the geospatial map.
newGeospatialMapStyleOptions ::
  GeospatialMapStyleOptions
newGeospatialMapStyleOptions =
  GeospatialMapStyleOptions'
    { baseMapStyle =
        Prelude.Nothing
    }

-- | The base map style of the geospatial map.
geospatialMapStyleOptions_baseMapStyle :: Lens.Lens' GeospatialMapStyleOptions (Prelude.Maybe BaseMapStyleType)
geospatialMapStyleOptions_baseMapStyle = Lens.lens (\GeospatialMapStyleOptions' {baseMapStyle} -> baseMapStyle) (\s@GeospatialMapStyleOptions' {} a -> s {baseMapStyle = a} :: GeospatialMapStyleOptions)

instance Data.FromJSON GeospatialMapStyleOptions where
  parseJSON =
    Data.withObject
      "GeospatialMapStyleOptions"
      ( \x ->
          GeospatialMapStyleOptions'
            Prelude.<$> (x Data..:? "BaseMapStyle")
      )

instance Prelude.Hashable GeospatialMapStyleOptions where
  hashWithSalt _salt GeospatialMapStyleOptions' {..} =
    _salt `Prelude.hashWithSalt` baseMapStyle

instance Prelude.NFData GeospatialMapStyleOptions where
  rnf GeospatialMapStyleOptions' {..} =
    Prelude.rnf baseMapStyle

instance Data.ToJSON GeospatialMapStyleOptions where
  toJSON GeospatialMapStyleOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [("BaseMapStyle" Data..=) Prelude.<$> baseMapStyle]
      )
