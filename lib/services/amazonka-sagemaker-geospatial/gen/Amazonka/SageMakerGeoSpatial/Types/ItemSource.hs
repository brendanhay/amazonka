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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.ItemSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.ItemSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.AssetValue
import Amazonka.SageMakerGeoSpatial.Types.Geometry
import Amazonka.SageMakerGeoSpatial.Types.Properties

-- | The structure representing the items in the response for
-- SearchRasterDataCollection.
--
-- /See:/ 'newItemSource' smart constructor.
data ItemSource = ItemSource'
  { -- | This is a dictionary of Asset Objects data associated with the Item that
    -- can be downloaded or streamed, each with a unique key.
    assets :: Prelude.Maybe (Prelude.HashMap Prelude.Text AssetValue),
    -- | This field contains additional properties of the item.
    properties :: Prelude.Maybe Properties,
    -- | The searchable date and time of the item, in UTC.
    dateTime :: Data.POSIX,
    -- | The item Geometry in GeoJson format.
    geometry :: Geometry,
    -- | A unique Id for the source item.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ItemSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assets', 'itemSource_assets' - This is a dictionary of Asset Objects data associated with the Item that
-- can be downloaded or streamed, each with a unique key.
--
-- 'properties', 'itemSource_properties' - This field contains additional properties of the item.
--
-- 'dateTime', 'itemSource_dateTime' - The searchable date and time of the item, in UTC.
--
-- 'geometry', 'itemSource_geometry' - The item Geometry in GeoJson format.
--
-- 'id', 'itemSource_id' - A unique Id for the source item.
newItemSource ::
  -- | 'dateTime'
  Prelude.UTCTime ->
  -- | 'geometry'
  Geometry ->
  -- | 'id'
  Prelude.Text ->
  ItemSource
newItemSource pDateTime_ pGeometry_ pId_ =
  ItemSource'
    { assets = Prelude.Nothing,
      properties = Prelude.Nothing,
      dateTime = Data._Time Lens.# pDateTime_,
      geometry = pGeometry_,
      id = pId_
    }

-- | This is a dictionary of Asset Objects data associated with the Item that
-- can be downloaded or streamed, each with a unique key.
itemSource_assets :: Lens.Lens' ItemSource (Prelude.Maybe (Prelude.HashMap Prelude.Text AssetValue))
itemSource_assets = Lens.lens (\ItemSource' {assets} -> assets) (\s@ItemSource' {} a -> s {assets = a} :: ItemSource) Prelude.. Lens.mapping Lens.coerced

-- | This field contains additional properties of the item.
itemSource_properties :: Lens.Lens' ItemSource (Prelude.Maybe Properties)
itemSource_properties = Lens.lens (\ItemSource' {properties} -> properties) (\s@ItemSource' {} a -> s {properties = a} :: ItemSource)

-- | The searchable date and time of the item, in UTC.
itemSource_dateTime :: Lens.Lens' ItemSource Prelude.UTCTime
itemSource_dateTime = Lens.lens (\ItemSource' {dateTime} -> dateTime) (\s@ItemSource' {} a -> s {dateTime = a} :: ItemSource) Prelude.. Data._Time

-- | The item Geometry in GeoJson format.
itemSource_geometry :: Lens.Lens' ItemSource Geometry
itemSource_geometry = Lens.lens (\ItemSource' {geometry} -> geometry) (\s@ItemSource' {} a -> s {geometry = a} :: ItemSource)

-- | A unique Id for the source item.
itemSource_id :: Lens.Lens' ItemSource Prelude.Text
itemSource_id = Lens.lens (\ItemSource' {id} -> id) (\s@ItemSource' {} a -> s {id = a} :: ItemSource)

instance Data.FromJSON ItemSource where
  parseJSON =
    Data.withObject
      "ItemSource"
      ( \x ->
          ItemSource'
            Prelude.<$> (x Data..:? "Assets" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Properties")
            Prelude.<*> (x Data..: "DateTime")
            Prelude.<*> (x Data..: "Geometry")
            Prelude.<*> (x Data..: "Id")
      )

instance Prelude.Hashable ItemSource where
  hashWithSalt _salt ItemSource' {..} =
    _salt
      `Prelude.hashWithSalt` assets
      `Prelude.hashWithSalt` properties
      `Prelude.hashWithSalt` dateTime
      `Prelude.hashWithSalt` geometry
      `Prelude.hashWithSalt` id

instance Prelude.NFData ItemSource where
  rnf ItemSource' {..} =
    Prelude.rnf assets
      `Prelude.seq` Prelude.rnf properties
      `Prelude.seq` Prelude.rnf dateTime
      `Prelude.seq` Prelude.rnf geometry
      `Prelude.seq` Prelude.rnf id
