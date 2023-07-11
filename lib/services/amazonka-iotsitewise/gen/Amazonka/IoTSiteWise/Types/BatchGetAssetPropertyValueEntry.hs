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
-- Module      : Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information for an asset property value entry that is
-- associated with the
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_BatchGetAssetPropertyValue.html BatchGetAssetPropertyValue>
-- API.
--
-- To identify an asset property, you must specify one of the following:
--
-- -   The @assetId@ and @propertyId@ of an asset property.
--
-- -   A @propertyAlias@, which is a data stream alias (for example,
--     @\/company\/windfarm\/3\/turbine\/7\/temperature@). To define an
--     asset property\'s alias, see
--     <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_UpdateAssetProperty.html UpdateAssetProperty>.
--
-- /See:/ 'newBatchGetAssetPropertyValueEntry' smart constructor.
data BatchGetAssetPropertyValueEntry = BatchGetAssetPropertyValueEntry'
  { -- | The ID of the asset in which the asset property was created.
    assetId :: Prelude.Maybe Prelude.Text,
    -- | The alias that identifies the property, such as an OPC-UA server data
    -- stream path (for example,
    -- @\/company\/windfarm\/3\/turbine\/7\/temperature@). For more
    -- information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/connect-data-streams.html Mapping industrial data streams to asset properties>
    -- in the /IoT SiteWise User Guide/.
    propertyAlias :: Prelude.Maybe Prelude.Text,
    -- | The ID of the asset property.
    propertyId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the entry.
    entryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetAssetPropertyValueEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assetId', 'batchGetAssetPropertyValueEntry_assetId' - The ID of the asset in which the asset property was created.
--
-- 'propertyAlias', 'batchGetAssetPropertyValueEntry_propertyAlias' - The alias that identifies the property, such as an OPC-UA server data
-- stream path (for example,
-- @\/company\/windfarm\/3\/turbine\/7\/temperature@). For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/connect-data-streams.html Mapping industrial data streams to asset properties>
-- in the /IoT SiteWise User Guide/.
--
-- 'propertyId', 'batchGetAssetPropertyValueEntry_propertyId' - The ID of the asset property.
--
-- 'entryId', 'batchGetAssetPropertyValueEntry_entryId' - The ID of the entry.
newBatchGetAssetPropertyValueEntry ::
  -- | 'entryId'
  Prelude.Text ->
  BatchGetAssetPropertyValueEntry
newBatchGetAssetPropertyValueEntry pEntryId_ =
  BatchGetAssetPropertyValueEntry'
    { assetId =
        Prelude.Nothing,
      propertyAlias = Prelude.Nothing,
      propertyId = Prelude.Nothing,
      entryId = pEntryId_
    }

-- | The ID of the asset in which the asset property was created.
batchGetAssetPropertyValueEntry_assetId :: Lens.Lens' BatchGetAssetPropertyValueEntry (Prelude.Maybe Prelude.Text)
batchGetAssetPropertyValueEntry_assetId = Lens.lens (\BatchGetAssetPropertyValueEntry' {assetId} -> assetId) (\s@BatchGetAssetPropertyValueEntry' {} a -> s {assetId = a} :: BatchGetAssetPropertyValueEntry)

-- | The alias that identifies the property, such as an OPC-UA server data
-- stream path (for example,
-- @\/company\/windfarm\/3\/turbine\/7\/temperature@). For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/connect-data-streams.html Mapping industrial data streams to asset properties>
-- in the /IoT SiteWise User Guide/.
batchGetAssetPropertyValueEntry_propertyAlias :: Lens.Lens' BatchGetAssetPropertyValueEntry (Prelude.Maybe Prelude.Text)
batchGetAssetPropertyValueEntry_propertyAlias = Lens.lens (\BatchGetAssetPropertyValueEntry' {propertyAlias} -> propertyAlias) (\s@BatchGetAssetPropertyValueEntry' {} a -> s {propertyAlias = a} :: BatchGetAssetPropertyValueEntry)

-- | The ID of the asset property.
batchGetAssetPropertyValueEntry_propertyId :: Lens.Lens' BatchGetAssetPropertyValueEntry (Prelude.Maybe Prelude.Text)
batchGetAssetPropertyValueEntry_propertyId = Lens.lens (\BatchGetAssetPropertyValueEntry' {propertyId} -> propertyId) (\s@BatchGetAssetPropertyValueEntry' {} a -> s {propertyId = a} :: BatchGetAssetPropertyValueEntry)

-- | The ID of the entry.
batchGetAssetPropertyValueEntry_entryId :: Lens.Lens' BatchGetAssetPropertyValueEntry Prelude.Text
batchGetAssetPropertyValueEntry_entryId = Lens.lens (\BatchGetAssetPropertyValueEntry' {entryId} -> entryId) (\s@BatchGetAssetPropertyValueEntry' {} a -> s {entryId = a} :: BatchGetAssetPropertyValueEntry)

instance
  Prelude.Hashable
    BatchGetAssetPropertyValueEntry
  where
  hashWithSalt
    _salt
    BatchGetAssetPropertyValueEntry' {..} =
      _salt
        `Prelude.hashWithSalt` assetId
        `Prelude.hashWithSalt` propertyAlias
        `Prelude.hashWithSalt` propertyId
        `Prelude.hashWithSalt` entryId

instance
  Prelude.NFData
    BatchGetAssetPropertyValueEntry
  where
  rnf BatchGetAssetPropertyValueEntry' {..} =
    Prelude.rnf assetId
      `Prelude.seq` Prelude.rnf propertyAlias
      `Prelude.seq` Prelude.rnf propertyId
      `Prelude.seq` Prelude.rnf entryId

instance Data.ToJSON BatchGetAssetPropertyValueEntry where
  toJSON BatchGetAssetPropertyValueEntry' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("assetId" Data..=) Prelude.<$> assetId,
            ("propertyAlias" Data..=) Prelude.<$> propertyAlias,
            ("propertyId" Data..=) Prelude.<$> propertyId,
            Prelude.Just ("entryId" Data..= entryId)
          ]
      )
