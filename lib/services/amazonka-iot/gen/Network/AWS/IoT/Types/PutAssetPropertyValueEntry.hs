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
-- Module      : Network.AWS.IoT.Types.PutAssetPropertyValueEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.PutAssetPropertyValueEntry where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.AssetPropertyValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An asset property value entry containing the following information.
--
-- /See:/ 'newPutAssetPropertyValueEntry' smart constructor.
data PutAssetPropertyValueEntry = PutAssetPropertyValueEntry'
  { -- | Optional. A unique identifier for this entry that you can define to
    -- better track which message caused an error in case of failure. Accepts
    -- substitution templates. Defaults to a new UUID.
    entryId :: Prelude.Maybe Prelude.Text,
    -- | The name of the property alias associated with your asset property. You
    -- must specify either a @propertyAlias@ or both an @aliasId@ and a
    -- @propertyId@. Accepts substitution templates.
    propertyAlias :: Prelude.Maybe Prelude.Text,
    -- | The ID of the asset\'s property. You must specify either a
    -- @propertyAlias@ or both an @aliasId@ and a @propertyId@. Accepts
    -- substitution templates.
    propertyId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the IoT SiteWise asset. You must specify either a
    -- @propertyAlias@ or both an @aliasId@ and a @propertyId@. Accepts
    -- substitution templates.
    assetId :: Prelude.Maybe Prelude.Text,
    -- | A list of property values to insert that each contain timestamp,
    -- quality, and value (TQV) information.
    propertyValues :: Prelude.NonEmpty AssetPropertyValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAssetPropertyValueEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entryId', 'putAssetPropertyValueEntry_entryId' - Optional. A unique identifier for this entry that you can define to
-- better track which message caused an error in case of failure. Accepts
-- substitution templates. Defaults to a new UUID.
--
-- 'propertyAlias', 'putAssetPropertyValueEntry_propertyAlias' - The name of the property alias associated with your asset property. You
-- must specify either a @propertyAlias@ or both an @aliasId@ and a
-- @propertyId@. Accepts substitution templates.
--
-- 'propertyId', 'putAssetPropertyValueEntry_propertyId' - The ID of the asset\'s property. You must specify either a
-- @propertyAlias@ or both an @aliasId@ and a @propertyId@. Accepts
-- substitution templates.
--
-- 'assetId', 'putAssetPropertyValueEntry_assetId' - The ID of the IoT SiteWise asset. You must specify either a
-- @propertyAlias@ or both an @aliasId@ and a @propertyId@. Accepts
-- substitution templates.
--
-- 'propertyValues', 'putAssetPropertyValueEntry_propertyValues' - A list of property values to insert that each contain timestamp,
-- quality, and value (TQV) information.
newPutAssetPropertyValueEntry ::
  -- | 'propertyValues'
  Prelude.NonEmpty AssetPropertyValue ->
  PutAssetPropertyValueEntry
newPutAssetPropertyValueEntry pPropertyValues_ =
  PutAssetPropertyValueEntry'
    { entryId =
        Prelude.Nothing,
      propertyAlias = Prelude.Nothing,
      propertyId = Prelude.Nothing,
      assetId = Prelude.Nothing,
      propertyValues =
        Lens.coerced Lens.# pPropertyValues_
    }

-- | Optional. A unique identifier for this entry that you can define to
-- better track which message caused an error in case of failure. Accepts
-- substitution templates. Defaults to a new UUID.
putAssetPropertyValueEntry_entryId :: Lens.Lens' PutAssetPropertyValueEntry (Prelude.Maybe Prelude.Text)
putAssetPropertyValueEntry_entryId = Lens.lens (\PutAssetPropertyValueEntry' {entryId} -> entryId) (\s@PutAssetPropertyValueEntry' {} a -> s {entryId = a} :: PutAssetPropertyValueEntry)

-- | The name of the property alias associated with your asset property. You
-- must specify either a @propertyAlias@ or both an @aliasId@ and a
-- @propertyId@. Accepts substitution templates.
putAssetPropertyValueEntry_propertyAlias :: Lens.Lens' PutAssetPropertyValueEntry (Prelude.Maybe Prelude.Text)
putAssetPropertyValueEntry_propertyAlias = Lens.lens (\PutAssetPropertyValueEntry' {propertyAlias} -> propertyAlias) (\s@PutAssetPropertyValueEntry' {} a -> s {propertyAlias = a} :: PutAssetPropertyValueEntry)

-- | The ID of the asset\'s property. You must specify either a
-- @propertyAlias@ or both an @aliasId@ and a @propertyId@. Accepts
-- substitution templates.
putAssetPropertyValueEntry_propertyId :: Lens.Lens' PutAssetPropertyValueEntry (Prelude.Maybe Prelude.Text)
putAssetPropertyValueEntry_propertyId = Lens.lens (\PutAssetPropertyValueEntry' {propertyId} -> propertyId) (\s@PutAssetPropertyValueEntry' {} a -> s {propertyId = a} :: PutAssetPropertyValueEntry)

-- | The ID of the IoT SiteWise asset. You must specify either a
-- @propertyAlias@ or both an @aliasId@ and a @propertyId@. Accepts
-- substitution templates.
putAssetPropertyValueEntry_assetId :: Lens.Lens' PutAssetPropertyValueEntry (Prelude.Maybe Prelude.Text)
putAssetPropertyValueEntry_assetId = Lens.lens (\PutAssetPropertyValueEntry' {assetId} -> assetId) (\s@PutAssetPropertyValueEntry' {} a -> s {assetId = a} :: PutAssetPropertyValueEntry)

-- | A list of property values to insert that each contain timestamp,
-- quality, and value (TQV) information.
putAssetPropertyValueEntry_propertyValues :: Lens.Lens' PutAssetPropertyValueEntry (Prelude.NonEmpty AssetPropertyValue)
putAssetPropertyValueEntry_propertyValues = Lens.lens (\PutAssetPropertyValueEntry' {propertyValues} -> propertyValues) (\s@PutAssetPropertyValueEntry' {} a -> s {propertyValues = a} :: PutAssetPropertyValueEntry) Prelude.. Lens.coerced

instance Core.FromJSON PutAssetPropertyValueEntry where
  parseJSON =
    Core.withObject
      "PutAssetPropertyValueEntry"
      ( \x ->
          PutAssetPropertyValueEntry'
            Prelude.<$> (x Core..:? "entryId")
            Prelude.<*> (x Core..:? "propertyAlias")
            Prelude.<*> (x Core..:? "propertyId")
            Prelude.<*> (x Core..:? "assetId")
            Prelude.<*> (x Core..: "propertyValues")
      )

instance Prelude.Hashable PutAssetPropertyValueEntry

instance Prelude.NFData PutAssetPropertyValueEntry

instance Core.ToJSON PutAssetPropertyValueEntry where
  toJSON PutAssetPropertyValueEntry' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("entryId" Core..=) Prelude.<$> entryId,
            ("propertyAlias" Core..=) Prelude.<$> propertyAlias,
            ("propertyId" Core..=) Prelude.<$> propertyId,
            ("assetId" Core..=) Prelude.<$> assetId,
            Prelude.Just
              ("propertyValues" Core..= propertyValues)
          ]
      )
