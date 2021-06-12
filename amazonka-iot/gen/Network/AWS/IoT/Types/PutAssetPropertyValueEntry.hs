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

-- | An asset property value entry containing the following information.
--
-- /See:/ 'newPutAssetPropertyValueEntry' smart constructor.
data PutAssetPropertyValueEntry = PutAssetPropertyValueEntry'
  { -- | Optional. A unique identifier for this entry that you can define to
    -- better track which message caused an error in case of failure. Accepts
    -- substitution templates. Defaults to a new UUID.
    entryId :: Core.Maybe Core.Text,
    -- | The name of the property alias associated with your asset property. You
    -- must specify either a @propertyAlias@ or both an @aliasId@ and a
    -- @propertyId@. Accepts substitution templates.
    propertyAlias :: Core.Maybe Core.Text,
    -- | The ID of the AWS IoT SiteWise asset. You must specify either a
    -- @propertyAlias@ or both an @aliasId@ and a @propertyId@. Accepts
    -- substitution templates.
    assetId :: Core.Maybe Core.Text,
    -- | The ID of the asset\'s property. You must specify either a
    -- @propertyAlias@ or both an @aliasId@ and a @propertyId@. Accepts
    -- substitution templates.
    propertyId :: Core.Maybe Core.Text,
    -- | A list of property values to insert that each contain timestamp,
    -- quality, and value (TQV) information.
    propertyValues :: Core.NonEmpty AssetPropertyValue
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'assetId', 'putAssetPropertyValueEntry_assetId' - The ID of the AWS IoT SiteWise asset. You must specify either a
-- @propertyAlias@ or both an @aliasId@ and a @propertyId@. Accepts
-- substitution templates.
--
-- 'propertyId', 'putAssetPropertyValueEntry_propertyId' - The ID of the asset\'s property. You must specify either a
-- @propertyAlias@ or both an @aliasId@ and a @propertyId@. Accepts
-- substitution templates.
--
-- 'propertyValues', 'putAssetPropertyValueEntry_propertyValues' - A list of property values to insert that each contain timestamp,
-- quality, and value (TQV) information.
newPutAssetPropertyValueEntry ::
  -- | 'propertyValues'
  Core.NonEmpty AssetPropertyValue ->
  PutAssetPropertyValueEntry
newPutAssetPropertyValueEntry pPropertyValues_ =
  PutAssetPropertyValueEntry'
    { entryId = Core.Nothing,
      propertyAlias = Core.Nothing,
      assetId = Core.Nothing,
      propertyId = Core.Nothing,
      propertyValues =
        Lens._Coerce Lens.# pPropertyValues_
    }

-- | Optional. A unique identifier for this entry that you can define to
-- better track which message caused an error in case of failure. Accepts
-- substitution templates. Defaults to a new UUID.
putAssetPropertyValueEntry_entryId :: Lens.Lens' PutAssetPropertyValueEntry (Core.Maybe Core.Text)
putAssetPropertyValueEntry_entryId = Lens.lens (\PutAssetPropertyValueEntry' {entryId} -> entryId) (\s@PutAssetPropertyValueEntry' {} a -> s {entryId = a} :: PutAssetPropertyValueEntry)

-- | The name of the property alias associated with your asset property. You
-- must specify either a @propertyAlias@ or both an @aliasId@ and a
-- @propertyId@. Accepts substitution templates.
putAssetPropertyValueEntry_propertyAlias :: Lens.Lens' PutAssetPropertyValueEntry (Core.Maybe Core.Text)
putAssetPropertyValueEntry_propertyAlias = Lens.lens (\PutAssetPropertyValueEntry' {propertyAlias} -> propertyAlias) (\s@PutAssetPropertyValueEntry' {} a -> s {propertyAlias = a} :: PutAssetPropertyValueEntry)

-- | The ID of the AWS IoT SiteWise asset. You must specify either a
-- @propertyAlias@ or both an @aliasId@ and a @propertyId@. Accepts
-- substitution templates.
putAssetPropertyValueEntry_assetId :: Lens.Lens' PutAssetPropertyValueEntry (Core.Maybe Core.Text)
putAssetPropertyValueEntry_assetId = Lens.lens (\PutAssetPropertyValueEntry' {assetId} -> assetId) (\s@PutAssetPropertyValueEntry' {} a -> s {assetId = a} :: PutAssetPropertyValueEntry)

-- | The ID of the asset\'s property. You must specify either a
-- @propertyAlias@ or both an @aliasId@ and a @propertyId@. Accepts
-- substitution templates.
putAssetPropertyValueEntry_propertyId :: Lens.Lens' PutAssetPropertyValueEntry (Core.Maybe Core.Text)
putAssetPropertyValueEntry_propertyId = Lens.lens (\PutAssetPropertyValueEntry' {propertyId} -> propertyId) (\s@PutAssetPropertyValueEntry' {} a -> s {propertyId = a} :: PutAssetPropertyValueEntry)

-- | A list of property values to insert that each contain timestamp,
-- quality, and value (TQV) information.
putAssetPropertyValueEntry_propertyValues :: Lens.Lens' PutAssetPropertyValueEntry (Core.NonEmpty AssetPropertyValue)
putAssetPropertyValueEntry_propertyValues = Lens.lens (\PutAssetPropertyValueEntry' {propertyValues} -> propertyValues) (\s@PutAssetPropertyValueEntry' {} a -> s {propertyValues = a} :: PutAssetPropertyValueEntry) Core.. Lens._Coerce

instance Core.FromJSON PutAssetPropertyValueEntry where
  parseJSON =
    Core.withObject
      "PutAssetPropertyValueEntry"
      ( \x ->
          PutAssetPropertyValueEntry'
            Core.<$> (x Core..:? "entryId")
            Core.<*> (x Core..:? "propertyAlias")
            Core.<*> (x Core..:? "assetId")
            Core.<*> (x Core..:? "propertyId")
            Core.<*> (x Core..: "propertyValues")
      )

instance Core.Hashable PutAssetPropertyValueEntry

instance Core.NFData PutAssetPropertyValueEntry

instance Core.ToJSON PutAssetPropertyValueEntry where
  toJSON PutAssetPropertyValueEntry' {..} =
    Core.object
      ( Core.catMaybes
          [ ("entryId" Core..=) Core.<$> entryId,
            ("propertyAlias" Core..=) Core.<$> propertyAlias,
            ("assetId" Core..=) Core.<$> assetId,
            ("propertyId" Core..=) Core.<$> propertyId,
            Core.Just ("propertyValues" Core..= propertyValues)
          ]
      )
