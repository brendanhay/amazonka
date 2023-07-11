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
-- Module      : Amazonka.IoT.Types.PutAssetPropertyValueEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.PutAssetPropertyValueEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.AssetPropertyValue
import qualified Amazonka.Prelude as Prelude

-- | An asset property value entry containing the following information.
--
-- /See:/ 'newPutAssetPropertyValueEntry' smart constructor.
data PutAssetPropertyValueEntry = PutAssetPropertyValueEntry'
  { -- | The ID of the IoT SiteWise asset. You must specify either a
    -- @propertyAlias@ or both an @aliasId@ and a @propertyId@. Accepts
    -- substitution templates.
    assetId :: Prelude.Maybe Prelude.Text,
    -- | Optional. A unique identifier for this entry that you can define to
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
-- 'assetId', 'putAssetPropertyValueEntry_assetId' - The ID of the IoT SiteWise asset. You must specify either a
-- @propertyAlias@ or both an @aliasId@ and a @propertyId@. Accepts
-- substitution templates.
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
-- 'propertyValues', 'putAssetPropertyValueEntry_propertyValues' - A list of property values to insert that each contain timestamp,
-- quality, and value (TQV) information.
newPutAssetPropertyValueEntry ::
  -- | 'propertyValues'
  Prelude.NonEmpty AssetPropertyValue ->
  PutAssetPropertyValueEntry
newPutAssetPropertyValueEntry pPropertyValues_ =
  PutAssetPropertyValueEntry'
    { assetId =
        Prelude.Nothing,
      entryId = Prelude.Nothing,
      propertyAlias = Prelude.Nothing,
      propertyId = Prelude.Nothing,
      propertyValues =
        Lens.coerced Lens.# pPropertyValues_
    }

-- | The ID of the IoT SiteWise asset. You must specify either a
-- @propertyAlias@ or both an @aliasId@ and a @propertyId@. Accepts
-- substitution templates.
putAssetPropertyValueEntry_assetId :: Lens.Lens' PutAssetPropertyValueEntry (Prelude.Maybe Prelude.Text)
putAssetPropertyValueEntry_assetId = Lens.lens (\PutAssetPropertyValueEntry' {assetId} -> assetId) (\s@PutAssetPropertyValueEntry' {} a -> s {assetId = a} :: PutAssetPropertyValueEntry)

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

-- | A list of property values to insert that each contain timestamp,
-- quality, and value (TQV) information.
putAssetPropertyValueEntry_propertyValues :: Lens.Lens' PutAssetPropertyValueEntry (Prelude.NonEmpty AssetPropertyValue)
putAssetPropertyValueEntry_propertyValues = Lens.lens (\PutAssetPropertyValueEntry' {propertyValues} -> propertyValues) (\s@PutAssetPropertyValueEntry' {} a -> s {propertyValues = a} :: PutAssetPropertyValueEntry) Prelude.. Lens.coerced

instance Data.FromJSON PutAssetPropertyValueEntry where
  parseJSON =
    Data.withObject
      "PutAssetPropertyValueEntry"
      ( \x ->
          PutAssetPropertyValueEntry'
            Prelude.<$> (x Data..:? "assetId")
            Prelude.<*> (x Data..:? "entryId")
            Prelude.<*> (x Data..:? "propertyAlias")
            Prelude.<*> (x Data..:? "propertyId")
            Prelude.<*> (x Data..: "propertyValues")
      )

instance Prelude.Hashable PutAssetPropertyValueEntry where
  hashWithSalt _salt PutAssetPropertyValueEntry' {..} =
    _salt
      `Prelude.hashWithSalt` assetId
      `Prelude.hashWithSalt` entryId
      `Prelude.hashWithSalt` propertyAlias
      `Prelude.hashWithSalt` propertyId
      `Prelude.hashWithSalt` propertyValues

instance Prelude.NFData PutAssetPropertyValueEntry where
  rnf PutAssetPropertyValueEntry' {..} =
    Prelude.rnf assetId
      `Prelude.seq` Prelude.rnf entryId
      `Prelude.seq` Prelude.rnf propertyAlias
      `Prelude.seq` Prelude.rnf propertyId
      `Prelude.seq` Prelude.rnf propertyValues

instance Data.ToJSON PutAssetPropertyValueEntry where
  toJSON PutAssetPropertyValueEntry' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("assetId" Data..=) Prelude.<$> assetId,
            ("entryId" Data..=) Prelude.<$> entryId,
            ("propertyAlias" Data..=) Prelude.<$> propertyAlias,
            ("propertyId" Data..=) Prelude.<$> propertyId,
            Prelude.Just
              ("propertyValues" Data..= propertyValues)
          ]
      )
