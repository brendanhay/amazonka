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
-- Module      : Amazonka.DataExchange.Types.AssetDestinationEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.AssetDestinationEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The destination for the asset.
--
-- /See:/ 'newAssetDestinationEntry' smart constructor.
data AssetDestinationEntry = AssetDestinationEntry'
  { -- | The name of the object in Amazon S3 for the asset.
    key :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the asset.
    assetId :: Prelude.Text,
    -- | The S3 bucket that is the destination for the asset.
    bucket :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetDestinationEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'assetDestinationEntry_key' - The name of the object in Amazon S3 for the asset.
--
-- 'assetId', 'assetDestinationEntry_assetId' - The unique identifier for the asset.
--
-- 'bucket', 'assetDestinationEntry_bucket' - The S3 bucket that is the destination for the asset.
newAssetDestinationEntry ::
  -- | 'assetId'
  Prelude.Text ->
  -- | 'bucket'
  Prelude.Text ->
  AssetDestinationEntry
newAssetDestinationEntry pAssetId_ pBucket_ =
  AssetDestinationEntry'
    { key = Prelude.Nothing,
      assetId = pAssetId_,
      bucket = pBucket_
    }

-- | The name of the object in Amazon S3 for the asset.
assetDestinationEntry_key :: Lens.Lens' AssetDestinationEntry (Prelude.Maybe Prelude.Text)
assetDestinationEntry_key = Lens.lens (\AssetDestinationEntry' {key} -> key) (\s@AssetDestinationEntry' {} a -> s {key = a} :: AssetDestinationEntry)

-- | The unique identifier for the asset.
assetDestinationEntry_assetId :: Lens.Lens' AssetDestinationEntry Prelude.Text
assetDestinationEntry_assetId = Lens.lens (\AssetDestinationEntry' {assetId} -> assetId) (\s@AssetDestinationEntry' {} a -> s {assetId = a} :: AssetDestinationEntry)

-- | The S3 bucket that is the destination for the asset.
assetDestinationEntry_bucket :: Lens.Lens' AssetDestinationEntry Prelude.Text
assetDestinationEntry_bucket = Lens.lens (\AssetDestinationEntry' {bucket} -> bucket) (\s@AssetDestinationEntry' {} a -> s {bucket = a} :: AssetDestinationEntry)

instance Data.FromJSON AssetDestinationEntry where
  parseJSON =
    Data.withObject
      "AssetDestinationEntry"
      ( \x ->
          AssetDestinationEntry'
            Prelude.<$> (x Data..:? "Key")
            Prelude.<*> (x Data..: "AssetId")
            Prelude.<*> (x Data..: "Bucket")
      )

instance Prelude.Hashable AssetDestinationEntry where
  hashWithSalt _salt AssetDestinationEntry' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` assetId
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData AssetDestinationEntry where
  rnf AssetDestinationEntry' {..} =
    Prelude.rnf key
      `Prelude.seq` Prelude.rnf assetId
      `Prelude.seq` Prelude.rnf bucket

instance Data.ToJSON AssetDestinationEntry where
  toJSON AssetDestinationEntry' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Key" Data..=) Prelude.<$> key,
            Prelude.Just ("AssetId" Data..= assetId),
            Prelude.Just ("Bucket" Data..= bucket)
          ]
      )
