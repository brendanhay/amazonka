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
-- Module      : Amazonka.DataExchange.Types.AssetSourceEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.AssetSourceEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The source of the assets.
--
-- /See:/ 'newAssetSourceEntry' smart constructor.
data AssetSourceEntry = AssetSourceEntry'
  { -- | The Amazon S3 bucket that\'s part of the source of the asset.
    bucket :: Prelude.Text,
    -- | The name of the object in Amazon S3 for the asset.
    key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetSourceEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucket', 'assetSourceEntry_bucket' - The Amazon S3 bucket that\'s part of the source of the asset.
--
-- 'key', 'assetSourceEntry_key' - The name of the object in Amazon S3 for the asset.
newAssetSourceEntry ::
  -- | 'bucket'
  Prelude.Text ->
  -- | 'key'
  Prelude.Text ->
  AssetSourceEntry
newAssetSourceEntry pBucket_ pKey_ =
  AssetSourceEntry' {bucket = pBucket_, key = pKey_}

-- | The Amazon S3 bucket that\'s part of the source of the asset.
assetSourceEntry_bucket :: Lens.Lens' AssetSourceEntry Prelude.Text
assetSourceEntry_bucket = Lens.lens (\AssetSourceEntry' {bucket} -> bucket) (\s@AssetSourceEntry' {} a -> s {bucket = a} :: AssetSourceEntry)

-- | The name of the object in Amazon S3 for the asset.
assetSourceEntry_key :: Lens.Lens' AssetSourceEntry Prelude.Text
assetSourceEntry_key = Lens.lens (\AssetSourceEntry' {key} -> key) (\s@AssetSourceEntry' {} a -> s {key = a} :: AssetSourceEntry)

instance Data.FromJSON AssetSourceEntry where
  parseJSON =
    Data.withObject
      "AssetSourceEntry"
      ( \x ->
          AssetSourceEntry'
            Prelude.<$> (x Data..: "Bucket") Prelude.<*> (x Data..: "Key")
      )

instance Prelude.Hashable AssetSourceEntry where
  hashWithSalt _salt AssetSourceEntry' {..} =
    _salt `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` key

instance Prelude.NFData AssetSourceEntry where
  rnf AssetSourceEntry' {..} =
    Prelude.rnf bucket `Prelude.seq` Prelude.rnf key

instance Data.ToJSON AssetSourceEntry where
  toJSON AssetSourceEntry' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Bucket" Data..= bucket),
            Prelude.Just ("Key" Data..= key)
          ]
      )
