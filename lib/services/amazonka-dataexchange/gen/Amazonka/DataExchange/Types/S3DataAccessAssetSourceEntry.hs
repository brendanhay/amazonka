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
-- Module      : Amazonka.DataExchange.Types.S3DataAccessAssetSourceEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.S3DataAccessAssetSourceEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Source details for an Amazon S3 data access asset.
--
-- /See:/ 'newS3DataAccessAssetSourceEntry' smart constructor.
data S3DataAccessAssetSourceEntry = S3DataAccessAssetSourceEntry'
  { -- | Organizes Amazon S3 asset key prefixes stored in an Amazon S3 bucket.
    keyPrefixes :: Prelude.Maybe [Prelude.Text],
    -- | The keys used to create the Amazon S3 data access.
    keys :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon S3 bucket used for hosting shared data in the Amazon S3 data
    -- access.
    bucket :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3DataAccessAssetSourceEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPrefixes', 's3DataAccessAssetSourceEntry_keyPrefixes' - Organizes Amazon S3 asset key prefixes stored in an Amazon S3 bucket.
--
-- 'keys', 's3DataAccessAssetSourceEntry_keys' - The keys used to create the Amazon S3 data access.
--
-- 'bucket', 's3DataAccessAssetSourceEntry_bucket' - The Amazon S3 bucket used for hosting shared data in the Amazon S3 data
-- access.
newS3DataAccessAssetSourceEntry ::
  -- | 'bucket'
  Prelude.Text ->
  S3DataAccessAssetSourceEntry
newS3DataAccessAssetSourceEntry pBucket_ =
  S3DataAccessAssetSourceEntry'
    { keyPrefixes =
        Prelude.Nothing,
      keys = Prelude.Nothing,
      bucket = pBucket_
    }

-- | Organizes Amazon S3 asset key prefixes stored in an Amazon S3 bucket.
s3DataAccessAssetSourceEntry_keyPrefixes :: Lens.Lens' S3DataAccessAssetSourceEntry (Prelude.Maybe [Prelude.Text])
s3DataAccessAssetSourceEntry_keyPrefixes = Lens.lens (\S3DataAccessAssetSourceEntry' {keyPrefixes} -> keyPrefixes) (\s@S3DataAccessAssetSourceEntry' {} a -> s {keyPrefixes = a} :: S3DataAccessAssetSourceEntry) Prelude.. Lens.mapping Lens.coerced

-- | The keys used to create the Amazon S3 data access.
s3DataAccessAssetSourceEntry_keys :: Lens.Lens' S3DataAccessAssetSourceEntry (Prelude.Maybe [Prelude.Text])
s3DataAccessAssetSourceEntry_keys = Lens.lens (\S3DataAccessAssetSourceEntry' {keys} -> keys) (\s@S3DataAccessAssetSourceEntry' {} a -> s {keys = a} :: S3DataAccessAssetSourceEntry) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon S3 bucket used for hosting shared data in the Amazon S3 data
-- access.
s3DataAccessAssetSourceEntry_bucket :: Lens.Lens' S3DataAccessAssetSourceEntry Prelude.Text
s3DataAccessAssetSourceEntry_bucket = Lens.lens (\S3DataAccessAssetSourceEntry' {bucket} -> bucket) (\s@S3DataAccessAssetSourceEntry' {} a -> s {bucket = a} :: S3DataAccessAssetSourceEntry)

instance Data.FromJSON S3DataAccessAssetSourceEntry where
  parseJSON =
    Data.withObject
      "S3DataAccessAssetSourceEntry"
      ( \x ->
          S3DataAccessAssetSourceEntry'
            Prelude.<$> (x Data..:? "KeyPrefixes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Keys" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Bucket")
      )

instance
  Prelude.Hashable
    S3DataAccessAssetSourceEntry
  where
  hashWithSalt _salt S3DataAccessAssetSourceEntry' {..} =
    _salt
      `Prelude.hashWithSalt` keyPrefixes
      `Prelude.hashWithSalt` keys
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData S3DataAccessAssetSourceEntry where
  rnf S3DataAccessAssetSourceEntry' {..} =
    Prelude.rnf keyPrefixes `Prelude.seq`
      Prelude.rnf keys `Prelude.seq`
        Prelude.rnf bucket

instance Data.ToJSON S3DataAccessAssetSourceEntry where
  toJSON S3DataAccessAssetSourceEntry' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KeyPrefixes" Data..=) Prelude.<$> keyPrefixes,
            ("Keys" Data..=) Prelude.<$> keys,
            Prelude.Just ("Bucket" Data..= bucket)
          ]
      )
