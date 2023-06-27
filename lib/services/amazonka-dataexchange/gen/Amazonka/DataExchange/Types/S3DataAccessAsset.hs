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
-- Module      : Amazonka.DataExchange.Types.S3DataAccessAsset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.S3DataAccessAsset where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types.KmsKeyToGrant
import qualified Amazonka.Prelude as Prelude

-- | The Amazon S3 data access that is the asset.
--
-- /See:/ 'newS3DataAccessAsset' smart constructor.
data S3DataAccessAsset = S3DataAccessAsset'
  { -- | The Amazon S3 bucket used for hosting shared data in the Amazon S3 data
    -- access.
    keyPrefixes :: Prelude.Maybe [Prelude.Text],
    -- | S3 keys made available using this asset.
    keys :: Prelude.Maybe [Prelude.Text],
    -- | List of AWS KMS CMKs (Key Management System Customer Managed Keys) and
    -- ARNs used to encrypt S3 objects being shared in this S3 Data Access
    -- asset. Providers must include all AWS KMS keys used to encrypt these
    -- shared S3 objects.
    kmsKeysToGrant :: Prelude.Maybe (Prelude.NonEmpty KmsKeyToGrant),
    -- | The automatically-generated bucket-style alias for your Amazon S3 Access
    -- Point. Customers can access their entitled data using the S3 Access
    -- Point alias.
    s3AccessPointAlias :: Prelude.Maybe Prelude.Text,
    -- | The ARN for your Amazon S3 Access Point. Customers can also access their
    -- entitled data using the S3 Access Point ARN.
    s3AccessPointArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 bucket hosting data to be shared in the S3 data access.
    bucket :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3DataAccessAsset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPrefixes', 's3DataAccessAsset_keyPrefixes' - The Amazon S3 bucket used for hosting shared data in the Amazon S3 data
-- access.
--
-- 'keys', 's3DataAccessAsset_keys' - S3 keys made available using this asset.
--
-- 'kmsKeysToGrant', 's3DataAccessAsset_kmsKeysToGrant' - List of AWS KMS CMKs (Key Management System Customer Managed Keys) and
-- ARNs used to encrypt S3 objects being shared in this S3 Data Access
-- asset. Providers must include all AWS KMS keys used to encrypt these
-- shared S3 objects.
--
-- 's3AccessPointAlias', 's3DataAccessAsset_s3AccessPointAlias' - The automatically-generated bucket-style alias for your Amazon S3 Access
-- Point. Customers can access their entitled data using the S3 Access
-- Point alias.
--
-- 's3AccessPointArn', 's3DataAccessAsset_s3AccessPointArn' - The ARN for your Amazon S3 Access Point. Customers can also access their
-- entitled data using the S3 Access Point ARN.
--
-- 'bucket', 's3DataAccessAsset_bucket' - The Amazon S3 bucket hosting data to be shared in the S3 data access.
newS3DataAccessAsset ::
  -- | 'bucket'
  Prelude.Text ->
  S3DataAccessAsset
newS3DataAccessAsset pBucket_ =
  S3DataAccessAsset'
    { keyPrefixes = Prelude.Nothing,
      keys = Prelude.Nothing,
      kmsKeysToGrant = Prelude.Nothing,
      s3AccessPointAlias = Prelude.Nothing,
      s3AccessPointArn = Prelude.Nothing,
      bucket = pBucket_
    }

-- | The Amazon S3 bucket used for hosting shared data in the Amazon S3 data
-- access.
s3DataAccessAsset_keyPrefixes :: Lens.Lens' S3DataAccessAsset (Prelude.Maybe [Prelude.Text])
s3DataAccessAsset_keyPrefixes = Lens.lens (\S3DataAccessAsset' {keyPrefixes} -> keyPrefixes) (\s@S3DataAccessAsset' {} a -> s {keyPrefixes = a} :: S3DataAccessAsset) Prelude.. Lens.mapping Lens.coerced

-- | S3 keys made available using this asset.
s3DataAccessAsset_keys :: Lens.Lens' S3DataAccessAsset (Prelude.Maybe [Prelude.Text])
s3DataAccessAsset_keys = Lens.lens (\S3DataAccessAsset' {keys} -> keys) (\s@S3DataAccessAsset' {} a -> s {keys = a} :: S3DataAccessAsset) Prelude.. Lens.mapping Lens.coerced

-- | List of AWS KMS CMKs (Key Management System Customer Managed Keys) and
-- ARNs used to encrypt S3 objects being shared in this S3 Data Access
-- asset. Providers must include all AWS KMS keys used to encrypt these
-- shared S3 objects.
s3DataAccessAsset_kmsKeysToGrant :: Lens.Lens' S3DataAccessAsset (Prelude.Maybe (Prelude.NonEmpty KmsKeyToGrant))
s3DataAccessAsset_kmsKeysToGrant = Lens.lens (\S3DataAccessAsset' {kmsKeysToGrant} -> kmsKeysToGrant) (\s@S3DataAccessAsset' {} a -> s {kmsKeysToGrant = a} :: S3DataAccessAsset) Prelude.. Lens.mapping Lens.coerced

-- | The automatically-generated bucket-style alias for your Amazon S3 Access
-- Point. Customers can access their entitled data using the S3 Access
-- Point alias.
s3DataAccessAsset_s3AccessPointAlias :: Lens.Lens' S3DataAccessAsset (Prelude.Maybe Prelude.Text)
s3DataAccessAsset_s3AccessPointAlias = Lens.lens (\S3DataAccessAsset' {s3AccessPointAlias} -> s3AccessPointAlias) (\s@S3DataAccessAsset' {} a -> s {s3AccessPointAlias = a} :: S3DataAccessAsset)

-- | The ARN for your Amazon S3 Access Point. Customers can also access their
-- entitled data using the S3 Access Point ARN.
s3DataAccessAsset_s3AccessPointArn :: Lens.Lens' S3DataAccessAsset (Prelude.Maybe Prelude.Text)
s3DataAccessAsset_s3AccessPointArn = Lens.lens (\S3DataAccessAsset' {s3AccessPointArn} -> s3AccessPointArn) (\s@S3DataAccessAsset' {} a -> s {s3AccessPointArn = a} :: S3DataAccessAsset)

-- | The Amazon S3 bucket hosting data to be shared in the S3 data access.
s3DataAccessAsset_bucket :: Lens.Lens' S3DataAccessAsset Prelude.Text
s3DataAccessAsset_bucket = Lens.lens (\S3DataAccessAsset' {bucket} -> bucket) (\s@S3DataAccessAsset' {} a -> s {bucket = a} :: S3DataAccessAsset)

instance Data.FromJSON S3DataAccessAsset where
  parseJSON =
    Data.withObject
      "S3DataAccessAsset"
      ( \x ->
          S3DataAccessAsset'
            Prelude.<$> (x Data..:? "KeyPrefixes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Keys" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "KmsKeysToGrant")
            Prelude.<*> (x Data..:? "S3AccessPointAlias")
            Prelude.<*> (x Data..:? "S3AccessPointArn")
            Prelude.<*> (x Data..: "Bucket")
      )

instance Prelude.Hashable S3DataAccessAsset where
  hashWithSalt _salt S3DataAccessAsset' {..} =
    _salt
      `Prelude.hashWithSalt` keyPrefixes
      `Prelude.hashWithSalt` keys
      `Prelude.hashWithSalt` kmsKeysToGrant
      `Prelude.hashWithSalt` s3AccessPointAlias
      `Prelude.hashWithSalt` s3AccessPointArn
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData S3DataAccessAsset where
  rnf S3DataAccessAsset' {..} =
    Prelude.rnf keyPrefixes
      `Prelude.seq` Prelude.rnf keys
      `Prelude.seq` Prelude.rnf kmsKeysToGrant
      `Prelude.seq` Prelude.rnf s3AccessPointAlias
      `Prelude.seq` Prelude.rnf s3AccessPointArn
      `Prelude.seq` Prelude.rnf bucket
