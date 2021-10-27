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
-- Module      : Network.AWS.SecurityHub.Types.AwsS3BucketServerSideEncryptionByDefault
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsS3BucketServerSideEncryptionByDefault where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the default server-side encryption to apply to new objects in
-- the bucket.
--
-- /See:/ 'newAwsS3BucketServerSideEncryptionByDefault' smart constructor.
data AwsS3BucketServerSideEncryptionByDefault = AwsS3BucketServerSideEncryptionByDefault'
  { -- | Server-side encryption algorithm to use for the default encryption.
    sSEAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | KMS key ID to use for the default encryption.
    kmsMasterKeyID :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsS3BucketServerSideEncryptionByDefault' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sSEAlgorithm', 'awsS3BucketServerSideEncryptionByDefault_sSEAlgorithm' - Server-side encryption algorithm to use for the default encryption.
--
-- 'kmsMasterKeyID', 'awsS3BucketServerSideEncryptionByDefault_kmsMasterKeyID' - KMS key ID to use for the default encryption.
newAwsS3BucketServerSideEncryptionByDefault ::
  AwsS3BucketServerSideEncryptionByDefault
newAwsS3BucketServerSideEncryptionByDefault =
  AwsS3BucketServerSideEncryptionByDefault'
    { sSEAlgorithm =
        Prelude.Nothing,
      kmsMasterKeyID = Prelude.Nothing
    }

-- | Server-side encryption algorithm to use for the default encryption.
awsS3BucketServerSideEncryptionByDefault_sSEAlgorithm :: Lens.Lens' AwsS3BucketServerSideEncryptionByDefault (Prelude.Maybe Prelude.Text)
awsS3BucketServerSideEncryptionByDefault_sSEAlgorithm = Lens.lens (\AwsS3BucketServerSideEncryptionByDefault' {sSEAlgorithm} -> sSEAlgorithm) (\s@AwsS3BucketServerSideEncryptionByDefault' {} a -> s {sSEAlgorithm = a} :: AwsS3BucketServerSideEncryptionByDefault)

-- | KMS key ID to use for the default encryption.
awsS3BucketServerSideEncryptionByDefault_kmsMasterKeyID :: Lens.Lens' AwsS3BucketServerSideEncryptionByDefault (Prelude.Maybe Prelude.Text)
awsS3BucketServerSideEncryptionByDefault_kmsMasterKeyID = Lens.lens (\AwsS3BucketServerSideEncryptionByDefault' {kmsMasterKeyID} -> kmsMasterKeyID) (\s@AwsS3BucketServerSideEncryptionByDefault' {} a -> s {kmsMasterKeyID = a} :: AwsS3BucketServerSideEncryptionByDefault)

instance
  Core.FromJSON
    AwsS3BucketServerSideEncryptionByDefault
  where
  parseJSON =
    Core.withObject
      "AwsS3BucketServerSideEncryptionByDefault"
      ( \x ->
          AwsS3BucketServerSideEncryptionByDefault'
            Prelude.<$> (x Core..:? "SSEAlgorithm")
            Prelude.<*> (x Core..:? "KMSMasterKeyID")
      )

instance
  Prelude.Hashable
    AwsS3BucketServerSideEncryptionByDefault

instance
  Prelude.NFData
    AwsS3BucketServerSideEncryptionByDefault

instance
  Core.ToJSON
    AwsS3BucketServerSideEncryptionByDefault
  where
  toJSON AwsS3BucketServerSideEncryptionByDefault' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SSEAlgorithm" Core..=) Prelude.<$> sSEAlgorithm,
            ("KMSMasterKeyID" Core..=)
              Prelude.<$> kmsMasterKeyID
          ]
      )
