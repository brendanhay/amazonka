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
-- Module      : Amazonka.MacieV2.Types.BucketServerSideEncryption
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.BucketServerSideEncryption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.Type
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the default server-side encryption settings
-- for an S3 bucket. For detailed information about these settings, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/bucket-encryption.html Setting default server-side encryption behavior for Amazon S3 buckets>
-- in the /Amazon Simple Storage Service User Guide/.
--
-- /See:/ 'newBucketServerSideEncryption' smart constructor.
data BucketServerSideEncryption = BucketServerSideEncryption'
  { -- | The Amazon Resource Name (ARN) or unique identifier (key ID) for the KMS
    -- key that\'s used by default to encrypt objects that are added to the
    -- bucket. This value is null if the bucket uses an Amazon S3 managed key
    -- to encrypt new objects or the bucket doesn\'t encrypt new objects by
    -- default.
    kmsMasterKeyId :: Prelude.Maybe Prelude.Text,
    -- | The type of server-side encryption that\'s used by default when storing
    -- new objects in the bucket. Possible values are:
    --
    -- -   AES256 - New objects are encrypted with an Amazon S3 managed key.
    --     They use SSE-S3 encryption.
    --
    -- -   aws:kms - New objects are encrypted with an KMS key
    --     (kmsMasterKeyId), either an Amazon Web Services managed key or a
    --     customer managed key. They use SSE-KMS encryption.
    --
    -- -   NONE - New objects aren\'t encrypted by default. Default encryption
    --     is disabled for the bucket.
    type' :: Prelude.Maybe Type
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BucketServerSideEncryption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsMasterKeyId', 'bucketServerSideEncryption_kmsMasterKeyId' - The Amazon Resource Name (ARN) or unique identifier (key ID) for the KMS
-- key that\'s used by default to encrypt objects that are added to the
-- bucket. This value is null if the bucket uses an Amazon S3 managed key
-- to encrypt new objects or the bucket doesn\'t encrypt new objects by
-- default.
--
-- 'type'', 'bucketServerSideEncryption_type' - The type of server-side encryption that\'s used by default when storing
-- new objects in the bucket. Possible values are:
--
-- -   AES256 - New objects are encrypted with an Amazon S3 managed key.
--     They use SSE-S3 encryption.
--
-- -   aws:kms - New objects are encrypted with an KMS key
--     (kmsMasterKeyId), either an Amazon Web Services managed key or a
--     customer managed key. They use SSE-KMS encryption.
--
-- -   NONE - New objects aren\'t encrypted by default. Default encryption
--     is disabled for the bucket.
newBucketServerSideEncryption ::
  BucketServerSideEncryption
newBucketServerSideEncryption =
  BucketServerSideEncryption'
    { kmsMasterKeyId =
        Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) or unique identifier (key ID) for the KMS
-- key that\'s used by default to encrypt objects that are added to the
-- bucket. This value is null if the bucket uses an Amazon S3 managed key
-- to encrypt new objects or the bucket doesn\'t encrypt new objects by
-- default.
bucketServerSideEncryption_kmsMasterKeyId :: Lens.Lens' BucketServerSideEncryption (Prelude.Maybe Prelude.Text)
bucketServerSideEncryption_kmsMasterKeyId = Lens.lens (\BucketServerSideEncryption' {kmsMasterKeyId} -> kmsMasterKeyId) (\s@BucketServerSideEncryption' {} a -> s {kmsMasterKeyId = a} :: BucketServerSideEncryption)

-- | The type of server-side encryption that\'s used by default when storing
-- new objects in the bucket. Possible values are:
--
-- -   AES256 - New objects are encrypted with an Amazon S3 managed key.
--     They use SSE-S3 encryption.
--
-- -   aws:kms - New objects are encrypted with an KMS key
--     (kmsMasterKeyId), either an Amazon Web Services managed key or a
--     customer managed key. They use SSE-KMS encryption.
--
-- -   NONE - New objects aren\'t encrypted by default. Default encryption
--     is disabled for the bucket.
bucketServerSideEncryption_type :: Lens.Lens' BucketServerSideEncryption (Prelude.Maybe Type)
bucketServerSideEncryption_type = Lens.lens (\BucketServerSideEncryption' {type'} -> type') (\s@BucketServerSideEncryption' {} a -> s {type' = a} :: BucketServerSideEncryption)

instance Data.FromJSON BucketServerSideEncryption where
  parseJSON =
    Data.withObject
      "BucketServerSideEncryption"
      ( \x ->
          BucketServerSideEncryption'
            Prelude.<$> (x Data..:? "kmsMasterKeyId")
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable BucketServerSideEncryption where
  hashWithSalt _salt BucketServerSideEncryption' {..} =
    _salt `Prelude.hashWithSalt` kmsMasterKeyId
      `Prelude.hashWithSalt` type'

instance Prelude.NFData BucketServerSideEncryption where
  rnf BucketServerSideEncryption' {..} =
    Prelude.rnf kmsMasterKeyId
      `Prelude.seq` Prelude.rnf type'
