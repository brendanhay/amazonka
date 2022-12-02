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
-- Module      : Amazonka.MacieV2.Types.S3Bucket
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.S3Bucket where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.AllowsUnencryptedObjectUploads
import Amazonka.MacieV2.Types.BucketPublicAccess
import Amazonka.MacieV2.Types.KeyValuePair
import Amazonka.MacieV2.Types.S3BucketOwner
import Amazonka.MacieV2.Types.ServerSideEncryption
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the S3 bucket that a finding applies to.
--
-- /See:/ 'newS3Bucket' smart constructor.
data S3Bucket = S3Bucket'
  { -- | The tags that are associated with the bucket.
    tags :: Prelude.Maybe [KeyValuePair],
    -- | The name of the bucket.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of server-side encryption that\'s used by default to encrypt
    -- objects in the bucket.
    defaultServerSideEncryption :: Prelude.Maybe ServerSideEncryption,
    -- | The Amazon Resource Name (ARN) of the bucket.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the bucket policy for the bucket requires server-side
    -- encryption of objects when objects are uploaded to the bucket. Possible
    -- values are:
    --
    -- -   FALSE - The bucket policy requires server-side encryption of new
    --     objects. PutObject requests must include the
    --     x-amz-server-side-encryption header and the value for that header
    --     must be AES256 or aws:kms.
    --
    -- -   TRUE - The bucket doesn\'t have a bucket policy or it has a bucket
    --     policy that doesn\'t require server-side encryption of new objects.
    --     If a bucket policy exists, it doesn\'t require PutObject requests to
    --     include the x-amz-server-side-encryption header and it doesn\'t
    --     require the value for that header to be AES256 or aws:kms.
    --
    -- -   UNKNOWN - Amazon Macie can\'t determine whether the bucket policy
    --     requires server-side encryption of objects.
    allowsUnencryptedObjectUploads :: Prelude.Maybe AllowsUnencryptedObjectUploads,
    -- | The permissions settings that determine whether the bucket is publicly
    -- accessible.
    publicAccess :: Prelude.Maybe BucketPublicAccess,
    -- | The display name and canonical user ID for the Amazon Web Services
    -- account that owns the bucket.
    owner :: Prelude.Maybe S3BucketOwner,
    -- | The date and time, in UTC and extended ISO 8601 format, when the bucket
    -- was created.
    createdAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Bucket' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 's3Bucket_tags' - The tags that are associated with the bucket.
--
-- 'name', 's3Bucket_name' - The name of the bucket.
--
-- 'defaultServerSideEncryption', 's3Bucket_defaultServerSideEncryption' - The type of server-side encryption that\'s used by default to encrypt
-- objects in the bucket.
--
-- 'arn', 's3Bucket_arn' - The Amazon Resource Name (ARN) of the bucket.
--
-- 'allowsUnencryptedObjectUploads', 's3Bucket_allowsUnencryptedObjectUploads' - Specifies whether the bucket policy for the bucket requires server-side
-- encryption of objects when objects are uploaded to the bucket. Possible
-- values are:
--
-- -   FALSE - The bucket policy requires server-side encryption of new
--     objects. PutObject requests must include the
--     x-amz-server-side-encryption header and the value for that header
--     must be AES256 or aws:kms.
--
-- -   TRUE - The bucket doesn\'t have a bucket policy or it has a bucket
--     policy that doesn\'t require server-side encryption of new objects.
--     If a bucket policy exists, it doesn\'t require PutObject requests to
--     include the x-amz-server-side-encryption header and it doesn\'t
--     require the value for that header to be AES256 or aws:kms.
--
-- -   UNKNOWN - Amazon Macie can\'t determine whether the bucket policy
--     requires server-side encryption of objects.
--
-- 'publicAccess', 's3Bucket_publicAccess' - The permissions settings that determine whether the bucket is publicly
-- accessible.
--
-- 'owner', 's3Bucket_owner' - The display name and canonical user ID for the Amazon Web Services
-- account that owns the bucket.
--
-- 'createdAt', 's3Bucket_createdAt' - The date and time, in UTC and extended ISO 8601 format, when the bucket
-- was created.
newS3Bucket ::
  S3Bucket
newS3Bucket =
  S3Bucket'
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      defaultServerSideEncryption = Prelude.Nothing,
      arn = Prelude.Nothing,
      allowsUnencryptedObjectUploads = Prelude.Nothing,
      publicAccess = Prelude.Nothing,
      owner = Prelude.Nothing,
      createdAt = Prelude.Nothing
    }

-- | The tags that are associated with the bucket.
s3Bucket_tags :: Lens.Lens' S3Bucket (Prelude.Maybe [KeyValuePair])
s3Bucket_tags = Lens.lens (\S3Bucket' {tags} -> tags) (\s@S3Bucket' {} a -> s {tags = a} :: S3Bucket) Prelude.. Lens.mapping Lens.coerced

-- | The name of the bucket.
s3Bucket_name :: Lens.Lens' S3Bucket (Prelude.Maybe Prelude.Text)
s3Bucket_name = Lens.lens (\S3Bucket' {name} -> name) (\s@S3Bucket' {} a -> s {name = a} :: S3Bucket)

-- | The type of server-side encryption that\'s used by default to encrypt
-- objects in the bucket.
s3Bucket_defaultServerSideEncryption :: Lens.Lens' S3Bucket (Prelude.Maybe ServerSideEncryption)
s3Bucket_defaultServerSideEncryption = Lens.lens (\S3Bucket' {defaultServerSideEncryption} -> defaultServerSideEncryption) (\s@S3Bucket' {} a -> s {defaultServerSideEncryption = a} :: S3Bucket)

-- | The Amazon Resource Name (ARN) of the bucket.
s3Bucket_arn :: Lens.Lens' S3Bucket (Prelude.Maybe Prelude.Text)
s3Bucket_arn = Lens.lens (\S3Bucket' {arn} -> arn) (\s@S3Bucket' {} a -> s {arn = a} :: S3Bucket)

-- | Specifies whether the bucket policy for the bucket requires server-side
-- encryption of objects when objects are uploaded to the bucket. Possible
-- values are:
--
-- -   FALSE - The bucket policy requires server-side encryption of new
--     objects. PutObject requests must include the
--     x-amz-server-side-encryption header and the value for that header
--     must be AES256 or aws:kms.
--
-- -   TRUE - The bucket doesn\'t have a bucket policy or it has a bucket
--     policy that doesn\'t require server-side encryption of new objects.
--     If a bucket policy exists, it doesn\'t require PutObject requests to
--     include the x-amz-server-side-encryption header and it doesn\'t
--     require the value for that header to be AES256 or aws:kms.
--
-- -   UNKNOWN - Amazon Macie can\'t determine whether the bucket policy
--     requires server-side encryption of objects.
s3Bucket_allowsUnencryptedObjectUploads :: Lens.Lens' S3Bucket (Prelude.Maybe AllowsUnencryptedObjectUploads)
s3Bucket_allowsUnencryptedObjectUploads = Lens.lens (\S3Bucket' {allowsUnencryptedObjectUploads} -> allowsUnencryptedObjectUploads) (\s@S3Bucket' {} a -> s {allowsUnencryptedObjectUploads = a} :: S3Bucket)

-- | The permissions settings that determine whether the bucket is publicly
-- accessible.
s3Bucket_publicAccess :: Lens.Lens' S3Bucket (Prelude.Maybe BucketPublicAccess)
s3Bucket_publicAccess = Lens.lens (\S3Bucket' {publicAccess} -> publicAccess) (\s@S3Bucket' {} a -> s {publicAccess = a} :: S3Bucket)

-- | The display name and canonical user ID for the Amazon Web Services
-- account that owns the bucket.
s3Bucket_owner :: Lens.Lens' S3Bucket (Prelude.Maybe S3BucketOwner)
s3Bucket_owner = Lens.lens (\S3Bucket' {owner} -> owner) (\s@S3Bucket' {} a -> s {owner = a} :: S3Bucket)

-- | The date and time, in UTC and extended ISO 8601 format, when the bucket
-- was created.
s3Bucket_createdAt :: Lens.Lens' S3Bucket (Prelude.Maybe Prelude.UTCTime)
s3Bucket_createdAt = Lens.lens (\S3Bucket' {createdAt} -> createdAt) (\s@S3Bucket' {} a -> s {createdAt = a} :: S3Bucket) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON S3Bucket where
  parseJSON =
    Data.withObject
      "S3Bucket"
      ( \x ->
          S3Bucket'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "defaultServerSideEncryption")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "allowsUnencryptedObjectUploads")
            Prelude.<*> (x Data..:? "publicAccess")
            Prelude.<*> (x Data..:? "owner")
            Prelude.<*> (x Data..:? "createdAt")
      )

instance Prelude.Hashable S3Bucket where
  hashWithSalt _salt S3Bucket' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` defaultServerSideEncryption
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` allowsUnencryptedObjectUploads
      `Prelude.hashWithSalt` publicAccess
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` createdAt

instance Prelude.NFData S3Bucket where
  rnf S3Bucket' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf defaultServerSideEncryption
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf allowsUnencryptedObjectUploads
      `Prelude.seq` Prelude.rnf publicAccess
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf createdAt
