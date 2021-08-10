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
-- Module      : Network.AWS.QLDB.Types.S3ExportConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QLDB.Types.S3ExportConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.QLDB.Types.S3EncryptionConfiguration

-- | The Amazon Simple Storage Service (Amazon S3) bucket location in which a
-- journal export job writes the journal contents.
--
-- /See:/ 'newS3ExportConfiguration' smart constructor.
data S3ExportConfiguration = S3ExportConfiguration'
  { -- | The Amazon S3 bucket name in which a journal export job writes the
    -- journal contents.
    --
    -- The bucket name must comply with the Amazon S3 bucket naming
    -- conventions. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html Bucket Restrictions and Limitations>
    -- in the /Amazon S3 Developer Guide/.
    bucket :: Prelude.Text,
    -- | The prefix for the Amazon S3 bucket in which a journal export job writes
    -- the journal contents.
    --
    -- The prefix must comply with Amazon S3 key naming rules and restrictions.
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html Object Key and Metadata>
    -- in the /Amazon S3 Developer Guide/.
    --
    -- The following are examples of valid @Prefix@ values:
    --
    -- -   @JournalExports-ForMyLedger\/Testing\/@
    --
    -- -   @JournalExports@
    --
    -- -   @My:Tests\/@
    prefix :: Prelude.Text,
    -- | The encryption settings that are used by a journal export job to write
    -- data in an Amazon S3 bucket.
    encryptionConfiguration :: S3EncryptionConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3ExportConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucket', 's3ExportConfiguration_bucket' - The Amazon S3 bucket name in which a journal export job writes the
-- journal contents.
--
-- The bucket name must comply with the Amazon S3 bucket naming
-- conventions. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html Bucket Restrictions and Limitations>
-- in the /Amazon S3 Developer Guide/.
--
-- 'prefix', 's3ExportConfiguration_prefix' - The prefix for the Amazon S3 bucket in which a journal export job writes
-- the journal contents.
--
-- The prefix must comply with Amazon S3 key naming rules and restrictions.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html Object Key and Metadata>
-- in the /Amazon S3 Developer Guide/.
--
-- The following are examples of valid @Prefix@ values:
--
-- -   @JournalExports-ForMyLedger\/Testing\/@
--
-- -   @JournalExports@
--
-- -   @My:Tests\/@
--
-- 'encryptionConfiguration', 's3ExportConfiguration_encryptionConfiguration' - The encryption settings that are used by a journal export job to write
-- data in an Amazon S3 bucket.
newS3ExportConfiguration ::
  -- | 'bucket'
  Prelude.Text ->
  -- | 'prefix'
  Prelude.Text ->
  -- | 'encryptionConfiguration'
  S3EncryptionConfiguration ->
  S3ExportConfiguration
newS3ExportConfiguration
  pBucket_
  pPrefix_
  pEncryptionConfiguration_ =
    S3ExportConfiguration'
      { bucket = pBucket_,
        prefix = pPrefix_,
        encryptionConfiguration = pEncryptionConfiguration_
      }

-- | The Amazon S3 bucket name in which a journal export job writes the
-- journal contents.
--
-- The bucket name must comply with the Amazon S3 bucket naming
-- conventions. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html Bucket Restrictions and Limitations>
-- in the /Amazon S3 Developer Guide/.
s3ExportConfiguration_bucket :: Lens.Lens' S3ExportConfiguration Prelude.Text
s3ExportConfiguration_bucket = Lens.lens (\S3ExportConfiguration' {bucket} -> bucket) (\s@S3ExportConfiguration' {} a -> s {bucket = a} :: S3ExportConfiguration)

-- | The prefix for the Amazon S3 bucket in which a journal export job writes
-- the journal contents.
--
-- The prefix must comply with Amazon S3 key naming rules and restrictions.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html Object Key and Metadata>
-- in the /Amazon S3 Developer Guide/.
--
-- The following are examples of valid @Prefix@ values:
--
-- -   @JournalExports-ForMyLedger\/Testing\/@
--
-- -   @JournalExports@
--
-- -   @My:Tests\/@
s3ExportConfiguration_prefix :: Lens.Lens' S3ExportConfiguration Prelude.Text
s3ExportConfiguration_prefix = Lens.lens (\S3ExportConfiguration' {prefix} -> prefix) (\s@S3ExportConfiguration' {} a -> s {prefix = a} :: S3ExportConfiguration)

-- | The encryption settings that are used by a journal export job to write
-- data in an Amazon S3 bucket.
s3ExportConfiguration_encryptionConfiguration :: Lens.Lens' S3ExportConfiguration S3EncryptionConfiguration
s3ExportConfiguration_encryptionConfiguration = Lens.lens (\S3ExportConfiguration' {encryptionConfiguration} -> encryptionConfiguration) (\s@S3ExportConfiguration' {} a -> s {encryptionConfiguration = a} :: S3ExportConfiguration)

instance Core.FromJSON S3ExportConfiguration where
  parseJSON =
    Core.withObject
      "S3ExportConfiguration"
      ( \x ->
          S3ExportConfiguration'
            Prelude.<$> (x Core..: "Bucket")
            Prelude.<*> (x Core..: "Prefix")
            Prelude.<*> (x Core..: "EncryptionConfiguration")
      )

instance Prelude.Hashable S3ExportConfiguration

instance Prelude.NFData S3ExportConfiguration

instance Core.ToJSON S3ExportConfiguration where
  toJSON S3ExportConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Bucket" Core..= bucket),
            Prelude.Just ("Prefix" Core..= prefix),
            Prelude.Just
              ( "EncryptionConfiguration"
                  Core..= encryptionConfiguration
              )
          ]
      )
