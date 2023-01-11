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
-- Module      : Amazonka.QLDB.Types.S3ExportConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QLDB.Types.S3ExportConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QLDB.Types.S3EncryptionConfiguration

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

instance Data.FromJSON S3ExportConfiguration where
  parseJSON =
    Data.withObject
      "S3ExportConfiguration"
      ( \x ->
          S3ExportConfiguration'
            Prelude.<$> (x Data..: "Bucket")
            Prelude.<*> (x Data..: "Prefix")
            Prelude.<*> (x Data..: "EncryptionConfiguration")
      )

instance Prelude.Hashable S3ExportConfiguration where
  hashWithSalt _salt S3ExportConfiguration' {..} =
    _salt `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` encryptionConfiguration

instance Prelude.NFData S3ExportConfiguration where
  rnf S3ExportConfiguration' {..} =
    Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf encryptionConfiguration

instance Data.ToJSON S3ExportConfiguration where
  toJSON S3ExportConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Bucket" Data..= bucket),
            Prelude.Just ("Prefix" Data..= prefix),
            Prelude.Just
              ( "EncryptionConfiguration"
                  Data..= encryptionConfiguration
              )
          ]
      )
