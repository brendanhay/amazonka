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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.S3Configuration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.S3Configuration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | For a SQL-based Kinesis Data Analytics application, provides a
-- description of an Amazon S3 data source, including the Amazon Resource
-- Name (ARN) of the S3 bucket and the name of the Amazon S3 object that
-- contains the data.
--
-- /See:/ 'newS3Configuration' smart constructor.
data S3Configuration = S3Configuration'
  { -- | The ARN of the S3 bucket that contains the data.
    bucketARN :: Prelude.Text,
    -- | The name of the object that contains the data.
    fileKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Configuration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketARN', 's3Configuration_bucketARN' - The ARN of the S3 bucket that contains the data.
--
-- 'fileKey', 's3Configuration_fileKey' - The name of the object that contains the data.
newS3Configuration ::
  -- | 'bucketARN'
  Prelude.Text ->
  -- | 'fileKey'
  Prelude.Text ->
  S3Configuration
newS3Configuration pBucketARN_ pFileKey_ =
  S3Configuration'
    { bucketARN = pBucketARN_,
      fileKey = pFileKey_
    }

-- | The ARN of the S3 bucket that contains the data.
s3Configuration_bucketARN :: Lens.Lens' S3Configuration Prelude.Text
s3Configuration_bucketARN = Lens.lens (\S3Configuration' {bucketARN} -> bucketARN) (\s@S3Configuration' {} a -> s {bucketARN = a} :: S3Configuration)

-- | The name of the object that contains the data.
s3Configuration_fileKey :: Lens.Lens' S3Configuration Prelude.Text
s3Configuration_fileKey = Lens.lens (\S3Configuration' {fileKey} -> fileKey) (\s@S3Configuration' {} a -> s {fileKey = a} :: S3Configuration)

instance Prelude.Hashable S3Configuration where
  hashWithSalt _salt S3Configuration' {..} =
    _salt
      `Prelude.hashWithSalt` bucketARN
      `Prelude.hashWithSalt` fileKey

instance Prelude.NFData S3Configuration where
  rnf S3Configuration' {..} =
    Prelude.rnf bucketARN
      `Prelude.seq` Prelude.rnf fileKey

instance Data.ToJSON S3Configuration where
  toJSON S3Configuration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("BucketARN" Data..= bucketARN),
            Prelude.Just ("FileKey" Data..= fileKey)
          ]
      )
