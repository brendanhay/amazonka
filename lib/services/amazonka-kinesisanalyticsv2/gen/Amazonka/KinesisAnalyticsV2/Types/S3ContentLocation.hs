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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.S3ContentLocation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.S3ContentLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | For a Kinesis Data Analytics application provides a description of an
-- Amazon S3 object, including the Amazon Resource Name (ARN) of the S3
-- bucket, the name of the Amazon S3 object that contains the data, and the
-- version number of the Amazon S3 object that contains the data.
--
-- /See:/ 'newS3ContentLocation' smart constructor.
data S3ContentLocation = S3ContentLocation'
  { -- | The version of the object containing the application code.
    objectVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the S3 bucket containing the
    -- application code.
    bucketARN :: Prelude.Text,
    -- | The file key for the object containing the application code.
    fileKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3ContentLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectVersion', 's3ContentLocation_objectVersion' - The version of the object containing the application code.
--
-- 'bucketARN', 's3ContentLocation_bucketARN' - The Amazon Resource Name (ARN) for the S3 bucket containing the
-- application code.
--
-- 'fileKey', 's3ContentLocation_fileKey' - The file key for the object containing the application code.
newS3ContentLocation ::
  -- | 'bucketARN'
  Prelude.Text ->
  -- | 'fileKey'
  Prelude.Text ->
  S3ContentLocation
newS3ContentLocation pBucketARN_ pFileKey_ =
  S3ContentLocation'
    { objectVersion = Prelude.Nothing,
      bucketARN = pBucketARN_,
      fileKey = pFileKey_
    }

-- | The version of the object containing the application code.
s3ContentLocation_objectVersion :: Lens.Lens' S3ContentLocation (Prelude.Maybe Prelude.Text)
s3ContentLocation_objectVersion = Lens.lens (\S3ContentLocation' {objectVersion} -> objectVersion) (\s@S3ContentLocation' {} a -> s {objectVersion = a} :: S3ContentLocation)

-- | The Amazon Resource Name (ARN) for the S3 bucket containing the
-- application code.
s3ContentLocation_bucketARN :: Lens.Lens' S3ContentLocation Prelude.Text
s3ContentLocation_bucketARN = Lens.lens (\S3ContentLocation' {bucketARN} -> bucketARN) (\s@S3ContentLocation' {} a -> s {bucketARN = a} :: S3ContentLocation)

-- | The file key for the object containing the application code.
s3ContentLocation_fileKey :: Lens.Lens' S3ContentLocation Prelude.Text
s3ContentLocation_fileKey = Lens.lens (\S3ContentLocation' {fileKey} -> fileKey) (\s@S3ContentLocation' {} a -> s {fileKey = a} :: S3ContentLocation)

instance Data.FromJSON S3ContentLocation where
  parseJSON =
    Data.withObject
      "S3ContentLocation"
      ( \x ->
          S3ContentLocation'
            Prelude.<$> (x Data..:? "ObjectVersion")
            Prelude.<*> (x Data..: "BucketARN")
            Prelude.<*> (x Data..: "FileKey")
      )

instance Prelude.Hashable S3ContentLocation where
  hashWithSalt _salt S3ContentLocation' {..} =
    _salt
      `Prelude.hashWithSalt` objectVersion
      `Prelude.hashWithSalt` bucketARN
      `Prelude.hashWithSalt` fileKey

instance Prelude.NFData S3ContentLocation where
  rnf S3ContentLocation' {..} =
    Prelude.rnf objectVersion
      `Prelude.seq` Prelude.rnf bucketARN
      `Prelude.seq` Prelude.rnf fileKey

instance Data.ToJSON S3ContentLocation where
  toJSON S3ContentLocation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ObjectVersion" Data..=) Prelude.<$> objectVersion,
            Prelude.Just ("BucketARN" Data..= bucketARN),
            Prelude.Just ("FileKey" Data..= fileKey)
          ]
      )
