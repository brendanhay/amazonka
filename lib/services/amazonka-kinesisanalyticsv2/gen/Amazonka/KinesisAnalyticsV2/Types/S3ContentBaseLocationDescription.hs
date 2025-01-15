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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.S3ContentBaseLocationDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.S3ContentBaseLocationDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The description of the S3 base location that holds the application.
--
-- /See:/ 'newS3ContentBaseLocationDescription' smart constructor.
data S3ContentBaseLocationDescription = S3ContentBaseLocationDescription'
  { -- | The base path for the S3 bucket.
    basePath :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the S3 bucket.
    bucketARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3ContentBaseLocationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'basePath', 's3ContentBaseLocationDescription_basePath' - The base path for the S3 bucket.
--
-- 'bucketARN', 's3ContentBaseLocationDescription_bucketARN' - The Amazon Resource Name (ARN) of the S3 bucket.
newS3ContentBaseLocationDescription ::
  -- | 'bucketARN'
  Prelude.Text ->
  S3ContentBaseLocationDescription
newS3ContentBaseLocationDescription pBucketARN_ =
  S3ContentBaseLocationDescription'
    { basePath =
        Prelude.Nothing,
      bucketARN = pBucketARN_
    }

-- | The base path for the S3 bucket.
s3ContentBaseLocationDescription_basePath :: Lens.Lens' S3ContentBaseLocationDescription (Prelude.Maybe Prelude.Text)
s3ContentBaseLocationDescription_basePath = Lens.lens (\S3ContentBaseLocationDescription' {basePath} -> basePath) (\s@S3ContentBaseLocationDescription' {} a -> s {basePath = a} :: S3ContentBaseLocationDescription)

-- | The Amazon Resource Name (ARN) of the S3 bucket.
s3ContentBaseLocationDescription_bucketARN :: Lens.Lens' S3ContentBaseLocationDescription Prelude.Text
s3ContentBaseLocationDescription_bucketARN = Lens.lens (\S3ContentBaseLocationDescription' {bucketARN} -> bucketARN) (\s@S3ContentBaseLocationDescription' {} a -> s {bucketARN = a} :: S3ContentBaseLocationDescription)

instance
  Data.FromJSON
    S3ContentBaseLocationDescription
  where
  parseJSON =
    Data.withObject
      "S3ContentBaseLocationDescription"
      ( \x ->
          S3ContentBaseLocationDescription'
            Prelude.<$> (x Data..:? "BasePath")
            Prelude.<*> (x Data..: "BucketARN")
      )

instance
  Prelude.Hashable
    S3ContentBaseLocationDescription
  where
  hashWithSalt
    _salt
    S3ContentBaseLocationDescription' {..} =
      _salt
        `Prelude.hashWithSalt` basePath
        `Prelude.hashWithSalt` bucketARN

instance
  Prelude.NFData
    S3ContentBaseLocationDescription
  where
  rnf S3ContentBaseLocationDescription' {..} =
    Prelude.rnf basePath `Prelude.seq`
      Prelude.rnf bucketARN
