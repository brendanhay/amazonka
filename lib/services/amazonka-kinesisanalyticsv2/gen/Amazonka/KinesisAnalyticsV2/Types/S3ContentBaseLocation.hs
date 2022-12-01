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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.S3ContentBaseLocation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.S3ContentBaseLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The S3 bucket that holds the application information.
--
-- /See:/ 'newS3ContentBaseLocation' smart constructor.
data S3ContentBaseLocation = S3ContentBaseLocation'
  { -- | The base path for the S3 bucket.
    basePath :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the S3 bucket.
    bucketARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3ContentBaseLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'basePath', 's3ContentBaseLocation_basePath' - The base path for the S3 bucket.
--
-- 'bucketARN', 's3ContentBaseLocation_bucketARN' - The Amazon Resource Name (ARN) of the S3 bucket.
newS3ContentBaseLocation ::
  -- | 'bucketARN'
  Prelude.Text ->
  S3ContentBaseLocation
newS3ContentBaseLocation pBucketARN_ =
  S3ContentBaseLocation'
    { basePath = Prelude.Nothing,
      bucketARN = pBucketARN_
    }

-- | The base path for the S3 bucket.
s3ContentBaseLocation_basePath :: Lens.Lens' S3ContentBaseLocation (Prelude.Maybe Prelude.Text)
s3ContentBaseLocation_basePath = Lens.lens (\S3ContentBaseLocation' {basePath} -> basePath) (\s@S3ContentBaseLocation' {} a -> s {basePath = a} :: S3ContentBaseLocation)

-- | The Amazon Resource Name (ARN) of the S3 bucket.
s3ContentBaseLocation_bucketARN :: Lens.Lens' S3ContentBaseLocation Prelude.Text
s3ContentBaseLocation_bucketARN = Lens.lens (\S3ContentBaseLocation' {bucketARN} -> bucketARN) (\s@S3ContentBaseLocation' {} a -> s {bucketARN = a} :: S3ContentBaseLocation)

instance Prelude.Hashable S3ContentBaseLocation where
  hashWithSalt _salt S3ContentBaseLocation' {..} =
    _salt `Prelude.hashWithSalt` basePath
      `Prelude.hashWithSalt` bucketARN

instance Prelude.NFData S3ContentBaseLocation where
  rnf S3ContentBaseLocation' {..} =
    Prelude.rnf basePath
      `Prelude.seq` Prelude.rnf bucketARN

instance Core.ToJSON S3ContentBaseLocation where
  toJSON S3ContentBaseLocation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("BasePath" Core..=) Prelude.<$> basePath,
            Prelude.Just ("BucketARN" Core..= bucketARN)
          ]
      )
