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
-- Module      : Amazonka.KafkaConnect.Types.S3Location
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.S3Location where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The location of an object in Amazon S3.
--
-- /See:/ 'newS3Location' smart constructor.
data S3Location = S3Location'
  { -- | The version of an object in an S3 bucket.
    objectVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an S3 bucket.
    bucketArn :: Prelude.Text,
    -- | The file key for an object in an S3 bucket.
    fileKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Location' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectVersion', 's3Location_objectVersion' - The version of an object in an S3 bucket.
--
-- 'bucketArn', 's3Location_bucketArn' - The Amazon Resource Name (ARN) of an S3 bucket.
--
-- 'fileKey', 's3Location_fileKey' - The file key for an object in an S3 bucket.
newS3Location ::
  -- | 'bucketArn'
  Prelude.Text ->
  -- | 'fileKey'
  Prelude.Text ->
  S3Location
newS3Location pBucketArn_ pFileKey_ =
  S3Location'
    { objectVersion = Prelude.Nothing,
      bucketArn = pBucketArn_,
      fileKey = pFileKey_
    }

-- | The version of an object in an S3 bucket.
s3Location_objectVersion :: Lens.Lens' S3Location (Prelude.Maybe Prelude.Text)
s3Location_objectVersion = Lens.lens (\S3Location' {objectVersion} -> objectVersion) (\s@S3Location' {} a -> s {objectVersion = a} :: S3Location)

-- | The Amazon Resource Name (ARN) of an S3 bucket.
s3Location_bucketArn :: Lens.Lens' S3Location Prelude.Text
s3Location_bucketArn = Lens.lens (\S3Location' {bucketArn} -> bucketArn) (\s@S3Location' {} a -> s {bucketArn = a} :: S3Location)

-- | The file key for an object in an S3 bucket.
s3Location_fileKey :: Lens.Lens' S3Location Prelude.Text
s3Location_fileKey = Lens.lens (\S3Location' {fileKey} -> fileKey) (\s@S3Location' {} a -> s {fileKey = a} :: S3Location)

instance Prelude.Hashable S3Location where
  hashWithSalt _salt S3Location' {..} =
    _salt `Prelude.hashWithSalt` objectVersion
      `Prelude.hashWithSalt` bucketArn
      `Prelude.hashWithSalt` fileKey

instance Prelude.NFData S3Location where
  rnf S3Location' {..} =
    Prelude.rnf objectVersion
      `Prelude.seq` Prelude.rnf bucketArn
      `Prelude.seq` Prelude.rnf fileKey

instance Core.ToJSON S3Location where
  toJSON S3Location' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("objectVersion" Core..=) Prelude.<$> objectVersion,
            Prelude.Just ("bucketArn" Core..= bucketArn),
            Prelude.Just ("fileKey" Core..= fileKey)
          ]
      )
