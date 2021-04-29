{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodePipeline.Types.S3ArtifactLocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.S3ArtifactLocation where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The location of the S3 bucket that contains a revision.
--
-- /See:/ 'newS3ArtifactLocation' smart constructor.
data S3ArtifactLocation = S3ArtifactLocation'
  { -- | The name of the S3 bucket.
    bucketName :: Prelude.Text,
    -- | The key of the object in the S3 bucket, which uniquely identifies the
    -- object in the bucket.
    objectKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'S3ArtifactLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketName', 's3ArtifactLocation_bucketName' - The name of the S3 bucket.
--
-- 'objectKey', 's3ArtifactLocation_objectKey' - The key of the object in the S3 bucket, which uniquely identifies the
-- object in the bucket.
newS3ArtifactLocation ::
  -- | 'bucketName'
  Prelude.Text ->
  -- | 'objectKey'
  Prelude.Text ->
  S3ArtifactLocation
newS3ArtifactLocation pBucketName_ pObjectKey_ =
  S3ArtifactLocation'
    { bucketName = pBucketName_,
      objectKey = pObjectKey_
    }

-- | The name of the S3 bucket.
s3ArtifactLocation_bucketName :: Lens.Lens' S3ArtifactLocation Prelude.Text
s3ArtifactLocation_bucketName = Lens.lens (\S3ArtifactLocation' {bucketName} -> bucketName) (\s@S3ArtifactLocation' {} a -> s {bucketName = a} :: S3ArtifactLocation)

-- | The key of the object in the S3 bucket, which uniquely identifies the
-- object in the bucket.
s3ArtifactLocation_objectKey :: Lens.Lens' S3ArtifactLocation Prelude.Text
s3ArtifactLocation_objectKey = Lens.lens (\S3ArtifactLocation' {objectKey} -> objectKey) (\s@S3ArtifactLocation' {} a -> s {objectKey = a} :: S3ArtifactLocation)

instance Prelude.FromJSON S3ArtifactLocation where
  parseJSON =
    Prelude.withObject
      "S3ArtifactLocation"
      ( \x ->
          S3ArtifactLocation'
            Prelude.<$> (x Prelude..: "bucketName")
            Prelude.<*> (x Prelude..: "objectKey")
      )

instance Prelude.Hashable S3ArtifactLocation

instance Prelude.NFData S3ArtifactLocation
