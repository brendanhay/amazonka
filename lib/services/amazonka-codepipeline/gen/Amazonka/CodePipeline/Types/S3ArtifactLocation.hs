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
-- Module      : Amazonka.CodePipeline.Types.S3ArtifactLocation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.S3ArtifactLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON S3ArtifactLocation where
  parseJSON =
    Data.withObject
      "S3ArtifactLocation"
      ( \x ->
          S3ArtifactLocation'
            Prelude.<$> (x Data..: "bucketName")
            Prelude.<*> (x Data..: "objectKey")
      )

instance Prelude.Hashable S3ArtifactLocation where
  hashWithSalt _salt S3ArtifactLocation' {..} =
    _salt
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` objectKey

instance Prelude.NFData S3ArtifactLocation where
  rnf S3ArtifactLocation' {..} =
    Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf objectKey
