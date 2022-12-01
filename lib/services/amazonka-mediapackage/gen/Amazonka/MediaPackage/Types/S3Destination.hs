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
-- Module      : Amazonka.MediaPackage.Types.S3Destination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackage.Types.S3Destination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Configuration parameters for where in an S3 bucket to place the
-- harvested content
--
-- /See:/ 'newS3Destination' smart constructor.
data S3Destination = S3Destination'
  { -- | The key in the specified S3 bucket where the harvested top-level
    -- manifest will be placed.
    manifestKey :: Prelude.Text,
    -- | The name of an S3 bucket within which harvested content will be exported
    bucketName :: Prelude.Text,
    -- | The IAM role used to write to the specified S3 bucket
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Destination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'manifestKey', 's3Destination_manifestKey' - The key in the specified S3 bucket where the harvested top-level
-- manifest will be placed.
--
-- 'bucketName', 's3Destination_bucketName' - The name of an S3 bucket within which harvested content will be exported
--
-- 'roleArn', 's3Destination_roleArn' - The IAM role used to write to the specified S3 bucket
newS3Destination ::
  -- | 'manifestKey'
  Prelude.Text ->
  -- | 'bucketName'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  S3Destination
newS3Destination pManifestKey_ pBucketName_ pRoleArn_ =
  S3Destination'
    { manifestKey = pManifestKey_,
      bucketName = pBucketName_,
      roleArn = pRoleArn_
    }

-- | The key in the specified S3 bucket where the harvested top-level
-- manifest will be placed.
s3Destination_manifestKey :: Lens.Lens' S3Destination Prelude.Text
s3Destination_manifestKey = Lens.lens (\S3Destination' {manifestKey} -> manifestKey) (\s@S3Destination' {} a -> s {manifestKey = a} :: S3Destination)

-- | The name of an S3 bucket within which harvested content will be exported
s3Destination_bucketName :: Lens.Lens' S3Destination Prelude.Text
s3Destination_bucketName = Lens.lens (\S3Destination' {bucketName} -> bucketName) (\s@S3Destination' {} a -> s {bucketName = a} :: S3Destination)

-- | The IAM role used to write to the specified S3 bucket
s3Destination_roleArn :: Lens.Lens' S3Destination Prelude.Text
s3Destination_roleArn = Lens.lens (\S3Destination' {roleArn} -> roleArn) (\s@S3Destination' {} a -> s {roleArn = a} :: S3Destination)

instance Core.FromJSON S3Destination where
  parseJSON =
    Core.withObject
      "S3Destination"
      ( \x ->
          S3Destination'
            Prelude.<$> (x Core..: "manifestKey")
            Prelude.<*> (x Core..: "bucketName")
            Prelude.<*> (x Core..: "roleArn")
      )

instance Prelude.Hashable S3Destination where
  hashWithSalt _salt S3Destination' {..} =
    _salt `Prelude.hashWithSalt` manifestKey
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData S3Destination where
  rnf S3Destination' {..} =
    Prelude.rnf manifestKey
      `Prelude.seq` Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf roleArn

instance Core.ToJSON S3Destination where
  toJSON S3Destination' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("manifestKey" Core..= manifestKey),
            Prelude.Just ("bucketName" Core..= bucketName),
            Prelude.Just ("roleArn" Core..= roleArn)
          ]
      )
