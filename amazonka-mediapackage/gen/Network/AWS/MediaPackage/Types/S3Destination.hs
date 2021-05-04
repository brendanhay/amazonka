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
-- Module      : Network.AWS.MediaPackage.Types.S3Destination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.S3Destination where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON S3Destination where
  parseJSON =
    Prelude.withObject
      "S3Destination"
      ( \x ->
          S3Destination'
            Prelude.<$> (x Prelude..: "manifestKey")
            Prelude.<*> (x Prelude..: "bucketName")
            Prelude.<*> (x Prelude..: "roleArn")
      )

instance Prelude.Hashable S3Destination

instance Prelude.NFData S3Destination

instance Prelude.ToJSON S3Destination where
  toJSON S3Destination' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("manifestKey" Prelude..= manifestKey),
            Prelude.Just ("bucketName" Prelude..= bucketName),
            Prelude.Just ("roleArn" Prelude..= roleArn)
          ]
      )
