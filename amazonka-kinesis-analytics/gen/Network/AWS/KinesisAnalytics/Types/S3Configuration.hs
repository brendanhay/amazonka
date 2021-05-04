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
-- Module      : Network.AWS.KinesisAnalytics.Types.S3Configuration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.S3Configuration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides a description of an Amazon S3 data source, including the Amazon
-- Resource Name (ARN) of the S3 bucket, the ARN of the IAM role that is
-- used to access the bucket, and the name of the Amazon S3 object that
-- contains the data.
--
-- /See:/ 'newS3Configuration' smart constructor.
data S3Configuration = S3Configuration'
  { -- | IAM ARN of the role used to access the data.
    roleARN :: Prelude.Text,
    -- | ARN of the S3 bucket that contains the data.
    bucketARN :: Prelude.Text,
    -- | The name of the object that contains the data.
    fileKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'S3Configuration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleARN', 's3Configuration_roleARN' - IAM ARN of the role used to access the data.
--
-- 'bucketARN', 's3Configuration_bucketARN' - ARN of the S3 bucket that contains the data.
--
-- 'fileKey', 's3Configuration_fileKey' - The name of the object that contains the data.
newS3Configuration ::
  -- | 'roleARN'
  Prelude.Text ->
  -- | 'bucketARN'
  Prelude.Text ->
  -- | 'fileKey'
  Prelude.Text ->
  S3Configuration
newS3Configuration pRoleARN_ pBucketARN_ pFileKey_ =
  S3Configuration'
    { roleARN = pRoleARN_,
      bucketARN = pBucketARN_,
      fileKey = pFileKey_
    }

-- | IAM ARN of the role used to access the data.
s3Configuration_roleARN :: Lens.Lens' S3Configuration Prelude.Text
s3Configuration_roleARN = Lens.lens (\S3Configuration' {roleARN} -> roleARN) (\s@S3Configuration' {} a -> s {roleARN = a} :: S3Configuration)

-- | ARN of the S3 bucket that contains the data.
s3Configuration_bucketARN :: Lens.Lens' S3Configuration Prelude.Text
s3Configuration_bucketARN = Lens.lens (\S3Configuration' {bucketARN} -> bucketARN) (\s@S3Configuration' {} a -> s {bucketARN = a} :: S3Configuration)

-- | The name of the object that contains the data.
s3Configuration_fileKey :: Lens.Lens' S3Configuration Prelude.Text
s3Configuration_fileKey = Lens.lens (\S3Configuration' {fileKey} -> fileKey) (\s@S3Configuration' {} a -> s {fileKey = a} :: S3Configuration)

instance Prelude.Hashable S3Configuration

instance Prelude.NFData S3Configuration

instance Prelude.ToJSON S3Configuration where
  toJSON S3Configuration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("RoleARN" Prelude..= roleARN),
            Prelude.Just ("BucketARN" Prelude..= bucketARN),
            Prelude.Just ("FileKey" Prelude..= fileKey)
          ]
      )
