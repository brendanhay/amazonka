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
-- Module      : Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceUpdate where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the S3 bucket name, object key name, and IAM role that Amazon
-- Kinesis Analytics can assume to read the Amazon S3 object on your behalf
-- and populate the in-application reference table.
--
-- /See:/ 'newS3ReferenceDataSourceUpdate' smart constructor.
data S3ReferenceDataSourceUpdate = S3ReferenceDataSourceUpdate'
  { -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to read the
    -- Amazon S3 object and populate the in-application.
    referenceRoleARNUpdate :: Prelude.Maybe Prelude.Text,
    -- | Object key name.
    fileKeyUpdate :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the S3 bucket.
    bucketARNUpdate :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'S3ReferenceDataSourceUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'referenceRoleARNUpdate', 's3ReferenceDataSourceUpdate_referenceRoleARNUpdate' - ARN of the IAM role that Amazon Kinesis Analytics can assume to read the
-- Amazon S3 object and populate the in-application.
--
-- 'fileKeyUpdate', 's3ReferenceDataSourceUpdate_fileKeyUpdate' - Object key name.
--
-- 'bucketARNUpdate', 's3ReferenceDataSourceUpdate_bucketARNUpdate' - Amazon Resource Name (ARN) of the S3 bucket.
newS3ReferenceDataSourceUpdate ::
  S3ReferenceDataSourceUpdate
newS3ReferenceDataSourceUpdate =
  S3ReferenceDataSourceUpdate'
    { referenceRoleARNUpdate =
        Prelude.Nothing,
      fileKeyUpdate = Prelude.Nothing,
      bucketARNUpdate = Prelude.Nothing
    }

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to read the
-- Amazon S3 object and populate the in-application.
s3ReferenceDataSourceUpdate_referenceRoleARNUpdate :: Lens.Lens' S3ReferenceDataSourceUpdate (Prelude.Maybe Prelude.Text)
s3ReferenceDataSourceUpdate_referenceRoleARNUpdate = Lens.lens (\S3ReferenceDataSourceUpdate' {referenceRoleARNUpdate} -> referenceRoleARNUpdate) (\s@S3ReferenceDataSourceUpdate' {} a -> s {referenceRoleARNUpdate = a} :: S3ReferenceDataSourceUpdate)

-- | Object key name.
s3ReferenceDataSourceUpdate_fileKeyUpdate :: Lens.Lens' S3ReferenceDataSourceUpdate (Prelude.Maybe Prelude.Text)
s3ReferenceDataSourceUpdate_fileKeyUpdate = Lens.lens (\S3ReferenceDataSourceUpdate' {fileKeyUpdate} -> fileKeyUpdate) (\s@S3ReferenceDataSourceUpdate' {} a -> s {fileKeyUpdate = a} :: S3ReferenceDataSourceUpdate)

-- | Amazon Resource Name (ARN) of the S3 bucket.
s3ReferenceDataSourceUpdate_bucketARNUpdate :: Lens.Lens' S3ReferenceDataSourceUpdate (Prelude.Maybe Prelude.Text)
s3ReferenceDataSourceUpdate_bucketARNUpdate = Lens.lens (\S3ReferenceDataSourceUpdate' {bucketARNUpdate} -> bucketARNUpdate) (\s@S3ReferenceDataSourceUpdate' {} a -> s {bucketARNUpdate = a} :: S3ReferenceDataSourceUpdate)

instance Prelude.Hashable S3ReferenceDataSourceUpdate

instance Prelude.NFData S3ReferenceDataSourceUpdate

instance Prelude.ToJSON S3ReferenceDataSourceUpdate where
  toJSON S3ReferenceDataSourceUpdate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ReferenceRoleARNUpdate" Prelude..=)
              Prelude.<$> referenceRoleARNUpdate,
            ("FileKeyUpdate" Prelude..=)
              Prelude.<$> fileKeyUpdate,
            ("BucketARNUpdate" Prelude..=)
              Prelude.<$> bucketARNUpdate
          ]
      )
