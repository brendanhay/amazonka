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
-- Module      : Amazonka.SimSpaceWeaver.Types.S3Location
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SimSpaceWeaver.Types.S3Location where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A location in Amazon Simple Storage Service (Amazon S3) where SimSpace
-- Weaver stores simulation data, such as your app zip files and schema
-- file. For more information about Amazon S3, see the
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/Welcome.html Amazon Simple Storage Service User Guide>
-- .
--
-- /See:/ 'newS3Location' smart constructor.
data S3Location = S3Location'
  { -- | The name of an Amazon S3 bucket. For more information about buckets, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/creating-buckets-s3.html Creating, configuring, and working with Amazon S3 buckets>
    -- in the /Amazon Simple Storage Service User Guide/.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | The key name of an object in Amazon S3. For more information about
    -- Amazon S3 objects and object keys, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/uploading-downloading-objects.html Uploading, downloading, and working with objects in Amazon S3>
    -- in the /Amazon Simple Storage Service User Guide/.
    objectKey :: Prelude.Maybe Prelude.Text
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
-- 'bucketName', 's3Location_bucketName' - The name of an Amazon S3 bucket. For more information about buckets, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/creating-buckets-s3.html Creating, configuring, and working with Amazon S3 buckets>
-- in the /Amazon Simple Storage Service User Guide/.
--
-- 'objectKey', 's3Location_objectKey' - The key name of an object in Amazon S3. For more information about
-- Amazon S3 objects and object keys, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/uploading-downloading-objects.html Uploading, downloading, and working with objects in Amazon S3>
-- in the /Amazon Simple Storage Service User Guide/.
newS3Location ::
  S3Location
newS3Location =
  S3Location'
    { bucketName = Prelude.Nothing,
      objectKey = Prelude.Nothing
    }

-- | The name of an Amazon S3 bucket. For more information about buckets, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/creating-buckets-s3.html Creating, configuring, and working with Amazon S3 buckets>
-- in the /Amazon Simple Storage Service User Guide/.
s3Location_bucketName :: Lens.Lens' S3Location (Prelude.Maybe Prelude.Text)
s3Location_bucketName = Lens.lens (\S3Location' {bucketName} -> bucketName) (\s@S3Location' {} a -> s {bucketName = a} :: S3Location)

-- | The key name of an object in Amazon S3. For more information about
-- Amazon S3 objects and object keys, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/uploading-downloading-objects.html Uploading, downloading, and working with objects in Amazon S3>
-- in the /Amazon Simple Storage Service User Guide/.
s3Location_objectKey :: Lens.Lens' S3Location (Prelude.Maybe Prelude.Text)
s3Location_objectKey = Lens.lens (\S3Location' {objectKey} -> objectKey) (\s@S3Location' {} a -> s {objectKey = a} :: S3Location)

instance Data.FromJSON S3Location where
  parseJSON =
    Data.withObject
      "S3Location"
      ( \x ->
          S3Location'
            Prelude.<$> (x Data..:? "BucketName")
            Prelude.<*> (x Data..:? "ObjectKey")
      )

instance Prelude.Hashable S3Location where
  hashWithSalt _salt S3Location' {..} =
    _salt
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` objectKey

instance Prelude.NFData S3Location where
  rnf S3Location' {..} =
    Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf objectKey

instance Data.ToJSON S3Location where
  toJSON S3Location' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BucketName" Data..=) Prelude.<$> bucketName,
            ("ObjectKey" Data..=) Prelude.<$> objectKey
          ]
      )
