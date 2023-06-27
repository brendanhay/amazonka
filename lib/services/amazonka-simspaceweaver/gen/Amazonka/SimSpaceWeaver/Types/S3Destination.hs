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
-- Module      : Amazonka.SimSpaceWeaver.Types.S3Destination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SimSpaceWeaver.Types.S3Destination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An Amazon S3 bucket and optional folder (object key prefix) where
-- SimSpace Weaver creates a file.
--
-- /See:/ 'newS3Destination' smart constructor.
data S3Destination = S3Destination'
  { -- | The name of an Amazon S3 bucket. For more information about buckets, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/creating-buckets-s3.html Creating, configuring, and working with Amazon S3 buckets>
    -- in the /Amazon Simple Storage Service User Guide/.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | A string prefix for an Amazon S3 object key. It\'s usually a folder
    -- name. For more information about folders in Amazon S3, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-folders.html Organizing objects in the Amazon S3 console using folders>
    -- in the /Amazon Simple Storage Service User Guide/.
    objectKeyPrefix :: Prelude.Maybe Prelude.Text
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
-- 'bucketName', 's3Destination_bucketName' - The name of an Amazon S3 bucket. For more information about buckets, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/creating-buckets-s3.html Creating, configuring, and working with Amazon S3 buckets>
-- in the /Amazon Simple Storage Service User Guide/.
--
-- 'objectKeyPrefix', 's3Destination_objectKeyPrefix' - A string prefix for an Amazon S3 object key. It\'s usually a folder
-- name. For more information about folders in Amazon S3, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-folders.html Organizing objects in the Amazon S3 console using folders>
-- in the /Amazon Simple Storage Service User Guide/.
newS3Destination ::
  S3Destination
newS3Destination =
  S3Destination'
    { bucketName = Prelude.Nothing,
      objectKeyPrefix = Prelude.Nothing
    }

-- | The name of an Amazon S3 bucket. For more information about buckets, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/creating-buckets-s3.html Creating, configuring, and working with Amazon S3 buckets>
-- in the /Amazon Simple Storage Service User Guide/.
s3Destination_bucketName :: Lens.Lens' S3Destination (Prelude.Maybe Prelude.Text)
s3Destination_bucketName = Lens.lens (\S3Destination' {bucketName} -> bucketName) (\s@S3Destination' {} a -> s {bucketName = a} :: S3Destination)

-- | A string prefix for an Amazon S3 object key. It\'s usually a folder
-- name. For more information about folders in Amazon S3, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-folders.html Organizing objects in the Amazon S3 console using folders>
-- in the /Amazon Simple Storage Service User Guide/.
s3Destination_objectKeyPrefix :: Lens.Lens' S3Destination (Prelude.Maybe Prelude.Text)
s3Destination_objectKeyPrefix = Lens.lens (\S3Destination' {objectKeyPrefix} -> objectKeyPrefix) (\s@S3Destination' {} a -> s {objectKeyPrefix = a} :: S3Destination)

instance Prelude.Hashable S3Destination where
  hashWithSalt _salt S3Destination' {..} =
    _salt
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` objectKeyPrefix

instance Prelude.NFData S3Destination where
  rnf S3Destination' {..} =
    Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf objectKeyPrefix

instance Data.ToJSON S3Destination where
  toJSON S3Destination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BucketName" Data..=) Prelude.<$> bucketName,
            ("ObjectKeyPrefix" Data..=)
              Prelude.<$> objectKeyPrefix
          ]
      )
