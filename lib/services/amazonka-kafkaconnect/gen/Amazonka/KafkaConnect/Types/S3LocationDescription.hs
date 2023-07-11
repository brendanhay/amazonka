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
-- Module      : Amazonka.KafkaConnect.Types.S3LocationDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.S3LocationDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The description of the location of an object in Amazon S3.
--
-- /See:/ 'newS3LocationDescription' smart constructor.
data S3LocationDescription = S3LocationDescription'
  { -- | The Amazon Resource Name (ARN) of an S3 bucket.
    bucketArn :: Prelude.Maybe Prelude.Text,
    -- | The file key for an object in an S3 bucket.
    fileKey :: Prelude.Maybe Prelude.Text,
    -- | The version of an object in an S3 bucket.
    objectVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3LocationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketArn', 's3LocationDescription_bucketArn' - The Amazon Resource Name (ARN) of an S3 bucket.
--
-- 'fileKey', 's3LocationDescription_fileKey' - The file key for an object in an S3 bucket.
--
-- 'objectVersion', 's3LocationDescription_objectVersion' - The version of an object in an S3 bucket.
newS3LocationDescription ::
  S3LocationDescription
newS3LocationDescription =
  S3LocationDescription'
    { bucketArn = Prelude.Nothing,
      fileKey = Prelude.Nothing,
      objectVersion = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of an S3 bucket.
s3LocationDescription_bucketArn :: Lens.Lens' S3LocationDescription (Prelude.Maybe Prelude.Text)
s3LocationDescription_bucketArn = Lens.lens (\S3LocationDescription' {bucketArn} -> bucketArn) (\s@S3LocationDescription' {} a -> s {bucketArn = a} :: S3LocationDescription)

-- | The file key for an object in an S3 bucket.
s3LocationDescription_fileKey :: Lens.Lens' S3LocationDescription (Prelude.Maybe Prelude.Text)
s3LocationDescription_fileKey = Lens.lens (\S3LocationDescription' {fileKey} -> fileKey) (\s@S3LocationDescription' {} a -> s {fileKey = a} :: S3LocationDescription)

-- | The version of an object in an S3 bucket.
s3LocationDescription_objectVersion :: Lens.Lens' S3LocationDescription (Prelude.Maybe Prelude.Text)
s3LocationDescription_objectVersion = Lens.lens (\S3LocationDescription' {objectVersion} -> objectVersion) (\s@S3LocationDescription' {} a -> s {objectVersion = a} :: S3LocationDescription)

instance Data.FromJSON S3LocationDescription where
  parseJSON =
    Data.withObject
      "S3LocationDescription"
      ( \x ->
          S3LocationDescription'
            Prelude.<$> (x Data..:? "bucketArn")
            Prelude.<*> (x Data..:? "fileKey")
            Prelude.<*> (x Data..:? "objectVersion")
      )

instance Prelude.Hashable S3LocationDescription where
  hashWithSalt _salt S3LocationDescription' {..} =
    _salt
      `Prelude.hashWithSalt` bucketArn
      `Prelude.hashWithSalt` fileKey
      `Prelude.hashWithSalt` objectVersion

instance Prelude.NFData S3LocationDescription where
  rnf S3LocationDescription' {..} =
    Prelude.rnf bucketArn
      `Prelude.seq` Prelude.rnf fileKey
      `Prelude.seq` Prelude.rnf objectVersion
