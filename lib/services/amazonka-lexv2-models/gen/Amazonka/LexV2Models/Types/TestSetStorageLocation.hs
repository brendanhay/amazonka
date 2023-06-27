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
-- Module      : Amazonka.LexV2Models.Types.TestSetStorageLocation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.TestSetStorageLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the location in which the test set is stored.
--
-- /See:/ 'newTestSetStorageLocation' smart constructor.
data TestSetStorageLocation = TestSetStorageLocation'
  { -- | The Amazon Resource Name (ARN) of an Amazon Web Services Key Management
    -- Service (KMS) key for encrypting the test set.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon S3 bucket in which the test set is stored.
    s3BucketName :: Prelude.Text,
    -- | The path inside the Amazon S3 bucket where the test set is stored.
    s3Path :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestSetStorageLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyArn', 'testSetStorageLocation_kmsKeyArn' - The Amazon Resource Name (ARN) of an Amazon Web Services Key Management
-- Service (KMS) key for encrypting the test set.
--
-- 's3BucketName', 'testSetStorageLocation_s3BucketName' - The name of the Amazon S3 bucket in which the test set is stored.
--
-- 's3Path', 'testSetStorageLocation_s3Path' - The path inside the Amazon S3 bucket where the test set is stored.
newTestSetStorageLocation ::
  -- | 's3BucketName'
  Prelude.Text ->
  -- | 's3Path'
  Prelude.Text ->
  TestSetStorageLocation
newTestSetStorageLocation pS3BucketName_ pS3Path_ =
  TestSetStorageLocation'
    { kmsKeyArn =
        Prelude.Nothing,
      s3BucketName = pS3BucketName_,
      s3Path = pS3Path_
    }

-- | The Amazon Resource Name (ARN) of an Amazon Web Services Key Management
-- Service (KMS) key for encrypting the test set.
testSetStorageLocation_kmsKeyArn :: Lens.Lens' TestSetStorageLocation (Prelude.Maybe Prelude.Text)
testSetStorageLocation_kmsKeyArn = Lens.lens (\TestSetStorageLocation' {kmsKeyArn} -> kmsKeyArn) (\s@TestSetStorageLocation' {} a -> s {kmsKeyArn = a} :: TestSetStorageLocation)

-- | The name of the Amazon S3 bucket in which the test set is stored.
testSetStorageLocation_s3BucketName :: Lens.Lens' TestSetStorageLocation Prelude.Text
testSetStorageLocation_s3BucketName = Lens.lens (\TestSetStorageLocation' {s3BucketName} -> s3BucketName) (\s@TestSetStorageLocation' {} a -> s {s3BucketName = a} :: TestSetStorageLocation)

-- | The path inside the Amazon S3 bucket where the test set is stored.
testSetStorageLocation_s3Path :: Lens.Lens' TestSetStorageLocation Prelude.Text
testSetStorageLocation_s3Path = Lens.lens (\TestSetStorageLocation' {s3Path} -> s3Path) (\s@TestSetStorageLocation' {} a -> s {s3Path = a} :: TestSetStorageLocation)

instance Data.FromJSON TestSetStorageLocation where
  parseJSON =
    Data.withObject
      "TestSetStorageLocation"
      ( \x ->
          TestSetStorageLocation'
            Prelude.<$> (x Data..:? "kmsKeyArn")
            Prelude.<*> (x Data..: "s3BucketName")
            Prelude.<*> (x Data..: "s3Path")
      )

instance Prelude.Hashable TestSetStorageLocation where
  hashWithSalt _salt TestSetStorageLocation' {..} =
    _salt
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` s3BucketName
      `Prelude.hashWithSalt` s3Path

instance Prelude.NFData TestSetStorageLocation where
  rnf TestSetStorageLocation' {..} =
    Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf s3BucketName
      `Prelude.seq` Prelude.rnf s3Path

instance Data.ToJSON TestSetStorageLocation where
  toJSON TestSetStorageLocation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("kmsKeyArn" Data..=) Prelude.<$> kmsKeyArn,
            Prelude.Just ("s3BucketName" Data..= s3BucketName),
            Prelude.Just ("s3Path" Data..= s3Path)
          ]
      )
