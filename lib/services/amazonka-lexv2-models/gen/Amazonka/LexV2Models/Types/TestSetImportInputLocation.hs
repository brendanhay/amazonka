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
-- Module      : Amazonka.LexV2Models.Types.TestSetImportInputLocation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.TestSetImportInputLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the Amazon S3 location from which the test
-- set is imported.
--
-- /See:/ 'newTestSetImportInputLocation' smart constructor.
data TestSetImportInputLocation = TestSetImportInputLocation'
  { -- | The name of the Amazon S3 bucket.
    s3BucketName :: Prelude.Text,
    -- | The path inside the Amazon S3 bucket pointing to the test-set CSV file.
    s3Path :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestSetImportInputLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3BucketName', 'testSetImportInputLocation_s3BucketName' - The name of the Amazon S3 bucket.
--
-- 's3Path', 'testSetImportInputLocation_s3Path' - The path inside the Amazon S3 bucket pointing to the test-set CSV file.
newTestSetImportInputLocation ::
  -- | 's3BucketName'
  Prelude.Text ->
  -- | 's3Path'
  Prelude.Text ->
  TestSetImportInputLocation
newTestSetImportInputLocation pS3BucketName_ pS3Path_ =
  TestSetImportInputLocation'
    { s3BucketName =
        pS3BucketName_,
      s3Path = pS3Path_
    }

-- | The name of the Amazon S3 bucket.
testSetImportInputLocation_s3BucketName :: Lens.Lens' TestSetImportInputLocation Prelude.Text
testSetImportInputLocation_s3BucketName = Lens.lens (\TestSetImportInputLocation' {s3BucketName} -> s3BucketName) (\s@TestSetImportInputLocation' {} a -> s {s3BucketName = a} :: TestSetImportInputLocation)

-- | The path inside the Amazon S3 bucket pointing to the test-set CSV file.
testSetImportInputLocation_s3Path :: Lens.Lens' TestSetImportInputLocation Prelude.Text
testSetImportInputLocation_s3Path = Lens.lens (\TestSetImportInputLocation' {s3Path} -> s3Path) (\s@TestSetImportInputLocation' {} a -> s {s3Path = a} :: TestSetImportInputLocation)

instance Data.FromJSON TestSetImportInputLocation where
  parseJSON =
    Data.withObject
      "TestSetImportInputLocation"
      ( \x ->
          TestSetImportInputLocation'
            Prelude.<$> (x Data..: "s3BucketName")
            Prelude.<*> (x Data..: "s3Path")
      )

instance Prelude.Hashable TestSetImportInputLocation where
  hashWithSalt _salt TestSetImportInputLocation' {..} =
    _salt
      `Prelude.hashWithSalt` s3BucketName
      `Prelude.hashWithSalt` s3Path

instance Prelude.NFData TestSetImportInputLocation where
  rnf TestSetImportInputLocation' {..} =
    Prelude.rnf s3BucketName
      `Prelude.seq` Prelude.rnf s3Path

instance Data.ToJSON TestSetImportInputLocation where
  toJSON TestSetImportInputLocation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("s3BucketName" Data..= s3BucketName),
            Prelude.Just ("s3Path" Data..= s3Path)
          ]
      )
