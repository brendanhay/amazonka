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
-- Module      : Amazonka.Transfer.Types.S3InputFileLocation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.S3InputFileLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the customer input S3 file location. If it is used inside
-- @copyStepDetails.DestinationFileLocation@, it should be the S3 copy
-- destination.
--
-- You need to provide the bucket and key. The key can represent either a
-- path or a file. This is determined by whether or not you end the key
-- value with the forward slash (\/) character. If the final character is
-- \"\/\", then your file is copied to the folder, and its name does not
-- change. If, rather, the final character is alphanumeric, your uploaded
-- file is renamed to the path value. In this case, if a file with that
-- name already exists, it is overwritten.
--
-- For example, if your path is @shared-files\/bob\/@, your uploaded files
-- are copied to the @shared-files\/bob\/@, folder. If your path is
-- @shared-files\/today@, each uploaded file is copied to the
-- @shared-files@ folder and named @today@: each upload overwrites the
-- previous version of the /bob/ file.
--
-- /See:/ 'newS3InputFileLocation' smart constructor.
data S3InputFileLocation = S3InputFileLocation'
  { -- | The name assigned to the file when it was created in Amazon S3. You use
    -- the object key to retrieve the object.
    key :: Prelude.Maybe Prelude.Text,
    -- | Specifies the S3 bucket for the customer input file.
    bucket :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3InputFileLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 's3InputFileLocation_key' - The name assigned to the file when it was created in Amazon S3. You use
-- the object key to retrieve the object.
--
-- 'bucket', 's3InputFileLocation_bucket' - Specifies the S3 bucket for the customer input file.
newS3InputFileLocation ::
  S3InputFileLocation
newS3InputFileLocation =
  S3InputFileLocation'
    { key = Prelude.Nothing,
      bucket = Prelude.Nothing
    }

-- | The name assigned to the file when it was created in Amazon S3. You use
-- the object key to retrieve the object.
s3InputFileLocation_key :: Lens.Lens' S3InputFileLocation (Prelude.Maybe Prelude.Text)
s3InputFileLocation_key = Lens.lens (\S3InputFileLocation' {key} -> key) (\s@S3InputFileLocation' {} a -> s {key = a} :: S3InputFileLocation)

-- | Specifies the S3 bucket for the customer input file.
s3InputFileLocation_bucket :: Lens.Lens' S3InputFileLocation (Prelude.Maybe Prelude.Text)
s3InputFileLocation_bucket = Lens.lens (\S3InputFileLocation' {bucket} -> bucket) (\s@S3InputFileLocation' {} a -> s {bucket = a} :: S3InputFileLocation)

instance Data.FromJSON S3InputFileLocation where
  parseJSON =
    Data.withObject
      "S3InputFileLocation"
      ( \x ->
          S3InputFileLocation'
            Prelude.<$> (x Data..:? "Key") Prelude.<*> (x Data..:? "Bucket")
      )

instance Prelude.Hashable S3InputFileLocation where
  hashWithSalt _salt S3InputFileLocation' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData S3InputFileLocation where
  rnf S3InputFileLocation' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf bucket

instance Data.ToJSON S3InputFileLocation where
  toJSON S3InputFileLocation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Key" Data..=) Prelude.<$> key,
            ("Bucket" Data..=) Prelude.<$> bucket
          ]
      )
