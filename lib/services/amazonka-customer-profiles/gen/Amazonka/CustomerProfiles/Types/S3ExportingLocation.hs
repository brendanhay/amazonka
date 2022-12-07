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
-- Module      : Amazonka.CustomerProfiles.Types.S3ExportingLocation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.S3ExportingLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The S3 location where Identity Resolution Jobs write result files.
--
-- /See:/ 'newS3ExportingLocation' smart constructor.
data S3ExportingLocation = S3ExportingLocation'
  { -- | The name of the S3 bucket name where Identity Resolution Jobs write
    -- result files.
    s3BucketName :: Prelude.Maybe Prelude.Text,
    -- | The S3 key name of the location where Identity Resolution Jobs write
    -- result files.
    s3KeyName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3ExportingLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3BucketName', 's3ExportingLocation_s3BucketName' - The name of the S3 bucket name where Identity Resolution Jobs write
-- result files.
--
-- 's3KeyName', 's3ExportingLocation_s3KeyName' - The S3 key name of the location where Identity Resolution Jobs write
-- result files.
newS3ExportingLocation ::
  S3ExportingLocation
newS3ExportingLocation =
  S3ExportingLocation'
    { s3BucketName =
        Prelude.Nothing,
      s3KeyName = Prelude.Nothing
    }

-- | The name of the S3 bucket name where Identity Resolution Jobs write
-- result files.
s3ExportingLocation_s3BucketName :: Lens.Lens' S3ExportingLocation (Prelude.Maybe Prelude.Text)
s3ExportingLocation_s3BucketName = Lens.lens (\S3ExportingLocation' {s3BucketName} -> s3BucketName) (\s@S3ExportingLocation' {} a -> s {s3BucketName = a} :: S3ExportingLocation)

-- | The S3 key name of the location where Identity Resolution Jobs write
-- result files.
s3ExportingLocation_s3KeyName :: Lens.Lens' S3ExportingLocation (Prelude.Maybe Prelude.Text)
s3ExportingLocation_s3KeyName = Lens.lens (\S3ExportingLocation' {s3KeyName} -> s3KeyName) (\s@S3ExportingLocation' {} a -> s {s3KeyName = a} :: S3ExportingLocation)

instance Data.FromJSON S3ExportingLocation where
  parseJSON =
    Data.withObject
      "S3ExportingLocation"
      ( \x ->
          S3ExportingLocation'
            Prelude.<$> (x Data..:? "S3BucketName")
            Prelude.<*> (x Data..:? "S3KeyName")
      )

instance Prelude.Hashable S3ExportingLocation where
  hashWithSalt _salt S3ExportingLocation' {..} =
    _salt `Prelude.hashWithSalt` s3BucketName
      `Prelude.hashWithSalt` s3KeyName

instance Prelude.NFData S3ExportingLocation where
  rnf S3ExportingLocation' {..} =
    Prelude.rnf s3BucketName
      `Prelude.seq` Prelude.rnf s3KeyName
