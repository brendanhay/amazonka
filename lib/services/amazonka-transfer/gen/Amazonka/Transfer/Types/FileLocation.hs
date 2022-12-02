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
-- Module      : Amazonka.Transfer.Types.FileLocation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.FileLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.Types.EfsFileLocation
import Amazonka.Transfer.Types.S3FileLocation

-- | Specifies the Amazon S3 or EFS file details to be used in the step.
--
-- /See:/ 'newFileLocation' smart constructor.
data FileLocation = FileLocation'
  { -- | Specifies the S3 details for the file being used, such as bucket, ETag,
    -- and so forth.
    s3FileLocation :: Prelude.Maybe S3FileLocation,
    -- | Specifies the Amazon EFS identifier and the path for the file being
    -- used.
    efsFileLocation :: Prelude.Maybe EfsFileLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3FileLocation', 'fileLocation_s3FileLocation' - Specifies the S3 details for the file being used, such as bucket, ETag,
-- and so forth.
--
-- 'efsFileLocation', 'fileLocation_efsFileLocation' - Specifies the Amazon EFS identifier and the path for the file being
-- used.
newFileLocation ::
  FileLocation
newFileLocation =
  FileLocation'
    { s3FileLocation = Prelude.Nothing,
      efsFileLocation = Prelude.Nothing
    }

-- | Specifies the S3 details for the file being used, such as bucket, ETag,
-- and so forth.
fileLocation_s3FileLocation :: Lens.Lens' FileLocation (Prelude.Maybe S3FileLocation)
fileLocation_s3FileLocation = Lens.lens (\FileLocation' {s3FileLocation} -> s3FileLocation) (\s@FileLocation' {} a -> s {s3FileLocation = a} :: FileLocation)

-- | Specifies the Amazon EFS identifier and the path for the file being
-- used.
fileLocation_efsFileLocation :: Lens.Lens' FileLocation (Prelude.Maybe EfsFileLocation)
fileLocation_efsFileLocation = Lens.lens (\FileLocation' {efsFileLocation} -> efsFileLocation) (\s@FileLocation' {} a -> s {efsFileLocation = a} :: FileLocation)

instance Data.FromJSON FileLocation where
  parseJSON =
    Data.withObject
      "FileLocation"
      ( \x ->
          FileLocation'
            Prelude.<$> (x Data..:? "S3FileLocation")
            Prelude.<*> (x Data..:? "EfsFileLocation")
      )

instance Prelude.Hashable FileLocation where
  hashWithSalt _salt FileLocation' {..} =
    _salt `Prelude.hashWithSalt` s3FileLocation
      `Prelude.hashWithSalt` efsFileLocation

instance Prelude.NFData FileLocation where
  rnf FileLocation' {..} =
    Prelude.rnf s3FileLocation
      `Prelude.seq` Prelude.rnf efsFileLocation
