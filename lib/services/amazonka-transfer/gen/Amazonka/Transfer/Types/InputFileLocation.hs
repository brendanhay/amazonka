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
-- Module      : Amazonka.Transfer.Types.InputFileLocation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.InputFileLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.Types.EfsFileLocation
import Amazonka.Transfer.Types.S3InputFileLocation

-- | Specifies the location for the file being copied. Only applicable for
-- the Copy type of workflow steps.
--
-- /See:/ 'newInputFileLocation' smart constructor.
data InputFileLocation = InputFileLocation'
  { -- | Specifies the details for the S3 file being copied.
    s3FileLocation :: Prelude.Maybe S3InputFileLocation,
    -- | Reserved for future use.
    efsFileLocation :: Prelude.Maybe EfsFileLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputFileLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3FileLocation', 'inputFileLocation_s3FileLocation' - Specifies the details for the S3 file being copied.
--
-- 'efsFileLocation', 'inputFileLocation_efsFileLocation' - Reserved for future use.
newInputFileLocation ::
  InputFileLocation
newInputFileLocation =
  InputFileLocation'
    { s3FileLocation =
        Prelude.Nothing,
      efsFileLocation = Prelude.Nothing
    }

-- | Specifies the details for the S3 file being copied.
inputFileLocation_s3FileLocation :: Lens.Lens' InputFileLocation (Prelude.Maybe S3InputFileLocation)
inputFileLocation_s3FileLocation = Lens.lens (\InputFileLocation' {s3FileLocation} -> s3FileLocation) (\s@InputFileLocation' {} a -> s {s3FileLocation = a} :: InputFileLocation)

-- | Reserved for future use.
inputFileLocation_efsFileLocation :: Lens.Lens' InputFileLocation (Prelude.Maybe EfsFileLocation)
inputFileLocation_efsFileLocation = Lens.lens (\InputFileLocation' {efsFileLocation} -> efsFileLocation) (\s@InputFileLocation' {} a -> s {efsFileLocation = a} :: InputFileLocation)

instance Data.FromJSON InputFileLocation where
  parseJSON =
    Data.withObject
      "InputFileLocation"
      ( \x ->
          InputFileLocation'
            Prelude.<$> (x Data..:? "S3FileLocation")
            Prelude.<*> (x Data..:? "EfsFileLocation")
      )

instance Prelude.Hashable InputFileLocation where
  hashWithSalt _salt InputFileLocation' {..} =
    _salt `Prelude.hashWithSalt` s3FileLocation
      `Prelude.hashWithSalt` efsFileLocation

instance Prelude.NFData InputFileLocation where
  rnf InputFileLocation' {..} =
    Prelude.rnf s3FileLocation
      `Prelude.seq` Prelude.rnf efsFileLocation

instance Data.ToJSON InputFileLocation where
  toJSON InputFileLocation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("S3FileLocation" Data..=)
              Prelude.<$> s3FileLocation,
            ("EfsFileLocation" Data..=)
              Prelude.<$> efsFileLocation
          ]
      )
