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
-- Module      : Amazonka.TimeStreamWrite.Types.MagneticStoreRejectedDataLocation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.Types.MagneticStoreRejectedDataLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamWrite.Types.S3Configuration

-- | The location to write error reports for records rejected,
-- asynchronously, during magnetic store writes.
--
-- /See:/ 'newMagneticStoreRejectedDataLocation' smart constructor.
data MagneticStoreRejectedDataLocation = MagneticStoreRejectedDataLocation'
  { -- | Configuration of an S3 location to write error reports for records
    -- rejected, asynchronously, during magnetic store writes.
    s3Configuration :: Prelude.Maybe S3Configuration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MagneticStoreRejectedDataLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Configuration', 'magneticStoreRejectedDataLocation_s3Configuration' - Configuration of an S3 location to write error reports for records
-- rejected, asynchronously, during magnetic store writes.
newMagneticStoreRejectedDataLocation ::
  MagneticStoreRejectedDataLocation
newMagneticStoreRejectedDataLocation =
  MagneticStoreRejectedDataLocation'
    { s3Configuration =
        Prelude.Nothing
    }

-- | Configuration of an S3 location to write error reports for records
-- rejected, asynchronously, during magnetic store writes.
magneticStoreRejectedDataLocation_s3Configuration :: Lens.Lens' MagneticStoreRejectedDataLocation (Prelude.Maybe S3Configuration)
magneticStoreRejectedDataLocation_s3Configuration = Lens.lens (\MagneticStoreRejectedDataLocation' {s3Configuration} -> s3Configuration) (\s@MagneticStoreRejectedDataLocation' {} a -> s {s3Configuration = a} :: MagneticStoreRejectedDataLocation)

instance
  Data.FromJSON
    MagneticStoreRejectedDataLocation
  where
  parseJSON =
    Data.withObject
      "MagneticStoreRejectedDataLocation"
      ( \x ->
          MagneticStoreRejectedDataLocation'
            Prelude.<$> (x Data..:? "S3Configuration")
      )

instance
  Prelude.Hashable
    MagneticStoreRejectedDataLocation
  where
  hashWithSalt
    _salt
    MagneticStoreRejectedDataLocation' {..} =
      _salt `Prelude.hashWithSalt` s3Configuration

instance
  Prelude.NFData
    MagneticStoreRejectedDataLocation
  where
  rnf MagneticStoreRejectedDataLocation' {..} =
    Prelude.rnf s3Configuration

instance
  Data.ToJSON
    MagneticStoreRejectedDataLocation
  where
  toJSON MagneticStoreRejectedDataLocation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("S3Configuration" Data..=)
              Prelude.<$> s3Configuration
          ]
      )
