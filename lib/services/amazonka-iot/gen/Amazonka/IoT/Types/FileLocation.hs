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
-- Module      : Amazonka.IoT.Types.FileLocation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.FileLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.S3Location
import Amazonka.IoT.Types.Stream
import qualified Amazonka.Prelude as Prelude

-- | The location of the OTA update.
--
-- /See:/ 'newFileLocation' smart constructor.
data FileLocation = FileLocation'
  { -- | The location of the updated firmware in S3.
    s3Location :: Prelude.Maybe S3Location,
    -- | The stream that contains the OTA update.
    stream :: Prelude.Maybe Stream
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
-- 's3Location', 'fileLocation_s3Location' - The location of the updated firmware in S3.
--
-- 'stream', 'fileLocation_stream' - The stream that contains the OTA update.
newFileLocation ::
  FileLocation
newFileLocation =
  FileLocation'
    { s3Location = Prelude.Nothing,
      stream = Prelude.Nothing
    }

-- | The location of the updated firmware in S3.
fileLocation_s3Location :: Lens.Lens' FileLocation (Prelude.Maybe S3Location)
fileLocation_s3Location = Lens.lens (\FileLocation' {s3Location} -> s3Location) (\s@FileLocation' {} a -> s {s3Location = a} :: FileLocation)

-- | The stream that contains the OTA update.
fileLocation_stream :: Lens.Lens' FileLocation (Prelude.Maybe Stream)
fileLocation_stream = Lens.lens (\FileLocation' {stream} -> stream) (\s@FileLocation' {} a -> s {stream = a} :: FileLocation)

instance Data.FromJSON FileLocation where
  parseJSON =
    Data.withObject
      "FileLocation"
      ( \x ->
          FileLocation'
            Prelude.<$> (x Data..:? "s3Location")
            Prelude.<*> (x Data..:? "stream")
      )

instance Prelude.Hashable FileLocation where
  hashWithSalt _salt FileLocation' {..} =
    _salt
      `Prelude.hashWithSalt` s3Location
      `Prelude.hashWithSalt` stream

instance Prelude.NFData FileLocation where
  rnf FileLocation' {..} =
    Prelude.rnf s3Location
      `Prelude.seq` Prelude.rnf stream

instance Data.ToJSON FileLocation where
  toJSON FileLocation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("s3Location" Data..=) Prelude.<$> s3Location,
            ("stream" Data..=) Prelude.<$> stream
          ]
      )
