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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.FileLocation where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types.S3Location
import Amazonka.IoT.Types.Stream
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The location of the OTA update.
--
-- /See:/ 'newFileLocation' smart constructor.
data FileLocation = FileLocation'
  { -- | The stream that contains the OTA update.
    stream :: Prelude.Maybe Stream,
    -- | The location of the updated firmware in S3.
    s3Location :: Prelude.Maybe S3Location
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
-- 'stream', 'fileLocation_stream' - The stream that contains the OTA update.
--
-- 's3Location', 'fileLocation_s3Location' - The location of the updated firmware in S3.
newFileLocation ::
  FileLocation
newFileLocation =
  FileLocation'
    { stream = Prelude.Nothing,
      s3Location = Prelude.Nothing
    }

-- | The stream that contains the OTA update.
fileLocation_stream :: Lens.Lens' FileLocation (Prelude.Maybe Stream)
fileLocation_stream = Lens.lens (\FileLocation' {stream} -> stream) (\s@FileLocation' {} a -> s {stream = a} :: FileLocation)

-- | The location of the updated firmware in S3.
fileLocation_s3Location :: Lens.Lens' FileLocation (Prelude.Maybe S3Location)
fileLocation_s3Location = Lens.lens (\FileLocation' {s3Location} -> s3Location) (\s@FileLocation' {} a -> s {s3Location = a} :: FileLocation)

instance Core.FromJSON FileLocation where
  parseJSON =
    Core.withObject
      "FileLocation"
      ( \x ->
          FileLocation'
            Prelude.<$> (x Core..:? "stream")
            Prelude.<*> (x Core..:? "s3Location")
      )

instance Prelude.Hashable FileLocation

instance Prelude.NFData FileLocation

instance Core.ToJSON FileLocation where
  toJSON FileLocation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("stream" Core..=) Prelude.<$> stream,
            ("s3Location" Core..=) Prelude.<$> s3Location
          ]
      )
