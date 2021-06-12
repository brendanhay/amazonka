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
-- Module      : Network.AWS.IoT.Types.FileLocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.FileLocation where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.S3Location
import Network.AWS.IoT.Types.Stream
import qualified Network.AWS.Lens as Lens

-- | The location of the OTA update.
--
-- /See:/ 'newFileLocation' smart constructor.
data FileLocation = FileLocation'
  { -- | The stream that contains the OTA update.
    stream :: Core.Maybe Stream,
    -- | The location of the updated firmware in S3.
    s3Location :: Core.Maybe S3Location
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { stream = Core.Nothing,
      s3Location = Core.Nothing
    }

-- | The stream that contains the OTA update.
fileLocation_stream :: Lens.Lens' FileLocation (Core.Maybe Stream)
fileLocation_stream = Lens.lens (\FileLocation' {stream} -> stream) (\s@FileLocation' {} a -> s {stream = a} :: FileLocation)

-- | The location of the updated firmware in S3.
fileLocation_s3Location :: Lens.Lens' FileLocation (Core.Maybe S3Location)
fileLocation_s3Location = Lens.lens (\FileLocation' {s3Location} -> s3Location) (\s@FileLocation' {} a -> s {s3Location = a} :: FileLocation)

instance Core.FromJSON FileLocation where
  parseJSON =
    Core.withObject
      "FileLocation"
      ( \x ->
          FileLocation'
            Core.<$> (x Core..:? "stream")
            Core.<*> (x Core..:? "s3Location")
      )

instance Core.Hashable FileLocation

instance Core.NFData FileLocation

instance Core.ToJSON FileLocation where
  toJSON FileLocation' {..} =
    Core.object
      ( Core.catMaybes
          [ ("stream" Core..=) Core.<$> stream,
            ("s3Location" Core..=) Core.<$> s3Location
          ]
      )
