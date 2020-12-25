{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.FileLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.FileLocation
  ( FileLocation (..),

    -- * Smart constructor
    mkFileLocation,

    -- * Lenses
    flS3Location,
    flStream,
  )
where

import qualified Network.AWS.IoT.Types.S3Location as Types
import qualified Network.AWS.IoT.Types.Stream as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The location of the OTA update.
--
-- /See:/ 'mkFileLocation' smart constructor.
data FileLocation = FileLocation'
  { -- | The location of the updated firmware in S3.
    s3Location :: Core.Maybe Types.S3Location,
    -- | The stream that contains the OTA update.
    stream :: Core.Maybe Types.Stream
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FileLocation' value with any optional fields omitted.
mkFileLocation ::
  FileLocation
mkFileLocation =
  FileLocation' {s3Location = Core.Nothing, stream = Core.Nothing}

-- | The location of the updated firmware in S3.
--
-- /Note:/ Consider using 's3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flS3Location :: Lens.Lens' FileLocation (Core.Maybe Types.S3Location)
flS3Location = Lens.field @"s3Location"
{-# DEPRECATED flS3Location "Use generic-lens or generic-optics with 's3Location' instead." #-}

-- | The stream that contains the OTA update.
--
-- /Note:/ Consider using 'stream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flStream :: Lens.Lens' FileLocation (Core.Maybe Types.Stream)
flStream = Lens.field @"stream"
{-# DEPRECATED flStream "Use generic-lens or generic-optics with 'stream' instead." #-}

instance Core.FromJSON FileLocation where
  toJSON FileLocation {..} =
    Core.object
      ( Core.catMaybes
          [ ("s3Location" Core..=) Core.<$> s3Location,
            ("stream" Core..=) Core.<$> stream
          ]
      )

instance Core.FromJSON FileLocation where
  parseJSON =
    Core.withObject "FileLocation" Core.$
      \x ->
        FileLocation'
          Core.<$> (x Core..:? "s3Location") Core.<*> (x Core..:? "stream")
