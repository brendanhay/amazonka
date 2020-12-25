{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Sample
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Sample
  ( Sample (..),

    -- * Smart constructor
    mkSample,

    -- * Lenses
    sfArn,
    sfType,
    sfUrl,
  )
where

import qualified Network.AWS.DeviceFarm.Types.AmazonResourceName as Types
import qualified Network.AWS.DeviceFarm.Types.SampleType as Types
import qualified Network.AWS.DeviceFarm.Types.URL as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a sample of performance data.
--
-- /See:/ 'mkSample' smart constructor.
data Sample = Sample'
  { -- | The sample's ARN.
    arn :: Core.Maybe Types.AmazonResourceName,
    -- | The sample's type.
    --
    -- Must be one of the following values:
    --
    --     * CPU: A CPU sample type. This is expressed as the app processing CPU time (including child processes) as reported by process, as a percentage.
    --
    --
    --     * MEMORY: A memory usage sample type. This is expressed as the total proportional set size of an app process, in kilobytes.
    --
    --
    --     * NATIVE_AVG_DRAWTIME
    --
    --
    --     * NATIVE_FPS
    --
    --
    --     * NATIVE_FRAMES
    --
    --
    --     * NATIVE_MAX_DRAWTIME
    --
    --
    --     * NATIVE_MIN_DRAWTIME
    --
    --
    --     * OPENGL_AVG_DRAWTIME
    --
    --
    --     * OPENGL_FPS
    --
    --
    --     * OPENGL_FRAMES
    --
    --
    --     * OPENGL_MAX_DRAWTIME
    --
    --
    --     * OPENGL_MIN_DRAWTIME
    --
    --
    --     * RX
    --
    --
    --     * RX_RATE: The total number of bytes per second (TCP and UDP) that are sent, by app process.
    --
    --
    --     * THREADS: A threads sample type. This is expressed as the total number of threads per app process.
    --
    --
    --     * TX
    --
    --
    --     * TX_RATE: The total number of bytes per second (TCP and UDP) that are received, by app process.
    type' :: Core.Maybe Types.SampleType,
    -- | The presigned Amazon S3 URL that can be used with a GET request to download the sample's file.
    url :: Core.Maybe Types.URL
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Sample' value with any optional fields omitted.
mkSample ::
  Sample
mkSample =
  Sample'
    { arn = Core.Nothing,
      type' = Core.Nothing,
      url = Core.Nothing
    }

-- | The sample's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfArn :: Lens.Lens' Sample (Core.Maybe Types.AmazonResourceName)
sfArn = Lens.field @"arn"
{-# DEPRECATED sfArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The sample's type.
--
-- Must be one of the following values:
--
--     * CPU: A CPU sample type. This is expressed as the app processing CPU time (including child processes) as reported by process, as a percentage.
--
--
--     * MEMORY: A memory usage sample type. This is expressed as the total proportional set size of an app process, in kilobytes.
--
--
--     * NATIVE_AVG_DRAWTIME
--
--
--     * NATIVE_FPS
--
--
--     * NATIVE_FRAMES
--
--
--     * NATIVE_MAX_DRAWTIME
--
--
--     * NATIVE_MIN_DRAWTIME
--
--
--     * OPENGL_AVG_DRAWTIME
--
--
--     * OPENGL_FPS
--
--
--     * OPENGL_FRAMES
--
--
--     * OPENGL_MAX_DRAWTIME
--
--
--     * OPENGL_MIN_DRAWTIME
--
--
--     * RX
--
--
--     * RX_RATE: The total number of bytes per second (TCP and UDP) that are sent, by app process.
--
--
--     * THREADS: A threads sample type. This is expressed as the total number of threads per app process.
--
--
--     * TX
--
--
--     * TX_RATE: The total number of bytes per second (TCP and UDP) that are received, by app process.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfType :: Lens.Lens' Sample (Core.Maybe Types.SampleType)
sfType = Lens.field @"type'"
{-# DEPRECATED sfType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The presigned Amazon S3 URL that can be used with a GET request to download the sample's file.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfUrl :: Lens.Lens' Sample (Core.Maybe Types.URL)
sfUrl = Lens.field @"url"
{-# DEPRECATED sfUrl "Use generic-lens or generic-optics with 'url' instead." #-}

instance Core.FromJSON Sample where
  parseJSON =
    Core.withObject "Sample" Core.$
      \x ->
        Sample'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "type")
          Core.<*> (x Core..:? "url")
