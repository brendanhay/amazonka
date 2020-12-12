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
    samArn,
    samUrl,
    samType,
  )
where

import Network.AWS.DeviceFarm.Types.SampleType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a sample of performance data.
--
-- /See:/ 'mkSample' smart constructor.
data Sample = Sample'
  { arn :: Lude.Maybe Lude.Text,
    url :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe SampleType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Sample' with the minimum fields required to make a request.
--
-- * 'arn' - The sample's ARN.
-- * 'type'' - The sample's type.
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
-- * 'url' - The presigned Amazon S3 URL that can be used with a GET request to download the sample's file.
mkSample ::
  Sample
mkSample =
  Sample'
    { arn = Lude.Nothing,
      url = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The sample's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samArn :: Lens.Lens' Sample (Lude.Maybe Lude.Text)
samArn = Lens.lens (arn :: Sample -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Sample)
{-# DEPRECATED samArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The presigned Amazon S3 URL that can be used with a GET request to download the sample's file.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samUrl :: Lens.Lens' Sample (Lude.Maybe Lude.Text)
samUrl = Lens.lens (url :: Sample -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: Sample)
{-# DEPRECATED samUrl "Use generic-lens or generic-optics with 'url' instead." #-}

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
samType :: Lens.Lens' Sample (Lude.Maybe SampleType)
samType = Lens.lens (type' :: Sample -> Lude.Maybe SampleType) (\s a -> s {type' = a} :: Sample)
{-# DEPRECATED samType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON Sample where
  parseJSON =
    Lude.withObject
      "Sample"
      ( \x ->
          Sample'
            Lude.<$> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "url")
            Lude.<*> (x Lude..:? "type")
      )
