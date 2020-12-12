{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AutomatedAbrSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AutomatedAbrSettings
  ( AutomatedAbrSettings (..),

    -- * Smart constructor
    mkAutomatedAbrSettings,

    -- * Lenses
    aasMaxRenditions,
    aasMaxAbrBitrate,
    aasMinAbrBitrate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Use automated ABR to have MediaConvert set up the renditions in your ABR package for you automatically, based on characteristics of your input video. This feature optimizes video quality while minimizing the overall size of your ABR package.
--
-- /See:/ 'mkAutomatedAbrSettings' smart constructor.
data AutomatedAbrSettings = AutomatedAbrSettings'
  { maxRenditions ::
      Lude.Maybe Lude.Natural,
    maxAbrBitrate :: Lude.Maybe Lude.Natural,
    minAbrBitrate :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutomatedAbrSettings' with the minimum fields required to make a request.
--
-- * 'maxAbrBitrate' - Optional. The maximum target bit rate used in your automated ABR stack. Use this value to set an upper limit on the bandwidth consumed by the highest-quality rendition. This is the rendition that is delivered to viewers with the fastest internet connections. If you don't specify a value, MediaConvert uses 8,000,000 (8 mb/s) by default.
-- * 'maxRenditions' - Optional. The maximum number of renditions that MediaConvert will create in your automated ABR stack. The number of renditions is determined automatically, based on analysis of each job, but will never exceed this limit. When you set this to Auto in the console, which is equivalent to excluding it from your JSON job specification, MediaConvert defaults to a limit of 15.
-- * 'minAbrBitrate' - Optional. The minimum target bitrate used in your automated ABR stack. Use this value to set a lower limit on the bitrate of video delivered to viewers with slow internet connections. If you don't specify a value, MediaConvert uses 600,000 (600 kb/s) by default.
mkAutomatedAbrSettings ::
  AutomatedAbrSettings
mkAutomatedAbrSettings =
  AutomatedAbrSettings'
    { maxRenditions = Lude.Nothing,
      maxAbrBitrate = Lude.Nothing,
      minAbrBitrate = Lude.Nothing
    }

-- | Optional. The maximum number of renditions that MediaConvert will create in your automated ABR stack. The number of renditions is determined automatically, based on analysis of each job, but will never exceed this limit. When you set this to Auto in the console, which is equivalent to excluding it from your JSON job specification, MediaConvert defaults to a limit of 15.
--
-- /Note:/ Consider using 'maxRenditions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aasMaxRenditions :: Lens.Lens' AutomatedAbrSettings (Lude.Maybe Lude.Natural)
aasMaxRenditions = Lens.lens (maxRenditions :: AutomatedAbrSettings -> Lude.Maybe Lude.Natural) (\s a -> s {maxRenditions = a} :: AutomatedAbrSettings)
{-# DEPRECATED aasMaxRenditions "Use generic-lens or generic-optics with 'maxRenditions' instead." #-}

-- | Optional. The maximum target bit rate used in your automated ABR stack. Use this value to set an upper limit on the bandwidth consumed by the highest-quality rendition. This is the rendition that is delivered to viewers with the fastest internet connections. If you don't specify a value, MediaConvert uses 8,000,000 (8 mb/s) by default.
--
-- /Note:/ Consider using 'maxAbrBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aasMaxAbrBitrate :: Lens.Lens' AutomatedAbrSettings (Lude.Maybe Lude.Natural)
aasMaxAbrBitrate = Lens.lens (maxAbrBitrate :: AutomatedAbrSettings -> Lude.Maybe Lude.Natural) (\s a -> s {maxAbrBitrate = a} :: AutomatedAbrSettings)
{-# DEPRECATED aasMaxAbrBitrate "Use generic-lens or generic-optics with 'maxAbrBitrate' instead." #-}

-- | Optional. The minimum target bitrate used in your automated ABR stack. Use this value to set a lower limit on the bitrate of video delivered to viewers with slow internet connections. If you don't specify a value, MediaConvert uses 600,000 (600 kb/s) by default.
--
-- /Note:/ Consider using 'minAbrBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aasMinAbrBitrate :: Lens.Lens' AutomatedAbrSettings (Lude.Maybe Lude.Natural)
aasMinAbrBitrate = Lens.lens (minAbrBitrate :: AutomatedAbrSettings -> Lude.Maybe Lude.Natural) (\s a -> s {minAbrBitrate = a} :: AutomatedAbrSettings)
{-# DEPRECATED aasMinAbrBitrate "Use generic-lens or generic-optics with 'minAbrBitrate' instead." #-}

instance Lude.FromJSON AutomatedAbrSettings where
  parseJSON =
    Lude.withObject
      "AutomatedAbrSettings"
      ( \x ->
          AutomatedAbrSettings'
            Lude.<$> (x Lude..:? "maxRenditions")
            Lude.<*> (x Lude..:? "maxAbrBitrate")
            Lude.<*> (x Lude..:? "minAbrBitrate")
      )

instance Lude.ToJSON AutomatedAbrSettings where
  toJSON AutomatedAbrSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("maxRenditions" Lude..=) Lude.<$> maxRenditions,
            ("maxAbrBitrate" Lude..=) Lude.<$> maxAbrBitrate,
            ("minAbrBitrate" Lude..=) Lude.<$> minAbrBitrate
          ]
      )
