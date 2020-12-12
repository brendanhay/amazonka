{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264QvbrSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264QvbrSettings
  ( H264QvbrSettings (..),

    -- * Smart constructor
    mkH264QvbrSettings,

    -- * Lenses
    hQvbrQualityLevelFineTune,
    hMaxAverageBitrate,
    hQvbrQualityLevel,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Settings for quality-defined variable bitrate encoding with the H.264 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
--
-- /See:/ 'mkH264QvbrSettings' smart constructor.
data H264QvbrSettings = H264QvbrSettings'
  { qvbrQualityLevelFineTune ::
      Lude.Maybe Lude.Double,
    maxAverageBitrate :: Lude.Maybe Lude.Natural,
    qvbrQualityLevel :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'H264QvbrSettings' with the minimum fields required to make a request.
--
-- * 'maxAverageBitrate' - Use this setting only when Rate control mode is QVBR and Quality tuning level is Multi-pass HQ. For Max average bitrate values suited to the complexity of your input video, the service limits the average bitrate of the video part of this output to the value that you choose. That is, the total size of the video element is less than or equal to the value you set multiplied by the number of seconds of encoded output.
-- * 'qvbrQualityLevel' - Required when you use QVBR rate control mode. That is, when you specify qvbrSettings within h264Settings. Specify the general target quality level for this output, from 1 to 10. Use higher numbers for greater quality. Level 10 results in nearly lossless compression. The quality level for most broadcast-quality transcodes is between 6 and 9. Optionally, to specify a value between whole numbers, also provide a value for the setting qvbrQualityLevelFineTune. For example, if you want your QVBR quality level to be 7.33, set qvbrQualityLevel to 7 and set qvbrQualityLevelFineTune to .33.
-- * 'qvbrQualityLevelFineTune' - Optional. Specify a value here to set the QVBR quality to a level that is between whole numbers. For example, if you want your QVBR quality level to be 7.33, set qvbrQualityLevel to 7 and set qvbrQualityLevelFineTune to .33. MediaConvert rounds your QVBR quality level to the nearest third of a whole number. For example, if you set qvbrQualityLevel to 7 and you set qvbrQualityLevelFineTune to .25, your actual QVBR quality level is 7.33.
mkH264QvbrSettings ::
  H264QvbrSettings
mkH264QvbrSettings =
  H264QvbrSettings'
    { qvbrQualityLevelFineTune = Lude.Nothing,
      maxAverageBitrate = Lude.Nothing,
      qvbrQualityLevel = Lude.Nothing
    }

-- | Optional. Specify a value here to set the QVBR quality to a level that is between whole numbers. For example, if you want your QVBR quality level to be 7.33, set qvbrQualityLevel to 7 and set qvbrQualityLevelFineTune to .33. MediaConvert rounds your QVBR quality level to the nearest third of a whole number. For example, if you set qvbrQualityLevel to 7 and you set qvbrQualityLevelFineTune to .25, your actual QVBR quality level is 7.33.
--
-- /Note:/ Consider using 'qvbrQualityLevelFineTune' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hQvbrQualityLevelFineTune :: Lens.Lens' H264QvbrSettings (Lude.Maybe Lude.Double)
hQvbrQualityLevelFineTune = Lens.lens (qvbrQualityLevelFineTune :: H264QvbrSettings -> Lude.Maybe Lude.Double) (\s a -> s {qvbrQualityLevelFineTune = a} :: H264QvbrSettings)
{-# DEPRECATED hQvbrQualityLevelFineTune "Use generic-lens or generic-optics with 'qvbrQualityLevelFineTune' instead." #-}

-- | Use this setting only when Rate control mode is QVBR and Quality tuning level is Multi-pass HQ. For Max average bitrate values suited to the complexity of your input video, the service limits the average bitrate of the video part of this output to the value that you choose. That is, the total size of the video element is less than or equal to the value you set multiplied by the number of seconds of encoded output.
--
-- /Note:/ Consider using 'maxAverageBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hMaxAverageBitrate :: Lens.Lens' H264QvbrSettings (Lude.Maybe Lude.Natural)
hMaxAverageBitrate = Lens.lens (maxAverageBitrate :: H264QvbrSettings -> Lude.Maybe Lude.Natural) (\s a -> s {maxAverageBitrate = a} :: H264QvbrSettings)
{-# DEPRECATED hMaxAverageBitrate "Use generic-lens or generic-optics with 'maxAverageBitrate' instead." #-}

-- | Required when you use QVBR rate control mode. That is, when you specify qvbrSettings within h264Settings. Specify the general target quality level for this output, from 1 to 10. Use higher numbers for greater quality. Level 10 results in nearly lossless compression. The quality level for most broadcast-quality transcodes is between 6 and 9. Optionally, to specify a value between whole numbers, also provide a value for the setting qvbrQualityLevelFineTune. For example, if you want your QVBR quality level to be 7.33, set qvbrQualityLevel to 7 and set qvbrQualityLevelFineTune to .33.
--
-- /Note:/ Consider using 'qvbrQualityLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hQvbrQualityLevel :: Lens.Lens' H264QvbrSettings (Lude.Maybe Lude.Natural)
hQvbrQualityLevel = Lens.lens (qvbrQualityLevel :: H264QvbrSettings -> Lude.Maybe Lude.Natural) (\s a -> s {qvbrQualityLevel = a} :: H264QvbrSettings)
{-# DEPRECATED hQvbrQualityLevel "Use generic-lens or generic-optics with 'qvbrQualityLevel' instead." #-}

instance Lude.FromJSON H264QvbrSettings where
  parseJSON =
    Lude.withObject
      "H264QvbrSettings"
      ( \x ->
          H264QvbrSettings'
            Lude.<$> (x Lude..:? "qvbrQualityLevelFineTune")
            Lude.<*> (x Lude..:? "maxAverageBitrate")
            Lude.<*> (x Lude..:? "qvbrQualityLevel")
      )

instance Lude.ToJSON H264QvbrSettings where
  toJSON H264QvbrSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("qvbrQualityLevelFineTune" Lude..=)
              Lude.<$> qvbrQualityLevelFineTune,
            ("maxAverageBitrate" Lude..=) Lude.<$> maxAverageBitrate,
            ("qvbrQualityLevel" Lude..=) Lude.<$> qvbrQualityLevel
          ]
      )
