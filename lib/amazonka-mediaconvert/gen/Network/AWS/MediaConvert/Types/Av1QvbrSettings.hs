{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Av1QvbrSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Av1QvbrSettings
  ( Av1QvbrSettings (..),

    -- * Smart constructor
    mkAv1QvbrSettings,

    -- * Lenses
    aqsQvbrQualityLevelFineTune,
    aqsQvbrQualityLevel,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Settings for quality-defined variable bitrate encoding with the AV1 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
--
-- /See:/ 'mkAv1QvbrSettings' smart constructor.
data Av1QvbrSettings = Av1QvbrSettings'
  { -- | Optional. Specify a value here to set the QVBR quality to a level that is between whole numbers. For example, if you want your QVBR quality level to be 7.33, set qvbrQualityLevel to 7 and set qvbrQualityLevelFineTune to .33. MediaConvert rounds your QVBR quality level to the nearest third of a whole number. For example, if you set qvbrQualityLevel to 7 and you set qvbrQualityLevelFineTune to .25, your actual QVBR quality level is 7.33.
    qvbrQualityLevelFineTune :: Lude.Maybe Lude.Double,
    -- | Required when you use QVBR rate control mode. That is, when you specify qvbrSettings within av1Settings. Specify the general target quality level for this output, from 1 to 10. Use higher numbers for greater quality. Level 10 results in nearly lossless compression. The quality level for most broadcast-quality transcodes is between 6 and 9. Optionally, to specify a value between whole numbers, also provide a value for the setting qvbrQualityLevelFineTune. For example, if you want your QVBR quality level to be 7.33, set qvbrQualityLevel to 7 and set qvbrQualityLevelFineTune to .33.
    qvbrQualityLevel :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Av1QvbrSettings' with the minimum fields required to make a request.
--
-- * 'qvbrQualityLevelFineTune' - Optional. Specify a value here to set the QVBR quality to a level that is between whole numbers. For example, if you want your QVBR quality level to be 7.33, set qvbrQualityLevel to 7 and set qvbrQualityLevelFineTune to .33. MediaConvert rounds your QVBR quality level to the nearest third of a whole number. For example, if you set qvbrQualityLevel to 7 and you set qvbrQualityLevelFineTune to .25, your actual QVBR quality level is 7.33.
-- * 'qvbrQualityLevel' - Required when you use QVBR rate control mode. That is, when you specify qvbrSettings within av1Settings. Specify the general target quality level for this output, from 1 to 10. Use higher numbers for greater quality. Level 10 results in nearly lossless compression. The quality level for most broadcast-quality transcodes is between 6 and 9. Optionally, to specify a value between whole numbers, also provide a value for the setting qvbrQualityLevelFineTune. For example, if you want your QVBR quality level to be 7.33, set qvbrQualityLevel to 7 and set qvbrQualityLevelFineTune to .33.
mkAv1QvbrSettings ::
  Av1QvbrSettings
mkAv1QvbrSettings =
  Av1QvbrSettings'
    { qvbrQualityLevelFineTune = Lude.Nothing,
      qvbrQualityLevel = Lude.Nothing
    }

-- | Optional. Specify a value here to set the QVBR quality to a level that is between whole numbers. For example, if you want your QVBR quality level to be 7.33, set qvbrQualityLevel to 7 and set qvbrQualityLevelFineTune to .33. MediaConvert rounds your QVBR quality level to the nearest third of a whole number. For example, if you set qvbrQualityLevel to 7 and you set qvbrQualityLevelFineTune to .25, your actual QVBR quality level is 7.33.
--
-- /Note:/ Consider using 'qvbrQualityLevelFineTune' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aqsQvbrQualityLevelFineTune :: Lens.Lens' Av1QvbrSettings (Lude.Maybe Lude.Double)
aqsQvbrQualityLevelFineTune = Lens.lens (qvbrQualityLevelFineTune :: Av1QvbrSettings -> Lude.Maybe Lude.Double) (\s a -> s {qvbrQualityLevelFineTune = a} :: Av1QvbrSettings)
{-# DEPRECATED aqsQvbrQualityLevelFineTune "Use generic-lens or generic-optics with 'qvbrQualityLevelFineTune' instead." #-}

-- | Required when you use QVBR rate control mode. That is, when you specify qvbrSettings within av1Settings. Specify the general target quality level for this output, from 1 to 10. Use higher numbers for greater quality. Level 10 results in nearly lossless compression. The quality level for most broadcast-quality transcodes is between 6 and 9. Optionally, to specify a value between whole numbers, also provide a value for the setting qvbrQualityLevelFineTune. For example, if you want your QVBR quality level to be 7.33, set qvbrQualityLevel to 7 and set qvbrQualityLevelFineTune to .33.
--
-- /Note:/ Consider using 'qvbrQualityLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aqsQvbrQualityLevel :: Lens.Lens' Av1QvbrSettings (Lude.Maybe Lude.Natural)
aqsQvbrQualityLevel = Lens.lens (qvbrQualityLevel :: Av1QvbrSettings -> Lude.Maybe Lude.Natural) (\s a -> s {qvbrQualityLevel = a} :: Av1QvbrSettings)
{-# DEPRECATED aqsQvbrQualityLevel "Use generic-lens or generic-optics with 'qvbrQualityLevel' instead." #-}

instance Lude.FromJSON Av1QvbrSettings where
  parseJSON =
    Lude.withObject
      "Av1QvbrSettings"
      ( \x ->
          Av1QvbrSettings'
            Lude.<$> (x Lude..:? "qvbrQualityLevelFineTune")
            Lude.<*> (x Lude..:? "qvbrQualityLevel")
      )

instance Lude.ToJSON Av1QvbrSettings where
  toJSON Av1QvbrSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("qvbrQualityLevelFineTune" Lude..=)
              Lude.<$> qvbrQualityLevelFineTune,
            ("qvbrQualityLevel" Lude..=) Lude.<$> qvbrQualityLevel
          ]
      )
