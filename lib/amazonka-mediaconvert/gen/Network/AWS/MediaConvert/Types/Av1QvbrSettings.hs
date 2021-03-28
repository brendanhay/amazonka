{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Av1QvbrSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Av1QvbrSettings
  ( Av1QvbrSettings (..)
  -- * Smart constructor
  , mkAv1QvbrSettings
  -- * Lenses
  , aqsQvbrQualityLevel
  , aqsQvbrQualityLevelFineTune
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Settings for quality-defined variable bitrate encoding with the AV1 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
--
-- /See:/ 'mkAv1QvbrSettings' smart constructor.
data Av1QvbrSettings = Av1QvbrSettings'
  { qvbrQualityLevel :: Core.Maybe Core.Natural
    -- ^ Required when you use QVBR rate control mode. That is, when you specify qvbrSettings within av1Settings. Specify the general target quality level for this output, from 1 to 10. Use higher numbers for greater quality. Level 10 results in nearly lossless compression. The quality level for most broadcast-quality transcodes is between 6 and 9. Optionally, to specify a value between whole numbers, also provide a value for the setting qvbrQualityLevelFineTune. For example, if you want your QVBR quality level to be 7.33, set qvbrQualityLevel to 7 and set qvbrQualityLevelFineTune to .33.
  , qvbrQualityLevelFineTune :: Core.Maybe Core.Double
    -- ^ Optional. Specify a value here to set the QVBR quality to a level that is between whole numbers. For example, if you want your QVBR quality level to be 7.33, set qvbrQualityLevel to 7 and set qvbrQualityLevelFineTune to .33. MediaConvert rounds your QVBR quality level to the nearest third of a whole number. For example, if you set qvbrQualityLevel to 7 and you set qvbrQualityLevelFineTune to .25, your actual QVBR quality level is 7.33.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Av1QvbrSettings' value with any optional fields omitted.
mkAv1QvbrSettings
    :: Av1QvbrSettings
mkAv1QvbrSettings
  = Av1QvbrSettings'{qvbrQualityLevel = Core.Nothing,
                     qvbrQualityLevelFineTune = Core.Nothing}

-- | Required when you use QVBR rate control mode. That is, when you specify qvbrSettings within av1Settings. Specify the general target quality level for this output, from 1 to 10. Use higher numbers for greater quality. Level 10 results in nearly lossless compression. The quality level for most broadcast-quality transcodes is between 6 and 9. Optionally, to specify a value between whole numbers, also provide a value for the setting qvbrQualityLevelFineTune. For example, if you want your QVBR quality level to be 7.33, set qvbrQualityLevel to 7 and set qvbrQualityLevelFineTune to .33.
--
-- /Note:/ Consider using 'qvbrQualityLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aqsQvbrQualityLevel :: Lens.Lens' Av1QvbrSettings (Core.Maybe Core.Natural)
aqsQvbrQualityLevel = Lens.field @"qvbrQualityLevel"
{-# INLINEABLE aqsQvbrQualityLevel #-}
{-# DEPRECATED qvbrQualityLevel "Use generic-lens or generic-optics with 'qvbrQualityLevel' instead"  #-}

-- | Optional. Specify a value here to set the QVBR quality to a level that is between whole numbers. For example, if you want your QVBR quality level to be 7.33, set qvbrQualityLevel to 7 and set qvbrQualityLevelFineTune to .33. MediaConvert rounds your QVBR quality level to the nearest third of a whole number. For example, if you set qvbrQualityLevel to 7 and you set qvbrQualityLevelFineTune to .25, your actual QVBR quality level is 7.33.
--
-- /Note:/ Consider using 'qvbrQualityLevelFineTune' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aqsQvbrQualityLevelFineTune :: Lens.Lens' Av1QvbrSettings (Core.Maybe Core.Double)
aqsQvbrQualityLevelFineTune = Lens.field @"qvbrQualityLevelFineTune"
{-# INLINEABLE aqsQvbrQualityLevelFineTune #-}
{-# DEPRECATED qvbrQualityLevelFineTune "Use generic-lens or generic-optics with 'qvbrQualityLevelFineTune' instead"  #-}

instance Core.FromJSON Av1QvbrSettings where
        toJSON Av1QvbrSettings{..}
          = Core.object
              (Core.catMaybes
                 [("qvbrQualityLevel" Core..=) Core.<$> qvbrQualityLevel,
                  ("qvbrQualityLevelFineTune" Core..=) Core.<$>
                    qvbrQualityLevelFineTune])

instance Core.FromJSON Av1QvbrSettings where
        parseJSON
          = Core.withObject "Av1QvbrSettings" Core.$
              \ x ->
                Av1QvbrSettings' Core.<$>
                  (x Core..:? "qvbrQualityLevel") Core.<*>
                    x Core..:? "qvbrQualityLevelFineTune"
