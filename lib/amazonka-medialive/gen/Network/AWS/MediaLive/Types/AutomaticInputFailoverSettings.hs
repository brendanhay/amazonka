{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AutomaticInputFailoverSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.AutomaticInputFailoverSettings
  ( AutomaticInputFailoverSettings (..)
  -- * Smart constructor
  , mkAutomaticInputFailoverSettings
  -- * Lenses
  , aifsSecondaryInputId
  , aifsErrorClearTimeMsec
  , aifsFailoverConditions
  , aifsInputPreference
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.FailoverCondition as Types
import qualified Network.AWS.MediaLive.Types.InputPreference as Types
import qualified Network.AWS.Prelude as Core

-- | The settings for Automatic Input Failover.
--
-- /See:/ 'mkAutomaticInputFailoverSettings' smart constructor.
data AutomaticInputFailoverSettings = AutomaticInputFailoverSettings'
  { secondaryInputId :: Core.Text
    -- ^ The input ID of the secondary input in the automatic input failover pair.
  , errorClearTimeMsec :: Core.Maybe Core.Natural
    -- ^ This clear time defines the requirement a recovered input must meet to be considered healthy. The input must have no failover conditions for this length of time. Enter a time in milliseconds. This value is particularly important if the input_preference for the failover pair is set to PRIMARY_INPUT_PREFERRED, because after this time, MediaLive will switch back to the primary input.
  , failoverConditions :: Core.Maybe [Types.FailoverCondition]
    -- ^ A list of failover conditions. If any of these conditions occur, MediaLive will perform a failover to the other input.
  , inputPreference :: Core.Maybe Types.InputPreference
    -- ^ Input preference when deciding which input to make active when a previously failed input has recovered.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutomaticInputFailoverSettings' value with any optional fields omitted.
mkAutomaticInputFailoverSettings
    :: Core.Text -- ^ 'secondaryInputId'
    -> AutomaticInputFailoverSettings
mkAutomaticInputFailoverSettings secondaryInputId
  = AutomaticInputFailoverSettings'{secondaryInputId,
                                    errorClearTimeMsec = Core.Nothing,
                                    failoverConditions = Core.Nothing,
                                    inputPreference = Core.Nothing}

-- | The input ID of the secondary input in the automatic input failover pair.
--
-- /Note:/ Consider using 'secondaryInputId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aifsSecondaryInputId :: Lens.Lens' AutomaticInputFailoverSettings Core.Text
aifsSecondaryInputId = Lens.field @"secondaryInputId"
{-# INLINEABLE aifsSecondaryInputId #-}
{-# DEPRECATED secondaryInputId "Use generic-lens or generic-optics with 'secondaryInputId' instead"  #-}

-- | This clear time defines the requirement a recovered input must meet to be considered healthy. The input must have no failover conditions for this length of time. Enter a time in milliseconds. This value is particularly important if the input_preference for the failover pair is set to PRIMARY_INPUT_PREFERRED, because after this time, MediaLive will switch back to the primary input.
--
-- /Note:/ Consider using 'errorClearTimeMsec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aifsErrorClearTimeMsec :: Lens.Lens' AutomaticInputFailoverSettings (Core.Maybe Core.Natural)
aifsErrorClearTimeMsec = Lens.field @"errorClearTimeMsec"
{-# INLINEABLE aifsErrorClearTimeMsec #-}
{-# DEPRECATED errorClearTimeMsec "Use generic-lens or generic-optics with 'errorClearTimeMsec' instead"  #-}

-- | A list of failover conditions. If any of these conditions occur, MediaLive will perform a failover to the other input.
--
-- /Note:/ Consider using 'failoverConditions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aifsFailoverConditions :: Lens.Lens' AutomaticInputFailoverSettings (Core.Maybe [Types.FailoverCondition])
aifsFailoverConditions = Lens.field @"failoverConditions"
{-# INLINEABLE aifsFailoverConditions #-}
{-# DEPRECATED failoverConditions "Use generic-lens or generic-optics with 'failoverConditions' instead"  #-}

-- | Input preference when deciding which input to make active when a previously failed input has recovered.
--
-- /Note:/ Consider using 'inputPreference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aifsInputPreference :: Lens.Lens' AutomaticInputFailoverSettings (Core.Maybe Types.InputPreference)
aifsInputPreference = Lens.field @"inputPreference"
{-# INLINEABLE aifsInputPreference #-}
{-# DEPRECATED inputPreference "Use generic-lens or generic-optics with 'inputPreference' instead"  #-}

instance Core.FromJSON AutomaticInputFailoverSettings where
        toJSON AutomaticInputFailoverSettings{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("secondaryInputId" Core..= secondaryInputId),
                  ("errorClearTimeMsec" Core..=) Core.<$> errorClearTimeMsec,
                  ("failoverConditions" Core..=) Core.<$> failoverConditions,
                  ("inputPreference" Core..=) Core.<$> inputPreference])

instance Core.FromJSON AutomaticInputFailoverSettings where
        parseJSON
          = Core.withObject "AutomaticInputFailoverSettings" Core.$
              \ x ->
                AutomaticInputFailoverSettings' Core.<$>
                  (x Core..: "secondaryInputId") Core.<*>
                    x Core..:? "errorClearTimeMsec"
                    Core.<*> x Core..:? "failoverConditions"
                    Core.<*> x Core..:? "inputPreference"
