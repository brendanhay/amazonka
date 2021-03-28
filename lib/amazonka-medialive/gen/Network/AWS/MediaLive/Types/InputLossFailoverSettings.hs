{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputLossFailoverSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.InputLossFailoverSettings
  ( InputLossFailoverSettings (..)
  -- * Smart constructor
  , mkInputLossFailoverSettings
  -- * Lenses
  , ilfsInputLossThresholdMsec
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | MediaLive will perform a failover if content is not detected in this input for the specified period.
--
-- /See:/ 'mkInputLossFailoverSettings' smart constructor.
newtype InputLossFailoverSettings = InputLossFailoverSettings'
  { inputLossThresholdMsec :: Core.Maybe Core.Natural
    -- ^ The amount of time (in milliseconds) that no input is detected. After that time, an input failover will occur.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InputLossFailoverSettings' value with any optional fields omitted.
mkInputLossFailoverSettings
    :: InputLossFailoverSettings
mkInputLossFailoverSettings
  = InputLossFailoverSettings'{inputLossThresholdMsec = Core.Nothing}

-- | The amount of time (in milliseconds) that no input is detected. After that time, an input failover will occur.
--
-- /Note:/ Consider using 'inputLossThresholdMsec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilfsInputLossThresholdMsec :: Lens.Lens' InputLossFailoverSettings (Core.Maybe Core.Natural)
ilfsInputLossThresholdMsec = Lens.field @"inputLossThresholdMsec"
{-# INLINEABLE ilfsInputLossThresholdMsec #-}
{-# DEPRECATED inputLossThresholdMsec "Use generic-lens or generic-optics with 'inputLossThresholdMsec' instead"  #-}

instance Core.FromJSON InputLossFailoverSettings where
        toJSON InputLossFailoverSettings{..}
          = Core.object
              (Core.catMaybes
                 [("inputLossThresholdMsec" Core..=) Core.<$>
                    inputLossThresholdMsec])

instance Core.FromJSON InputLossFailoverSettings where
        parseJSON
          = Core.withObject "InputLossFailoverSettings" Core.$
              \ x ->
                InputLossFailoverSettings' Core.<$>
                  (x Core..:? "inputLossThresholdMsec")
