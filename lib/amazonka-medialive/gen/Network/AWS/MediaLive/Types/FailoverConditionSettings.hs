{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FailoverConditionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FailoverConditionSettings
  ( FailoverConditionSettings (..),

    -- * Smart constructor
    mkFailoverConditionSettings,

    -- * Lenses
    fcsInputLossSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.InputLossFailoverSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Settings for one failover condition.
--
-- /See:/ 'mkFailoverConditionSettings' smart constructor.
newtype FailoverConditionSettings = FailoverConditionSettings'
  { -- | MediaLive will perform a failover if content is not detected in this input for the specified period.
    inputLossSettings :: Core.Maybe Types.InputLossFailoverSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'FailoverConditionSettings' value with any optional fields omitted.
mkFailoverConditionSettings ::
  FailoverConditionSettings
mkFailoverConditionSettings =
  FailoverConditionSettings' {inputLossSettings = Core.Nothing}

-- | MediaLive will perform a failover if content is not detected in this input for the specified period.
--
-- /Note:/ Consider using 'inputLossSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcsInputLossSettings :: Lens.Lens' FailoverConditionSettings (Core.Maybe Types.InputLossFailoverSettings)
fcsInputLossSettings = Lens.field @"inputLossSettings"
{-# DEPRECATED fcsInputLossSettings "Use generic-lens or generic-optics with 'inputLossSettings' instead." #-}

instance Core.FromJSON FailoverConditionSettings where
  toJSON FailoverConditionSettings {..} =
    Core.object
      ( Core.catMaybes
          [("inputLossSettings" Core..=) Core.<$> inputLossSettings]
      )

instance Core.FromJSON FailoverConditionSettings where
  parseJSON =
    Core.withObject "FailoverConditionSettings" Core.$
      \x ->
        FailoverConditionSettings'
          Core.<$> (x Core..:? "inputLossSettings")
