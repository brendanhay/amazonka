{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.FailureDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.FailureDetails
  ( FailureDetails (..)
  -- * Smart constructor
  , mkFailureDetails
  -- * Lenses
  , fdDetails
  , fdFailureStage
  , fdFailureType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.AutomationParameterKey as Types
import qualified Network.AWS.SSM.Types.AutomationParameterValue as Types

-- | Information about an Automation failure.
--
-- /See:/ 'mkFailureDetails' smart constructor.
data FailureDetails = FailureDetails'
  { details :: Core.Maybe (Core.HashMap Types.AutomationParameterKey [Types.AutomationParameterValue])
    -- ^ Detailed information about the Automation step failure.
  , failureStage :: Core.Maybe Core.Text
    -- ^ The stage of the Automation execution when the failure occurred. The stages include the following: InputValidation, PreVerification, Invocation, PostVerification.
  , failureType :: Core.Maybe Core.Text
    -- ^ The type of Automation failure. Failure types include the following: Action, Permission, Throttling, Verification, Internal.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FailureDetails' value with any optional fields omitted.
mkFailureDetails
    :: FailureDetails
mkFailureDetails
  = FailureDetails'{details = Core.Nothing,
                    failureStage = Core.Nothing, failureType = Core.Nothing}

-- | Detailed information about the Automation step failure.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdDetails :: Lens.Lens' FailureDetails (Core.Maybe (Core.HashMap Types.AutomationParameterKey [Types.AutomationParameterValue]))
fdDetails = Lens.field @"details"
{-# INLINEABLE fdDetails #-}
{-# DEPRECATED details "Use generic-lens or generic-optics with 'details' instead"  #-}

-- | The stage of the Automation execution when the failure occurred. The stages include the following: InputValidation, PreVerification, Invocation, PostVerification.
--
-- /Note:/ Consider using 'failureStage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdFailureStage :: Lens.Lens' FailureDetails (Core.Maybe Core.Text)
fdFailureStage = Lens.field @"failureStage"
{-# INLINEABLE fdFailureStage #-}
{-# DEPRECATED failureStage "Use generic-lens or generic-optics with 'failureStage' instead"  #-}

-- | The type of Automation failure. Failure types include the following: Action, Permission, Throttling, Verification, Internal.
--
-- /Note:/ Consider using 'failureType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdFailureType :: Lens.Lens' FailureDetails (Core.Maybe Core.Text)
fdFailureType = Lens.field @"failureType"
{-# INLINEABLE fdFailureType #-}
{-# DEPRECATED failureType "Use generic-lens or generic-optics with 'failureType' instead"  #-}

instance Core.FromJSON FailureDetails where
        parseJSON
          = Core.withObject "FailureDetails" Core.$
              \ x ->
                FailureDetails' Core.<$>
                  (x Core..:? "Details") Core.<*> x Core..:? "FailureStage" Core.<*>
                    x Core..:? "FailureType"
