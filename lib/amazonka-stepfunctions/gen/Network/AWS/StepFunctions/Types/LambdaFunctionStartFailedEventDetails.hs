{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.LambdaFunctionStartFailedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.LambdaFunctionStartFailedEventDetails
  ( LambdaFunctionStartFailedEventDetails (..)
  -- * Smart constructor
  , mkLambdaFunctionStartFailedEventDetails
  -- * Lenses
  , lCause
  , lError
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StepFunctions.Types.SensitiveCause as Types
import qualified Network.AWS.StepFunctions.Types.SensitiveError as Types

-- | Contains details about a lambda function that failed to start during an execution.
--
-- /See:/ 'mkLambdaFunctionStartFailedEventDetails' smart constructor.
data LambdaFunctionStartFailedEventDetails = LambdaFunctionStartFailedEventDetails'
  { cause :: Core.Maybe Types.SensitiveCause
    -- ^ A more detailed explanation of the cause of the failure.
  , error :: Core.Maybe Types.SensitiveError
    -- ^ The error code of the failure.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LambdaFunctionStartFailedEventDetails' value with any optional fields omitted.
mkLambdaFunctionStartFailedEventDetails
    :: LambdaFunctionStartFailedEventDetails
mkLambdaFunctionStartFailedEventDetails
  = LambdaFunctionStartFailedEventDetails'{cause = Core.Nothing,
                                           error = Core.Nothing}

-- | A more detailed explanation of the cause of the failure.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lCause :: Lens.Lens' LambdaFunctionStartFailedEventDetails (Core.Maybe Types.SensitiveCause)
lCause = Lens.field @"cause"
{-# INLINEABLE lCause #-}
{-# DEPRECATED cause "Use generic-lens or generic-optics with 'cause' instead"  #-}

-- | The error code of the failure.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lError :: Lens.Lens' LambdaFunctionStartFailedEventDetails (Core.Maybe Types.SensitiveError)
lError = Lens.field @"error"
{-# INLINEABLE lError #-}
{-# DEPRECATED error "Use generic-lens or generic-optics with 'error' instead"  #-}

instance Core.FromJSON LambdaFunctionStartFailedEventDetails where
        parseJSON
          = Core.withObject "LambdaFunctionStartFailedEventDetails" Core.$
              \ x ->
                LambdaFunctionStartFailedEventDetails' Core.<$>
                  (x Core..:? "cause") Core.<*> x Core..:? "error"
