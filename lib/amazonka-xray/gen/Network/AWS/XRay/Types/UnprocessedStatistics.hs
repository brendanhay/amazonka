{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.UnprocessedStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.XRay.Types.UnprocessedStatistics
  ( UnprocessedStatistics (..)
  -- * Smart constructor
  , mkUnprocessedStatistics
  -- * Lenses
  , usErrorCode
  , usMessage
  , usRuleName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Sampling statistics from a call to 'GetSamplingTargets' that X-Ray could not process.
--
-- /See:/ 'mkUnprocessedStatistics' smart constructor.
data UnprocessedStatistics = UnprocessedStatistics'
  { errorCode :: Core.Maybe Core.Text
    -- ^ The error code.
  , message :: Core.Maybe Core.Text
    -- ^ The error message.
  , ruleName :: Core.Maybe Core.Text
    -- ^ The name of the sampling rule.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnprocessedStatistics' value with any optional fields omitted.
mkUnprocessedStatistics
    :: UnprocessedStatistics
mkUnprocessedStatistics
  = UnprocessedStatistics'{errorCode = Core.Nothing,
                           message = Core.Nothing, ruleName = Core.Nothing}

-- | The error code.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usErrorCode :: Lens.Lens' UnprocessedStatistics (Core.Maybe Core.Text)
usErrorCode = Lens.field @"errorCode"
{-# INLINEABLE usErrorCode #-}
{-# DEPRECATED errorCode "Use generic-lens or generic-optics with 'errorCode' instead"  #-}

-- | The error message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usMessage :: Lens.Lens' UnprocessedStatistics (Core.Maybe Core.Text)
usMessage = Lens.field @"message"
{-# INLINEABLE usMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | The name of the sampling rule.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usRuleName :: Lens.Lens' UnprocessedStatistics (Core.Maybe Core.Text)
usRuleName = Lens.field @"ruleName"
{-# INLINEABLE usRuleName #-}
{-# DEPRECATED ruleName "Use generic-lens or generic-optics with 'ruleName' instead"  #-}

instance Core.FromJSON UnprocessedStatistics where
        parseJSON
          = Core.withObject "UnprocessedStatistics" Core.$
              \ x ->
                UnprocessedStatistics' Core.<$>
                  (x Core..:? "ErrorCode") Core.<*> x Core..:? "Message" Core.<*>
                    x Core..:? "RuleName"
