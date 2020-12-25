{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.UnprocessedStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.UnprocessedStatistics
  ( UnprocessedStatistics (..),

    -- * Smart constructor
    mkUnprocessedStatistics,

    -- * Lenses
    usErrorCode,
    usMessage,
    usRuleName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.ErrorCode as Types
import qualified Network.AWS.XRay.Types.Message as Types
import qualified Network.AWS.XRay.Types.String as Types

-- | Sampling statistics from a call to 'GetSamplingTargets' that X-Ray could not process.
--
-- /See:/ 'mkUnprocessedStatistics' smart constructor.
data UnprocessedStatistics = UnprocessedStatistics'
  { -- | The error code.
    errorCode :: Core.Maybe Types.ErrorCode,
    -- | The error message.
    message :: Core.Maybe Types.Message,
    -- | The name of the sampling rule.
    ruleName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnprocessedStatistics' value with any optional fields omitted.
mkUnprocessedStatistics ::
  UnprocessedStatistics
mkUnprocessedStatistics =
  UnprocessedStatistics'
    { errorCode = Core.Nothing,
      message = Core.Nothing,
      ruleName = Core.Nothing
    }

-- | The error code.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usErrorCode :: Lens.Lens' UnprocessedStatistics (Core.Maybe Types.ErrorCode)
usErrorCode = Lens.field @"errorCode"
{-# DEPRECATED usErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The error message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usMessage :: Lens.Lens' UnprocessedStatistics (Core.Maybe Types.Message)
usMessage = Lens.field @"message"
{-# DEPRECATED usMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The name of the sampling rule.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usRuleName :: Lens.Lens' UnprocessedStatistics (Core.Maybe Types.String)
usRuleName = Lens.field @"ruleName"
{-# DEPRECATED usRuleName "Use generic-lens or generic-optics with 'ruleName' instead." #-}

instance Core.FromJSON UnprocessedStatistics where
  parseJSON =
    Core.withObject "UnprocessedStatistics" Core.$
      \x ->
        UnprocessedStatistics'
          Core.<$> (x Core..:? "ErrorCode")
          Core.<*> (x Core..:? "Message")
          Core.<*> (x Core..:? "RuleName")
