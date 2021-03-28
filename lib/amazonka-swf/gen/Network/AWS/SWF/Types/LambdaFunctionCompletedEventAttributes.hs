{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.LambdaFunctionCompletedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.LambdaFunctionCompletedEventAttributes
  ( LambdaFunctionCompletedEventAttributes (..)
  -- * Smart constructor
  , mkLambdaFunctionCompletedEventAttributes
  -- * Lenses
  , lfceaScheduledEventId
  , lfceaStartedEventId
  , lfceaResult
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Data as Types

-- | Provides the details of the @LambdaFunctionCompleted@ event. It isn't set for other event types.
--
-- /See:/ 'mkLambdaFunctionCompletedEventAttributes' smart constructor.
data LambdaFunctionCompletedEventAttributes = LambdaFunctionCompletedEventAttributes'
  { scheduledEventId :: Core.Integer
    -- ^ The ID of the @LambdaFunctionScheduled@ event that was recorded when this Lambda task was scheduled. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
  , startedEventId :: Core.Integer
    -- ^ The ID of the @LambdaFunctionStarted@ event recorded when this activity task started. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
  , result :: Core.Maybe Types.Data
    -- ^ The results of the Lambda task.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LambdaFunctionCompletedEventAttributes' value with any optional fields omitted.
mkLambdaFunctionCompletedEventAttributes
    :: Core.Integer -- ^ 'scheduledEventId'
    -> Core.Integer -- ^ 'startedEventId'
    -> LambdaFunctionCompletedEventAttributes
mkLambdaFunctionCompletedEventAttributes scheduledEventId
  startedEventId
  = LambdaFunctionCompletedEventAttributes'{scheduledEventId,
                                            startedEventId, result = Core.Nothing}

-- | The ID of the @LambdaFunctionScheduled@ event that was recorded when this Lambda task was scheduled. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'scheduledEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfceaScheduledEventId :: Lens.Lens' LambdaFunctionCompletedEventAttributes Core.Integer
lfceaScheduledEventId = Lens.field @"scheduledEventId"
{-# INLINEABLE lfceaScheduledEventId #-}
{-# DEPRECATED scheduledEventId "Use generic-lens or generic-optics with 'scheduledEventId' instead"  #-}

-- | The ID of the @LambdaFunctionStarted@ event recorded when this activity task started. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfceaStartedEventId :: Lens.Lens' LambdaFunctionCompletedEventAttributes Core.Integer
lfceaStartedEventId = Lens.field @"startedEventId"
{-# INLINEABLE lfceaStartedEventId #-}
{-# DEPRECATED startedEventId "Use generic-lens or generic-optics with 'startedEventId' instead"  #-}

-- | The results of the Lambda task.
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfceaResult :: Lens.Lens' LambdaFunctionCompletedEventAttributes (Core.Maybe Types.Data)
lfceaResult = Lens.field @"result"
{-# INLINEABLE lfceaResult #-}
{-# DEPRECATED result "Use generic-lens or generic-optics with 'result' instead"  #-}

instance Core.FromJSON LambdaFunctionCompletedEventAttributes where
        parseJSON
          = Core.withObject "LambdaFunctionCompletedEventAttributes" Core.$
              \ x ->
                LambdaFunctionCompletedEventAttributes' Core.<$>
                  (x Core..: "scheduledEventId") Core.<*> x Core..: "startedEventId"
                    Core.<*> x Core..:? "result"
