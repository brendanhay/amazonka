{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.LambdaFunctionStartedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.LambdaFunctionStartedEventAttributes
  ( LambdaFunctionStartedEventAttributes (..),

    -- * Smart constructor
    mkLambdaFunctionStartedEventAttributes,

    -- * Lenses
    lfseaScheduledEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides the details of the @LambdaFunctionStarted@ event. It isn't set for other event types.
--
-- /See:/ 'mkLambdaFunctionStartedEventAttributes' smart constructor.
newtype LambdaFunctionStartedEventAttributes = LambdaFunctionStartedEventAttributes'
  { -- | The ID of the @LambdaFunctionScheduled@ event that was recorded when this activity task was scheduled. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
    scheduledEventId :: Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'LambdaFunctionStartedEventAttributes' value with any optional fields omitted.
mkLambdaFunctionStartedEventAttributes ::
  -- | 'scheduledEventId'
  Core.Integer ->
  LambdaFunctionStartedEventAttributes
mkLambdaFunctionStartedEventAttributes scheduledEventId =
  LambdaFunctionStartedEventAttributes' {scheduledEventId}

-- | The ID of the @LambdaFunctionScheduled@ event that was recorded when this activity task was scheduled. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'scheduledEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfseaScheduledEventId :: Lens.Lens' LambdaFunctionStartedEventAttributes Core.Integer
lfseaScheduledEventId = Lens.field @"scheduledEventId"
{-# DEPRECATED lfseaScheduledEventId "Use generic-lens or generic-optics with 'scheduledEventId' instead." #-}

instance Core.FromJSON LambdaFunctionStartedEventAttributes where
  parseJSON =
    Core.withObject "LambdaFunctionStartedEventAttributes" Core.$
      \x ->
        LambdaFunctionStartedEventAttributes'
          Core.<$> (x Core..: "scheduledEventId")
