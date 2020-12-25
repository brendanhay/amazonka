{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.LambdaFunctionScheduledEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.LambdaFunctionScheduledEventAttributes
  ( LambdaFunctionScheduledEventAttributes (..),

    -- * Smart constructor
    mkLambdaFunctionScheduledEventAttributes,

    -- * Lenses
    lfseaId,
    lfseaName,
    lfseaDecisionTaskCompletedEventId,
    lfseaControl,
    lfseaInput,
    lfseaStartToCloseTimeout,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Data as Types
import qualified Network.AWS.SWF.Types.DurationInSecondsOptional as Types
import qualified Network.AWS.SWF.Types.FunctionId as Types
import qualified Network.AWS.SWF.Types.FunctionInput as Types
import qualified Network.AWS.SWF.Types.FunctionName as Types

-- | Provides the details of the @LambdaFunctionScheduled@ event. It isn't set for other event types.
--
-- /See:/ 'mkLambdaFunctionScheduledEventAttributes' smart constructor.
data LambdaFunctionScheduledEventAttributes = LambdaFunctionScheduledEventAttributes'
  { -- | The unique ID of the Lambda task.
    id :: Types.FunctionId,
    -- | The name of the Lambda function.
    name :: Types.FunctionName,
    -- | The ID of the @LambdaFunctionCompleted@ event corresponding to the decision that resulted in scheduling this activity task. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
    decisionTaskCompletedEventId :: Core.Integer,
    -- | Data attached to the event that the decider can use in subsequent workflow tasks. This data isn't sent to the Lambda task.
    control :: Core.Maybe Types.Data,
    -- | The input provided to the Lambda task.
    input :: Core.Maybe Types.FunctionInput,
    -- | The maximum amount of time a worker can take to process the Lambda task.
    startToCloseTimeout :: Core.Maybe Types.DurationInSecondsOptional
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LambdaFunctionScheduledEventAttributes' value with any optional fields omitted.
mkLambdaFunctionScheduledEventAttributes ::
  -- | 'id'
  Types.FunctionId ->
  -- | 'name'
  Types.FunctionName ->
  -- | 'decisionTaskCompletedEventId'
  Core.Integer ->
  LambdaFunctionScheduledEventAttributes
mkLambdaFunctionScheduledEventAttributes
  id
  name
  decisionTaskCompletedEventId =
    LambdaFunctionScheduledEventAttributes'
      { id,
        name,
        decisionTaskCompletedEventId,
        control = Core.Nothing,
        input = Core.Nothing,
        startToCloseTimeout = Core.Nothing
      }

-- | The unique ID of the Lambda task.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfseaId :: Lens.Lens' LambdaFunctionScheduledEventAttributes Types.FunctionId
lfseaId = Lens.field @"id"
{-# DEPRECATED lfseaId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The name of the Lambda function.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfseaName :: Lens.Lens' LambdaFunctionScheduledEventAttributes Types.FunctionName
lfseaName = Lens.field @"name"
{-# DEPRECATED lfseaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the @LambdaFunctionCompleted@ event corresponding to the decision that resulted in scheduling this activity task. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'decisionTaskCompletedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfseaDecisionTaskCompletedEventId :: Lens.Lens' LambdaFunctionScheduledEventAttributes Core.Integer
lfseaDecisionTaskCompletedEventId = Lens.field @"decisionTaskCompletedEventId"
{-# DEPRECATED lfseaDecisionTaskCompletedEventId "Use generic-lens or generic-optics with 'decisionTaskCompletedEventId' instead." #-}

-- | Data attached to the event that the decider can use in subsequent workflow tasks. This data isn't sent to the Lambda task.
--
-- /Note:/ Consider using 'control' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfseaControl :: Lens.Lens' LambdaFunctionScheduledEventAttributes (Core.Maybe Types.Data)
lfseaControl = Lens.field @"control"
{-# DEPRECATED lfseaControl "Use generic-lens or generic-optics with 'control' instead." #-}

-- | The input provided to the Lambda task.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfseaInput :: Lens.Lens' LambdaFunctionScheduledEventAttributes (Core.Maybe Types.FunctionInput)
lfseaInput = Lens.field @"input"
{-# DEPRECATED lfseaInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The maximum amount of time a worker can take to process the Lambda task.
--
-- /Note:/ Consider using 'startToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfseaStartToCloseTimeout :: Lens.Lens' LambdaFunctionScheduledEventAttributes (Core.Maybe Types.DurationInSecondsOptional)
lfseaStartToCloseTimeout = Lens.field @"startToCloseTimeout"
{-# DEPRECATED lfseaStartToCloseTimeout "Use generic-lens or generic-optics with 'startToCloseTimeout' instead." #-}

instance Core.FromJSON LambdaFunctionScheduledEventAttributes where
  parseJSON =
    Core.withObject "LambdaFunctionScheduledEventAttributes" Core.$
      \x ->
        LambdaFunctionScheduledEventAttributes'
          Core.<$> (x Core..: "id")
          Core.<*> (x Core..: "name")
          Core.<*> (x Core..: "decisionTaskCompletedEventId")
          Core.<*> (x Core..:? "control")
          Core.<*> (x Core..:? "input")
          Core.<*> (x Core..:? "startToCloseTimeout")
