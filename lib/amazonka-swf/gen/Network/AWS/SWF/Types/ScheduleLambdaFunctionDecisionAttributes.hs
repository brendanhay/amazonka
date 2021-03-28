{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ScheduleLambdaFunctionDecisionAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.ScheduleLambdaFunctionDecisionAttributes
  ( ScheduleLambdaFunctionDecisionAttributes (..)
  -- * Smart constructor
  , mkScheduleLambdaFunctionDecisionAttributes
  -- * Lenses
  , slfdaId
  , slfdaName
  , slfdaControl
  , slfdaInput
  , slfdaStartToCloseTimeout
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Data as Types
import qualified Network.AWS.SWF.Types.DurationInSecondsOptional as Types
import qualified Network.AWS.SWF.Types.FunctionId as Types
import qualified Network.AWS.SWF.Types.FunctionInput as Types
import qualified Network.AWS.SWF.Types.FunctionName as Types

-- | Decision attributes specified in @scheduleLambdaFunctionDecisionAttributes@ within the list of decisions @decisions@ passed to 'RespondDecisionTaskCompleted' .
--
-- /See:/ 'mkScheduleLambdaFunctionDecisionAttributes' smart constructor.
data ScheduleLambdaFunctionDecisionAttributes = ScheduleLambdaFunctionDecisionAttributes'
  { id :: Types.FunctionId
    -- ^ A string that identifies the Lambda function execution in the event history.
  , name :: Types.FunctionName
    -- ^ The name, or ARN, of the Lambda function to schedule.
  , control :: Core.Maybe Types.Data
    -- ^ The data attached to the event that the decider can use in subsequent workflow tasks. This data isn't sent to the Lambda task.
  , input :: Core.Maybe Types.FunctionInput
    -- ^ The optional input data to be supplied to the Lambda function.
  , startToCloseTimeout :: Core.Maybe Types.DurationInSecondsOptional
    -- ^ The timeout value, in seconds, after which the Lambda function is considered to be failed once it has started. This can be any integer from 1-300 (1s-5m). If no value is supplied, than a default value of 300s is assumed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScheduleLambdaFunctionDecisionAttributes' value with any optional fields omitted.
mkScheduleLambdaFunctionDecisionAttributes
    :: Types.FunctionId -- ^ 'id'
    -> Types.FunctionName -- ^ 'name'
    -> ScheduleLambdaFunctionDecisionAttributes
mkScheduleLambdaFunctionDecisionAttributes id name
  = ScheduleLambdaFunctionDecisionAttributes'{id, name,
                                              control = Core.Nothing, input = Core.Nothing,
                                              startToCloseTimeout = Core.Nothing}

-- | A string that identifies the Lambda function execution in the event history.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slfdaId :: Lens.Lens' ScheduleLambdaFunctionDecisionAttributes Types.FunctionId
slfdaId = Lens.field @"id"
{-# INLINEABLE slfdaId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The name, or ARN, of the Lambda function to schedule.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slfdaName :: Lens.Lens' ScheduleLambdaFunctionDecisionAttributes Types.FunctionName
slfdaName = Lens.field @"name"
{-# INLINEABLE slfdaName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The data attached to the event that the decider can use in subsequent workflow tasks. This data isn't sent to the Lambda task.
--
-- /Note:/ Consider using 'control' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slfdaControl :: Lens.Lens' ScheduleLambdaFunctionDecisionAttributes (Core.Maybe Types.Data)
slfdaControl = Lens.field @"control"
{-# INLINEABLE slfdaControl #-}
{-# DEPRECATED control "Use generic-lens or generic-optics with 'control' instead"  #-}

-- | The optional input data to be supplied to the Lambda function.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slfdaInput :: Lens.Lens' ScheduleLambdaFunctionDecisionAttributes (Core.Maybe Types.FunctionInput)
slfdaInput = Lens.field @"input"
{-# INLINEABLE slfdaInput #-}
{-# DEPRECATED input "Use generic-lens or generic-optics with 'input' instead"  #-}

-- | The timeout value, in seconds, after which the Lambda function is considered to be failed once it has started. This can be any integer from 1-300 (1s-5m). If no value is supplied, than a default value of 300s is assumed.
--
-- /Note:/ Consider using 'startToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slfdaStartToCloseTimeout :: Lens.Lens' ScheduleLambdaFunctionDecisionAttributes (Core.Maybe Types.DurationInSecondsOptional)
slfdaStartToCloseTimeout = Lens.field @"startToCloseTimeout"
{-# INLINEABLE slfdaStartToCloseTimeout #-}
{-# DEPRECATED startToCloseTimeout "Use generic-lens or generic-optics with 'startToCloseTimeout' instead"  #-}

instance Core.FromJSON ScheduleLambdaFunctionDecisionAttributes
         where
        toJSON ScheduleLambdaFunctionDecisionAttributes{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("id" Core..= id), Core.Just ("name" Core..= name),
                  ("control" Core..=) Core.<$> control,
                  ("input" Core..=) Core.<$> input,
                  ("startToCloseTimeout" Core..=) Core.<$> startToCloseTimeout])
