{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.StartLambdaFunctionFailedEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.StartLambdaFunctionFailedEventAttributes where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SWF.Types.StartLambdaFunctionFailedCause

-- | Provides the details of the @StartLambdaFunctionFailed@ event. It isn\'t
-- set for other event types.
--
-- /See:/ 'newStartLambdaFunctionFailedEventAttributes' smart constructor.
data StartLambdaFunctionFailedEventAttributes = StartLambdaFunctionFailedEventAttributes'
  { -- | A description that can help diagnose the cause of the fault.
    message :: Core.Maybe Core.Text,
    -- | The ID of the @ActivityTaskScheduled@ event that was recorded when this
    -- activity task was scheduled. To help diagnose issues, use this
    -- information to trace back the chain of events leading up to this event.
    scheduledEventId :: Core.Maybe Core.Integer,
    -- | The cause of the failure. To help diagnose issues, use this information
    -- to trace back the chain of events leading up to this event.
    --
    -- If @cause@ is set to @OPERATION_NOT_PERMITTED@, the decision failed
    -- because the IAM role attached to the execution lacked sufficient
    -- permissions. For details and example IAM policies, see
    -- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/lambda-task.html Lambda Tasks>
    -- in the /Amazon SWF Developer Guide/.
    cause :: Core.Maybe StartLambdaFunctionFailedCause
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartLambdaFunctionFailedEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'startLambdaFunctionFailedEventAttributes_message' - A description that can help diagnose the cause of the fault.
--
-- 'scheduledEventId', 'startLambdaFunctionFailedEventAttributes_scheduledEventId' - The ID of the @ActivityTaskScheduled@ event that was recorded when this
-- activity task was scheduled. To help diagnose issues, use this
-- information to trace back the chain of events leading up to this event.
--
-- 'cause', 'startLambdaFunctionFailedEventAttributes_cause' - The cause of the failure. To help diagnose issues, use this information
-- to trace back the chain of events leading up to this event.
--
-- If @cause@ is set to @OPERATION_NOT_PERMITTED@, the decision failed
-- because the IAM role attached to the execution lacked sufficient
-- permissions. For details and example IAM policies, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/lambda-task.html Lambda Tasks>
-- in the /Amazon SWF Developer Guide/.
newStartLambdaFunctionFailedEventAttributes ::
  StartLambdaFunctionFailedEventAttributes
newStartLambdaFunctionFailedEventAttributes =
  StartLambdaFunctionFailedEventAttributes'
    { message =
        Core.Nothing,
      scheduledEventId = Core.Nothing,
      cause = Core.Nothing
    }

-- | A description that can help diagnose the cause of the fault.
startLambdaFunctionFailedEventAttributes_message :: Lens.Lens' StartLambdaFunctionFailedEventAttributes (Core.Maybe Core.Text)
startLambdaFunctionFailedEventAttributes_message = Lens.lens (\StartLambdaFunctionFailedEventAttributes' {message} -> message) (\s@StartLambdaFunctionFailedEventAttributes' {} a -> s {message = a} :: StartLambdaFunctionFailedEventAttributes)

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this
-- activity task was scheduled. To help diagnose issues, use this
-- information to trace back the chain of events leading up to this event.
startLambdaFunctionFailedEventAttributes_scheduledEventId :: Lens.Lens' StartLambdaFunctionFailedEventAttributes (Core.Maybe Core.Integer)
startLambdaFunctionFailedEventAttributes_scheduledEventId = Lens.lens (\StartLambdaFunctionFailedEventAttributes' {scheduledEventId} -> scheduledEventId) (\s@StartLambdaFunctionFailedEventAttributes' {} a -> s {scheduledEventId = a} :: StartLambdaFunctionFailedEventAttributes)

-- | The cause of the failure. To help diagnose issues, use this information
-- to trace back the chain of events leading up to this event.
--
-- If @cause@ is set to @OPERATION_NOT_PERMITTED@, the decision failed
-- because the IAM role attached to the execution lacked sufficient
-- permissions. For details and example IAM policies, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/lambda-task.html Lambda Tasks>
-- in the /Amazon SWF Developer Guide/.
startLambdaFunctionFailedEventAttributes_cause :: Lens.Lens' StartLambdaFunctionFailedEventAttributes (Core.Maybe StartLambdaFunctionFailedCause)
startLambdaFunctionFailedEventAttributes_cause = Lens.lens (\StartLambdaFunctionFailedEventAttributes' {cause} -> cause) (\s@StartLambdaFunctionFailedEventAttributes' {} a -> s {cause = a} :: StartLambdaFunctionFailedEventAttributes)

instance
  Core.FromJSON
    StartLambdaFunctionFailedEventAttributes
  where
  parseJSON =
    Core.withObject
      "StartLambdaFunctionFailedEventAttributes"
      ( \x ->
          StartLambdaFunctionFailedEventAttributes'
            Core.<$> (x Core..:? "message")
            Core.<*> (x Core..:? "scheduledEventId")
            Core.<*> (x Core..:? "cause")
      )

instance
  Core.Hashable
    StartLambdaFunctionFailedEventAttributes

instance
  Core.NFData
    StartLambdaFunctionFailedEventAttributes
