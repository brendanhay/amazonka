{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SWF.Types.StartLambdaFunctionFailedCause

-- | Provides the details of the @StartLambdaFunctionFailed@ event. It isn\'t
-- set for other event types.
--
-- /See:/ 'newStartLambdaFunctionFailedEventAttributes' smart constructor.
data StartLambdaFunctionFailedEventAttributes = StartLambdaFunctionFailedEventAttributes'
  { -- | A description that can help diagnose the cause of the fault.
    message :: Prelude.Maybe Prelude.Text,
    -- | The ID of the @ActivityTaskScheduled@ event that was recorded when this
    -- activity task was scheduled. To help diagnose issues, use this
    -- information to trace back the chain of events leading up to this event.
    scheduledEventId :: Prelude.Maybe Prelude.Integer,
    -- | The cause of the failure. To help diagnose issues, use this information
    -- to trace back the chain of events leading up to this event.
    --
    -- If @cause@ is set to @OPERATION_NOT_PERMITTED@, the decision failed
    -- because the IAM role attached to the execution lacked sufficient
    -- permissions. For details and example IAM policies, see
    -- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/lambda-task.html Lambda Tasks>
    -- in the /Amazon SWF Developer Guide/.
    cause :: Prelude.Maybe StartLambdaFunctionFailedCause
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      scheduledEventId =
        Prelude.Nothing,
      cause = Prelude.Nothing
    }

-- | A description that can help diagnose the cause of the fault.
startLambdaFunctionFailedEventAttributes_message :: Lens.Lens' StartLambdaFunctionFailedEventAttributes (Prelude.Maybe Prelude.Text)
startLambdaFunctionFailedEventAttributes_message = Lens.lens (\StartLambdaFunctionFailedEventAttributes' {message} -> message) (\s@StartLambdaFunctionFailedEventAttributes' {} a -> s {message = a} :: StartLambdaFunctionFailedEventAttributes)

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this
-- activity task was scheduled. To help diagnose issues, use this
-- information to trace back the chain of events leading up to this event.
startLambdaFunctionFailedEventAttributes_scheduledEventId :: Lens.Lens' StartLambdaFunctionFailedEventAttributes (Prelude.Maybe Prelude.Integer)
startLambdaFunctionFailedEventAttributes_scheduledEventId = Lens.lens (\StartLambdaFunctionFailedEventAttributes' {scheduledEventId} -> scheduledEventId) (\s@StartLambdaFunctionFailedEventAttributes' {} a -> s {scheduledEventId = a} :: StartLambdaFunctionFailedEventAttributes)

-- | The cause of the failure. To help diagnose issues, use this information
-- to trace back the chain of events leading up to this event.
--
-- If @cause@ is set to @OPERATION_NOT_PERMITTED@, the decision failed
-- because the IAM role attached to the execution lacked sufficient
-- permissions. For details and example IAM policies, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/lambda-task.html Lambda Tasks>
-- in the /Amazon SWF Developer Guide/.
startLambdaFunctionFailedEventAttributes_cause :: Lens.Lens' StartLambdaFunctionFailedEventAttributes (Prelude.Maybe StartLambdaFunctionFailedCause)
startLambdaFunctionFailedEventAttributes_cause = Lens.lens (\StartLambdaFunctionFailedEventAttributes' {cause} -> cause) (\s@StartLambdaFunctionFailedEventAttributes' {} a -> s {cause = a} :: StartLambdaFunctionFailedEventAttributes)

instance
  Prelude.FromJSON
    StartLambdaFunctionFailedEventAttributes
  where
  parseJSON =
    Prelude.withObject
      "StartLambdaFunctionFailedEventAttributes"
      ( \x ->
          StartLambdaFunctionFailedEventAttributes'
            Prelude.<$> (x Prelude..:? "message")
            Prelude.<*> (x Prelude..:? "scheduledEventId")
            Prelude.<*> (x Prelude..:? "cause")
      )

instance
  Prelude.Hashable
    StartLambdaFunctionFailedEventAttributes

instance
  Prelude.NFData
    StartLambdaFunctionFailedEventAttributes
