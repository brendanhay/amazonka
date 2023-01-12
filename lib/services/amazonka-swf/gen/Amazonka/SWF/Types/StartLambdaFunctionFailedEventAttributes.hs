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
-- Module      : Amazonka.SWF.Types.StartLambdaFunctionFailedEventAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.StartLambdaFunctionFailedEventAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SWF.Types.StartLambdaFunctionFailedCause

-- | Provides the details of the @StartLambdaFunctionFailed@ event. It isn\'t
-- set for other event types.
--
-- /See:/ 'newStartLambdaFunctionFailedEventAttributes' smart constructor.
data StartLambdaFunctionFailedEventAttributes = StartLambdaFunctionFailedEventAttributes'
  { -- | The cause of the failure. To help diagnose issues, use this information
    -- to trace back the chain of events leading up to this event.
    --
    -- If @cause@ is set to @OPERATION_NOT_PERMITTED@, the decision failed
    -- because the IAM role attached to the execution lacked sufficient
    -- permissions. For details and example IAM policies, see
    -- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/lambda-task.html Lambda Tasks>
    -- in the /Amazon SWF Developer Guide/.
    cause :: Prelude.Maybe StartLambdaFunctionFailedCause,
    -- | A description that can help diagnose the cause of the fault.
    message :: Prelude.Maybe Prelude.Text,
    -- | The ID of the @ActivityTaskScheduled@ event that was recorded when this
    -- activity task was scheduled. To help diagnose issues, use this
    -- information to trace back the chain of events leading up to this event.
    scheduledEventId :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartLambdaFunctionFailedEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cause', 'startLambdaFunctionFailedEventAttributes_cause' - The cause of the failure. To help diagnose issues, use this information
-- to trace back the chain of events leading up to this event.
--
-- If @cause@ is set to @OPERATION_NOT_PERMITTED@, the decision failed
-- because the IAM role attached to the execution lacked sufficient
-- permissions. For details and example IAM policies, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/lambda-task.html Lambda Tasks>
-- in the /Amazon SWF Developer Guide/.
--
-- 'message', 'startLambdaFunctionFailedEventAttributes_message' - A description that can help diagnose the cause of the fault.
--
-- 'scheduledEventId', 'startLambdaFunctionFailedEventAttributes_scheduledEventId' - The ID of the @ActivityTaskScheduled@ event that was recorded when this
-- activity task was scheduled. To help diagnose issues, use this
-- information to trace back the chain of events leading up to this event.
newStartLambdaFunctionFailedEventAttributes ::
  StartLambdaFunctionFailedEventAttributes
newStartLambdaFunctionFailedEventAttributes =
  StartLambdaFunctionFailedEventAttributes'
    { cause =
        Prelude.Nothing,
      message = Prelude.Nothing,
      scheduledEventId =
        Prelude.Nothing
    }

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

-- | A description that can help diagnose the cause of the fault.
startLambdaFunctionFailedEventAttributes_message :: Lens.Lens' StartLambdaFunctionFailedEventAttributes (Prelude.Maybe Prelude.Text)
startLambdaFunctionFailedEventAttributes_message = Lens.lens (\StartLambdaFunctionFailedEventAttributes' {message} -> message) (\s@StartLambdaFunctionFailedEventAttributes' {} a -> s {message = a} :: StartLambdaFunctionFailedEventAttributes)

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this
-- activity task was scheduled. To help diagnose issues, use this
-- information to trace back the chain of events leading up to this event.
startLambdaFunctionFailedEventAttributes_scheduledEventId :: Lens.Lens' StartLambdaFunctionFailedEventAttributes (Prelude.Maybe Prelude.Integer)
startLambdaFunctionFailedEventAttributes_scheduledEventId = Lens.lens (\StartLambdaFunctionFailedEventAttributes' {scheduledEventId} -> scheduledEventId) (\s@StartLambdaFunctionFailedEventAttributes' {} a -> s {scheduledEventId = a} :: StartLambdaFunctionFailedEventAttributes)

instance
  Data.FromJSON
    StartLambdaFunctionFailedEventAttributes
  where
  parseJSON =
    Data.withObject
      "StartLambdaFunctionFailedEventAttributes"
      ( \x ->
          StartLambdaFunctionFailedEventAttributes'
            Prelude.<$> (x Data..:? "cause")
            Prelude.<*> (x Data..:? "message")
            Prelude.<*> (x Data..:? "scheduledEventId")
      )

instance
  Prelude.Hashable
    StartLambdaFunctionFailedEventAttributes
  where
  hashWithSalt
    _salt
    StartLambdaFunctionFailedEventAttributes' {..} =
      _salt `Prelude.hashWithSalt` cause
        `Prelude.hashWithSalt` message
        `Prelude.hashWithSalt` scheduledEventId

instance
  Prelude.NFData
    StartLambdaFunctionFailedEventAttributes
  where
  rnf StartLambdaFunctionFailedEventAttributes' {..} =
    Prelude.rnf cause
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf scheduledEventId
