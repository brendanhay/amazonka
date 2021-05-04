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
-- Module      : Network.AWS.SWF.Types.LambdaFunctionScheduledEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.LambdaFunctionScheduledEventAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides the details of the @LambdaFunctionScheduled@ event. It isn\'t
-- set for other event types.
--
-- /See:/ 'newLambdaFunctionScheduledEventAttributes' smart constructor.
data LambdaFunctionScheduledEventAttributes = LambdaFunctionScheduledEventAttributes'
  { -- | The input provided to the Lambda task.
    input :: Prelude.Maybe Prelude.Text,
    -- | Data attached to the event that the decider can use in subsequent
    -- workflow tasks. This data isn\'t sent to the Lambda task.
    control :: Prelude.Maybe Prelude.Text,
    -- | The maximum amount of time a worker can take to process the Lambda task.
    startToCloseTimeout :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the Lambda task.
    id :: Prelude.Text,
    -- | The name of the Lambda function.
    name :: Prelude.Text,
    -- | The ID of the @LambdaFunctionCompleted@ event corresponding to the
    -- decision that resulted in scheduling this activity task. To help
    -- diagnose issues, use this information to trace back the chain of events
    -- leading up to this event.
    decisionTaskCompletedEventId :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LambdaFunctionScheduledEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'input', 'lambdaFunctionScheduledEventAttributes_input' - The input provided to the Lambda task.
--
-- 'control', 'lambdaFunctionScheduledEventAttributes_control' - Data attached to the event that the decider can use in subsequent
-- workflow tasks. This data isn\'t sent to the Lambda task.
--
-- 'startToCloseTimeout', 'lambdaFunctionScheduledEventAttributes_startToCloseTimeout' - The maximum amount of time a worker can take to process the Lambda task.
--
-- 'id', 'lambdaFunctionScheduledEventAttributes_id' - The unique ID of the Lambda task.
--
-- 'name', 'lambdaFunctionScheduledEventAttributes_name' - The name of the Lambda function.
--
-- 'decisionTaskCompletedEventId', 'lambdaFunctionScheduledEventAttributes_decisionTaskCompletedEventId' - The ID of the @LambdaFunctionCompleted@ event corresponding to the
-- decision that resulted in scheduling this activity task. To help
-- diagnose issues, use this information to trace back the chain of events
-- leading up to this event.
newLambdaFunctionScheduledEventAttributes ::
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'decisionTaskCompletedEventId'
  Prelude.Integer ->
  LambdaFunctionScheduledEventAttributes
newLambdaFunctionScheduledEventAttributes
  pId_
  pName_
  pDecisionTaskCompletedEventId_ =
    LambdaFunctionScheduledEventAttributes'
      { input =
          Prelude.Nothing,
        control = Prelude.Nothing,
        startToCloseTimeout =
          Prelude.Nothing,
        id = pId_,
        name = pName_,
        decisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | The input provided to the Lambda task.
lambdaFunctionScheduledEventAttributes_input :: Lens.Lens' LambdaFunctionScheduledEventAttributes (Prelude.Maybe Prelude.Text)
lambdaFunctionScheduledEventAttributes_input = Lens.lens (\LambdaFunctionScheduledEventAttributes' {input} -> input) (\s@LambdaFunctionScheduledEventAttributes' {} a -> s {input = a} :: LambdaFunctionScheduledEventAttributes)

-- | Data attached to the event that the decider can use in subsequent
-- workflow tasks. This data isn\'t sent to the Lambda task.
lambdaFunctionScheduledEventAttributes_control :: Lens.Lens' LambdaFunctionScheduledEventAttributes (Prelude.Maybe Prelude.Text)
lambdaFunctionScheduledEventAttributes_control = Lens.lens (\LambdaFunctionScheduledEventAttributes' {control} -> control) (\s@LambdaFunctionScheduledEventAttributes' {} a -> s {control = a} :: LambdaFunctionScheduledEventAttributes)

-- | The maximum amount of time a worker can take to process the Lambda task.
lambdaFunctionScheduledEventAttributes_startToCloseTimeout :: Lens.Lens' LambdaFunctionScheduledEventAttributes (Prelude.Maybe Prelude.Text)
lambdaFunctionScheduledEventAttributes_startToCloseTimeout = Lens.lens (\LambdaFunctionScheduledEventAttributes' {startToCloseTimeout} -> startToCloseTimeout) (\s@LambdaFunctionScheduledEventAttributes' {} a -> s {startToCloseTimeout = a} :: LambdaFunctionScheduledEventAttributes)

-- | The unique ID of the Lambda task.
lambdaFunctionScheduledEventAttributes_id :: Lens.Lens' LambdaFunctionScheduledEventAttributes Prelude.Text
lambdaFunctionScheduledEventAttributes_id = Lens.lens (\LambdaFunctionScheduledEventAttributes' {id} -> id) (\s@LambdaFunctionScheduledEventAttributes' {} a -> s {id = a} :: LambdaFunctionScheduledEventAttributes)

-- | The name of the Lambda function.
lambdaFunctionScheduledEventAttributes_name :: Lens.Lens' LambdaFunctionScheduledEventAttributes Prelude.Text
lambdaFunctionScheduledEventAttributes_name = Lens.lens (\LambdaFunctionScheduledEventAttributes' {name} -> name) (\s@LambdaFunctionScheduledEventAttributes' {} a -> s {name = a} :: LambdaFunctionScheduledEventAttributes)

-- | The ID of the @LambdaFunctionCompleted@ event corresponding to the
-- decision that resulted in scheduling this activity task. To help
-- diagnose issues, use this information to trace back the chain of events
-- leading up to this event.
lambdaFunctionScheduledEventAttributes_decisionTaskCompletedEventId :: Lens.Lens' LambdaFunctionScheduledEventAttributes Prelude.Integer
lambdaFunctionScheduledEventAttributes_decisionTaskCompletedEventId = Lens.lens (\LambdaFunctionScheduledEventAttributes' {decisionTaskCompletedEventId} -> decisionTaskCompletedEventId) (\s@LambdaFunctionScheduledEventAttributes' {} a -> s {decisionTaskCompletedEventId = a} :: LambdaFunctionScheduledEventAttributes)

instance
  Prelude.FromJSON
    LambdaFunctionScheduledEventAttributes
  where
  parseJSON =
    Prelude.withObject
      "LambdaFunctionScheduledEventAttributes"
      ( \x ->
          LambdaFunctionScheduledEventAttributes'
            Prelude.<$> (x Prelude..:? "input")
            Prelude.<*> (x Prelude..:? "control")
            Prelude.<*> (x Prelude..:? "startToCloseTimeout")
            Prelude.<*> (x Prelude..: "id")
            Prelude.<*> (x Prelude..: "name")
            Prelude.<*> (x Prelude..: "decisionTaskCompletedEventId")
      )

instance
  Prelude.Hashable
    LambdaFunctionScheduledEventAttributes

instance
  Prelude.NFData
    LambdaFunctionScheduledEventAttributes
