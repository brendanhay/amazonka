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
-- Module      : Amazonka.SWF.Types.LambdaFunctionScheduledEventAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.LambdaFunctionScheduledEventAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the details of the @LambdaFunctionScheduled@ event. It isn\'t
-- set for other event types.
--
-- /See:/ 'newLambdaFunctionScheduledEventAttributes' smart constructor.
data LambdaFunctionScheduledEventAttributes = LambdaFunctionScheduledEventAttributes'
  { -- | Data attached to the event that the decider can use in subsequent
    -- workflow tasks. This data isn\'t sent to the Lambda task.
    control :: Prelude.Maybe Prelude.Text,
    -- | The input provided to the Lambda task.
    input :: Prelude.Maybe Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaFunctionScheduledEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'control', 'lambdaFunctionScheduledEventAttributes_control' - Data attached to the event that the decider can use in subsequent
-- workflow tasks. This data isn\'t sent to the Lambda task.
--
-- 'input', 'lambdaFunctionScheduledEventAttributes_input' - The input provided to the Lambda task.
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
      { control =
          Prelude.Nothing,
        input = Prelude.Nothing,
        startToCloseTimeout =
          Prelude.Nothing,
        id = pId_,
        name = pName_,
        decisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | Data attached to the event that the decider can use in subsequent
-- workflow tasks. This data isn\'t sent to the Lambda task.
lambdaFunctionScheduledEventAttributes_control :: Lens.Lens' LambdaFunctionScheduledEventAttributes (Prelude.Maybe Prelude.Text)
lambdaFunctionScheduledEventAttributes_control = Lens.lens (\LambdaFunctionScheduledEventAttributes' {control} -> control) (\s@LambdaFunctionScheduledEventAttributes' {} a -> s {control = a} :: LambdaFunctionScheduledEventAttributes)

-- | The input provided to the Lambda task.
lambdaFunctionScheduledEventAttributes_input :: Lens.Lens' LambdaFunctionScheduledEventAttributes (Prelude.Maybe Prelude.Text)
lambdaFunctionScheduledEventAttributes_input = Lens.lens (\LambdaFunctionScheduledEventAttributes' {input} -> input) (\s@LambdaFunctionScheduledEventAttributes' {} a -> s {input = a} :: LambdaFunctionScheduledEventAttributes)

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
  Data.FromJSON
    LambdaFunctionScheduledEventAttributes
  where
  parseJSON =
    Data.withObject
      "LambdaFunctionScheduledEventAttributes"
      ( \x ->
          LambdaFunctionScheduledEventAttributes'
            Prelude.<$> (x Data..:? "control")
            Prelude.<*> (x Data..:? "input")
            Prelude.<*> (x Data..:? "startToCloseTimeout")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "decisionTaskCompletedEventId")
      )

instance
  Prelude.Hashable
    LambdaFunctionScheduledEventAttributes
  where
  hashWithSalt
    _salt
    LambdaFunctionScheduledEventAttributes' {..} =
      _salt
        `Prelude.hashWithSalt` control
        `Prelude.hashWithSalt` input
        `Prelude.hashWithSalt` startToCloseTimeout
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` decisionTaskCompletedEventId

instance
  Prelude.NFData
    LambdaFunctionScheduledEventAttributes
  where
  rnf LambdaFunctionScheduledEventAttributes' {..} =
    Prelude.rnf control
      `Prelude.seq` Prelude.rnf input
      `Prelude.seq` Prelude.rnf startToCloseTimeout
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf decisionTaskCompletedEventId
