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
-- Module      : Amazonka.SWF.Types.ScheduleLambdaFunctionDecisionAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.ScheduleLambdaFunctionDecisionAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Decision attributes specified in
-- @scheduleLambdaFunctionDecisionAttributes@ within the list of decisions
-- @decisions@ passed to RespondDecisionTaskCompleted.
--
-- /See:/ 'newScheduleLambdaFunctionDecisionAttributes' smart constructor.
data ScheduleLambdaFunctionDecisionAttributes = ScheduleLambdaFunctionDecisionAttributes'
  { -- | The data attached to the event that the decider can use in subsequent
    -- workflow tasks. This data isn\'t sent to the Lambda task.
    control :: Prelude.Maybe Prelude.Text,
    -- | The optional input data to be supplied to the Lambda function.
    input :: Prelude.Maybe Prelude.Text,
    -- | The timeout value, in seconds, after which the Lambda function is
    -- considered to be failed once it has started. This can be any integer
    -- from 1-900 (1s-15m).
    --
    -- If no value is supplied, then a default value of 900s is assumed.
    startToCloseTimeout :: Prelude.Maybe Prelude.Text,
    -- | A string that identifies the Lambda function execution in the event
    -- history.
    id :: Prelude.Text,
    -- | The name, or ARN, of the Lambda function to schedule.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduleLambdaFunctionDecisionAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'control', 'scheduleLambdaFunctionDecisionAttributes_control' - The data attached to the event that the decider can use in subsequent
-- workflow tasks. This data isn\'t sent to the Lambda task.
--
-- 'input', 'scheduleLambdaFunctionDecisionAttributes_input' - The optional input data to be supplied to the Lambda function.
--
-- 'startToCloseTimeout', 'scheduleLambdaFunctionDecisionAttributes_startToCloseTimeout' - The timeout value, in seconds, after which the Lambda function is
-- considered to be failed once it has started. This can be any integer
-- from 1-900 (1s-15m).
--
-- If no value is supplied, then a default value of 900s is assumed.
--
-- 'id', 'scheduleLambdaFunctionDecisionAttributes_id' - A string that identifies the Lambda function execution in the event
-- history.
--
-- 'name', 'scheduleLambdaFunctionDecisionAttributes_name' - The name, or ARN, of the Lambda function to schedule.
newScheduleLambdaFunctionDecisionAttributes ::
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  ScheduleLambdaFunctionDecisionAttributes
newScheduleLambdaFunctionDecisionAttributes
  pId_
  pName_ =
    ScheduleLambdaFunctionDecisionAttributes'
      { control =
          Prelude.Nothing,
        input = Prelude.Nothing,
        startToCloseTimeout =
          Prelude.Nothing,
        id = pId_,
        name = pName_
      }

-- | The data attached to the event that the decider can use in subsequent
-- workflow tasks. This data isn\'t sent to the Lambda task.
scheduleLambdaFunctionDecisionAttributes_control :: Lens.Lens' ScheduleLambdaFunctionDecisionAttributes (Prelude.Maybe Prelude.Text)
scheduleLambdaFunctionDecisionAttributes_control = Lens.lens (\ScheduleLambdaFunctionDecisionAttributes' {control} -> control) (\s@ScheduleLambdaFunctionDecisionAttributes' {} a -> s {control = a} :: ScheduleLambdaFunctionDecisionAttributes)

-- | The optional input data to be supplied to the Lambda function.
scheduleLambdaFunctionDecisionAttributes_input :: Lens.Lens' ScheduleLambdaFunctionDecisionAttributes (Prelude.Maybe Prelude.Text)
scheduleLambdaFunctionDecisionAttributes_input = Lens.lens (\ScheduleLambdaFunctionDecisionAttributes' {input} -> input) (\s@ScheduleLambdaFunctionDecisionAttributes' {} a -> s {input = a} :: ScheduleLambdaFunctionDecisionAttributes)

-- | The timeout value, in seconds, after which the Lambda function is
-- considered to be failed once it has started. This can be any integer
-- from 1-900 (1s-15m).
--
-- If no value is supplied, then a default value of 900s is assumed.
scheduleLambdaFunctionDecisionAttributes_startToCloseTimeout :: Lens.Lens' ScheduleLambdaFunctionDecisionAttributes (Prelude.Maybe Prelude.Text)
scheduleLambdaFunctionDecisionAttributes_startToCloseTimeout = Lens.lens (\ScheduleLambdaFunctionDecisionAttributes' {startToCloseTimeout} -> startToCloseTimeout) (\s@ScheduleLambdaFunctionDecisionAttributes' {} a -> s {startToCloseTimeout = a} :: ScheduleLambdaFunctionDecisionAttributes)

-- | A string that identifies the Lambda function execution in the event
-- history.
scheduleLambdaFunctionDecisionAttributes_id :: Lens.Lens' ScheduleLambdaFunctionDecisionAttributes Prelude.Text
scheduleLambdaFunctionDecisionAttributes_id = Lens.lens (\ScheduleLambdaFunctionDecisionAttributes' {id} -> id) (\s@ScheduleLambdaFunctionDecisionAttributes' {} a -> s {id = a} :: ScheduleLambdaFunctionDecisionAttributes)

-- | The name, or ARN, of the Lambda function to schedule.
scheduleLambdaFunctionDecisionAttributes_name :: Lens.Lens' ScheduleLambdaFunctionDecisionAttributes Prelude.Text
scheduleLambdaFunctionDecisionAttributes_name = Lens.lens (\ScheduleLambdaFunctionDecisionAttributes' {name} -> name) (\s@ScheduleLambdaFunctionDecisionAttributes' {} a -> s {name = a} :: ScheduleLambdaFunctionDecisionAttributes)

instance
  Prelude.Hashable
    ScheduleLambdaFunctionDecisionAttributes
  where
  hashWithSalt
    _salt
    ScheduleLambdaFunctionDecisionAttributes' {..} =
      _salt
        `Prelude.hashWithSalt` control
        `Prelude.hashWithSalt` input
        `Prelude.hashWithSalt` startToCloseTimeout
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` name

instance
  Prelude.NFData
    ScheduleLambdaFunctionDecisionAttributes
  where
  rnf ScheduleLambdaFunctionDecisionAttributes' {..} =
    Prelude.rnf control
      `Prelude.seq` Prelude.rnf input
      `Prelude.seq` Prelude.rnf startToCloseTimeout
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name

instance
  Data.ToJSON
    ScheduleLambdaFunctionDecisionAttributes
  where
  toJSON ScheduleLambdaFunctionDecisionAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("control" Data..=) Prelude.<$> control,
            ("input" Data..=) Prelude.<$> input,
            ("startToCloseTimeout" Data..=)
              Prelude.<$> startToCloseTimeout,
            Prelude.Just ("id" Data..= id),
            Prelude.Just ("name" Data..= name)
          ]
      )
