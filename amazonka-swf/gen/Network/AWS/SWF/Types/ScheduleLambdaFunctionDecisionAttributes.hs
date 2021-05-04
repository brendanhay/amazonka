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
-- Module      : Network.AWS.SWF.Types.ScheduleLambdaFunctionDecisionAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ScheduleLambdaFunctionDecisionAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Decision attributes specified in
-- @scheduleLambdaFunctionDecisionAttributes@ within the list of decisions
-- @decisions@ passed to RespondDecisionTaskCompleted.
--
-- /See:/ 'newScheduleLambdaFunctionDecisionAttributes' smart constructor.
data ScheduleLambdaFunctionDecisionAttributes = ScheduleLambdaFunctionDecisionAttributes'
  { -- | The optional input data to be supplied to the Lambda function.
    input :: Prelude.Maybe Prelude.Text,
    -- | The data attached to the event that the decider can use in subsequent
    -- workflow tasks. This data isn\'t sent to the Lambda task.
    control :: Prelude.Maybe Prelude.Text,
    -- | The timeout value, in seconds, after which the Lambda function is
    -- considered to be failed once it has started. This can be any integer
    -- from 1-300 (1s-5m). If no value is supplied, than a default value of
    -- 300s is assumed.
    startToCloseTimeout :: Prelude.Maybe Prelude.Text,
    -- | A string that identifies the Lambda function execution in the event
    -- history.
    id :: Prelude.Text,
    -- | The name, or ARN, of the Lambda function to schedule.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ScheduleLambdaFunctionDecisionAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'input', 'scheduleLambdaFunctionDecisionAttributes_input' - The optional input data to be supplied to the Lambda function.
--
-- 'control', 'scheduleLambdaFunctionDecisionAttributes_control' - The data attached to the event that the decider can use in subsequent
-- workflow tasks. This data isn\'t sent to the Lambda task.
--
-- 'startToCloseTimeout', 'scheduleLambdaFunctionDecisionAttributes_startToCloseTimeout' - The timeout value, in seconds, after which the Lambda function is
-- considered to be failed once it has started. This can be any integer
-- from 1-300 (1s-5m). If no value is supplied, than a default value of
-- 300s is assumed.
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
      { input =
          Prelude.Nothing,
        control = Prelude.Nothing,
        startToCloseTimeout =
          Prelude.Nothing,
        id = pId_,
        name = pName_
      }

-- | The optional input data to be supplied to the Lambda function.
scheduleLambdaFunctionDecisionAttributes_input :: Lens.Lens' ScheduleLambdaFunctionDecisionAttributes (Prelude.Maybe Prelude.Text)
scheduleLambdaFunctionDecisionAttributes_input = Lens.lens (\ScheduleLambdaFunctionDecisionAttributes' {input} -> input) (\s@ScheduleLambdaFunctionDecisionAttributes' {} a -> s {input = a} :: ScheduleLambdaFunctionDecisionAttributes)

-- | The data attached to the event that the decider can use in subsequent
-- workflow tasks. This data isn\'t sent to the Lambda task.
scheduleLambdaFunctionDecisionAttributes_control :: Lens.Lens' ScheduleLambdaFunctionDecisionAttributes (Prelude.Maybe Prelude.Text)
scheduleLambdaFunctionDecisionAttributes_control = Lens.lens (\ScheduleLambdaFunctionDecisionAttributes' {control} -> control) (\s@ScheduleLambdaFunctionDecisionAttributes' {} a -> s {control = a} :: ScheduleLambdaFunctionDecisionAttributes)

-- | The timeout value, in seconds, after which the Lambda function is
-- considered to be failed once it has started. This can be any integer
-- from 1-300 (1s-5m). If no value is supplied, than a default value of
-- 300s is assumed.
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

instance
  Prelude.NFData
    ScheduleLambdaFunctionDecisionAttributes

instance
  Prelude.ToJSON
    ScheduleLambdaFunctionDecisionAttributes
  where
  toJSON ScheduleLambdaFunctionDecisionAttributes' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("input" Prelude..=) Prelude.<$> input,
            ("control" Prelude..=) Prelude.<$> control,
            ("startToCloseTimeout" Prelude..=)
              Prelude.<$> startToCloseTimeout,
            Prelude.Just ("id" Prelude..= id),
            Prelude.Just ("name" Prelude..= name)
          ]
      )
