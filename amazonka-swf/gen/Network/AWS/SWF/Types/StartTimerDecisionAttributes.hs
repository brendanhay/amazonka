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
-- Module      : Network.AWS.SWF.Types.StartTimerDecisionAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.StartTimerDecisionAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides the details of the @StartTimer@ decision.
--
-- __Access Control__
--
-- You can use IAM policies to control this decision\'s access to Amazon
-- SWF resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
--
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
--
-- -   You cannot use an IAM policy to constrain this action\'s parameters.
--
-- If the caller doesn\'t have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s @cause@ parameter is set
-- to @OPERATION_NOT_PERMITTED@. For details and example IAM policies, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>
-- in the /Amazon SWF Developer Guide/.
--
-- /See:/ 'newStartTimerDecisionAttributes' smart constructor.
data StartTimerDecisionAttributes = StartTimerDecisionAttributes'
  { -- | The data attached to the event that can be used by the decider in
    -- subsequent workflow tasks.
    control :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the timer.
    --
    -- The specified string must not start or end with whitespace. It must not
    -- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
    -- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
    -- contain the literal string @arn@.
    timerId :: Prelude.Text,
    -- | The duration to wait before firing the timer.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@.
    startToFireTimeout :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartTimerDecisionAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'control', 'startTimerDecisionAttributes_control' - The data attached to the event that can be used by the decider in
-- subsequent workflow tasks.
--
-- 'timerId', 'startTimerDecisionAttributes_timerId' - The unique ID of the timer.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
-- contain the literal string @arn@.
--
-- 'startToFireTimeout', 'startTimerDecisionAttributes_startToFireTimeout' - The duration to wait before firing the timer.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@.
newStartTimerDecisionAttributes ::
  -- | 'timerId'
  Prelude.Text ->
  -- | 'startToFireTimeout'
  Prelude.Text ->
  StartTimerDecisionAttributes
newStartTimerDecisionAttributes
  pTimerId_
  pStartToFireTimeout_ =
    StartTimerDecisionAttributes'
      { control =
          Prelude.Nothing,
        timerId = pTimerId_,
        startToFireTimeout = pStartToFireTimeout_
      }

-- | The data attached to the event that can be used by the decider in
-- subsequent workflow tasks.
startTimerDecisionAttributes_control :: Lens.Lens' StartTimerDecisionAttributes (Prelude.Maybe Prelude.Text)
startTimerDecisionAttributes_control = Lens.lens (\StartTimerDecisionAttributes' {control} -> control) (\s@StartTimerDecisionAttributes' {} a -> s {control = a} :: StartTimerDecisionAttributes)

-- | The unique ID of the timer.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
-- contain the literal string @arn@.
startTimerDecisionAttributes_timerId :: Lens.Lens' StartTimerDecisionAttributes Prelude.Text
startTimerDecisionAttributes_timerId = Lens.lens (\StartTimerDecisionAttributes' {timerId} -> timerId) (\s@StartTimerDecisionAttributes' {} a -> s {timerId = a} :: StartTimerDecisionAttributes)

-- | The duration to wait before firing the timer.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@.
startTimerDecisionAttributes_startToFireTimeout :: Lens.Lens' StartTimerDecisionAttributes Prelude.Text
startTimerDecisionAttributes_startToFireTimeout = Lens.lens (\StartTimerDecisionAttributes' {startToFireTimeout} -> startToFireTimeout) (\s@StartTimerDecisionAttributes' {} a -> s {startToFireTimeout = a} :: StartTimerDecisionAttributes)

instance
  Prelude.Hashable
    StartTimerDecisionAttributes

instance Prelude.NFData StartTimerDecisionAttributes

instance Prelude.ToJSON StartTimerDecisionAttributes where
  toJSON StartTimerDecisionAttributes' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("control" Prelude..=) Prelude.<$> control,
            Prelude.Just ("timerId" Prelude..= timerId),
            Prelude.Just
              ( "startToFireTimeout"
                  Prelude..= startToFireTimeout
              )
          ]
      )
