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
-- Module      : Amazonka.SWF.Types.LambdaFunctionCompletedEventAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.LambdaFunctionCompletedEventAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the details of the @LambdaFunctionCompleted@ event. It isn\'t
-- set for other event types.
--
-- /See:/ 'newLambdaFunctionCompletedEventAttributes' smart constructor.
data LambdaFunctionCompletedEventAttributes = LambdaFunctionCompletedEventAttributes'
  { -- | The results of the Lambda task.
    result :: Prelude.Maybe Prelude.Text,
    -- | The ID of the @LambdaFunctionScheduled@ event that was recorded when
    -- this Lambda task was scheduled. To help diagnose issues, use this
    -- information to trace back the chain of events leading up to this event.
    scheduledEventId :: Prelude.Integer,
    -- | The ID of the @LambdaFunctionStarted@ event recorded when this activity
    -- task started. To help diagnose issues, use this information to trace
    -- back the chain of events leading up to this event.
    startedEventId :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaFunctionCompletedEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'result', 'lambdaFunctionCompletedEventAttributes_result' - The results of the Lambda task.
--
-- 'scheduledEventId', 'lambdaFunctionCompletedEventAttributes_scheduledEventId' - The ID of the @LambdaFunctionScheduled@ event that was recorded when
-- this Lambda task was scheduled. To help diagnose issues, use this
-- information to trace back the chain of events leading up to this event.
--
-- 'startedEventId', 'lambdaFunctionCompletedEventAttributes_startedEventId' - The ID of the @LambdaFunctionStarted@ event recorded when this activity
-- task started. To help diagnose issues, use this information to trace
-- back the chain of events leading up to this event.
newLambdaFunctionCompletedEventAttributes ::
  -- | 'scheduledEventId'
  Prelude.Integer ->
  -- | 'startedEventId'
  Prelude.Integer ->
  LambdaFunctionCompletedEventAttributes
newLambdaFunctionCompletedEventAttributes
  pScheduledEventId_
  pStartedEventId_ =
    LambdaFunctionCompletedEventAttributes'
      { result =
          Prelude.Nothing,
        scheduledEventId =
          pScheduledEventId_,
        startedEventId = pStartedEventId_
      }

-- | The results of the Lambda task.
lambdaFunctionCompletedEventAttributes_result :: Lens.Lens' LambdaFunctionCompletedEventAttributes (Prelude.Maybe Prelude.Text)
lambdaFunctionCompletedEventAttributes_result = Lens.lens (\LambdaFunctionCompletedEventAttributes' {result} -> result) (\s@LambdaFunctionCompletedEventAttributes' {} a -> s {result = a} :: LambdaFunctionCompletedEventAttributes)

-- | The ID of the @LambdaFunctionScheduled@ event that was recorded when
-- this Lambda task was scheduled. To help diagnose issues, use this
-- information to trace back the chain of events leading up to this event.
lambdaFunctionCompletedEventAttributes_scheduledEventId :: Lens.Lens' LambdaFunctionCompletedEventAttributes Prelude.Integer
lambdaFunctionCompletedEventAttributes_scheduledEventId = Lens.lens (\LambdaFunctionCompletedEventAttributes' {scheduledEventId} -> scheduledEventId) (\s@LambdaFunctionCompletedEventAttributes' {} a -> s {scheduledEventId = a} :: LambdaFunctionCompletedEventAttributes)

-- | The ID of the @LambdaFunctionStarted@ event recorded when this activity
-- task started. To help diagnose issues, use this information to trace
-- back the chain of events leading up to this event.
lambdaFunctionCompletedEventAttributes_startedEventId :: Lens.Lens' LambdaFunctionCompletedEventAttributes Prelude.Integer
lambdaFunctionCompletedEventAttributes_startedEventId = Lens.lens (\LambdaFunctionCompletedEventAttributes' {startedEventId} -> startedEventId) (\s@LambdaFunctionCompletedEventAttributes' {} a -> s {startedEventId = a} :: LambdaFunctionCompletedEventAttributes)

instance
  Data.FromJSON
    LambdaFunctionCompletedEventAttributes
  where
  parseJSON =
    Data.withObject
      "LambdaFunctionCompletedEventAttributes"
      ( \x ->
          LambdaFunctionCompletedEventAttributes'
            Prelude.<$> (x Data..:? "result")
            Prelude.<*> (x Data..: "scheduledEventId")
            Prelude.<*> (x Data..: "startedEventId")
      )

instance
  Prelude.Hashable
    LambdaFunctionCompletedEventAttributes
  where
  hashWithSalt
    _salt
    LambdaFunctionCompletedEventAttributes' {..} =
      _salt
        `Prelude.hashWithSalt` result
        `Prelude.hashWithSalt` scheduledEventId
        `Prelude.hashWithSalt` startedEventId

instance
  Prelude.NFData
    LambdaFunctionCompletedEventAttributes
  where
  rnf LambdaFunctionCompletedEventAttributes' {..} =
    Prelude.rnf result
      `Prelude.seq` Prelude.rnf scheduledEventId
      `Prelude.seq` Prelude.rnf startedEventId
