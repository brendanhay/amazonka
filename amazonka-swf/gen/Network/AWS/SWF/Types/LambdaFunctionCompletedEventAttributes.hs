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
-- Module      : Network.AWS.SWF.Types.LambdaFunctionCompletedEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.LambdaFunctionCompletedEventAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.FromJSON
    LambdaFunctionCompletedEventAttributes
  where
  parseJSON =
    Prelude.withObject
      "LambdaFunctionCompletedEventAttributes"
      ( \x ->
          LambdaFunctionCompletedEventAttributes'
            Prelude.<$> (x Prelude..:? "result")
            Prelude.<*> (x Prelude..: "scheduledEventId")
            Prelude.<*> (x Prelude..: "startedEventId")
      )

instance
  Prelude.Hashable
    LambdaFunctionCompletedEventAttributes

instance
  Prelude.NFData
    LambdaFunctionCompletedEventAttributes
