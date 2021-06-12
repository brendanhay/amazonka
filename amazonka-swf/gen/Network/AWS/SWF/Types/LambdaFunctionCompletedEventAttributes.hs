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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides the details of the @LambdaFunctionCompleted@ event. It isn\'t
-- set for other event types.
--
-- /See:/ 'newLambdaFunctionCompletedEventAttributes' smart constructor.
data LambdaFunctionCompletedEventAttributes = LambdaFunctionCompletedEventAttributes'
  { -- | The results of the Lambda task.
    result :: Core.Maybe Core.Text,
    -- | The ID of the @LambdaFunctionScheduled@ event that was recorded when
    -- this Lambda task was scheduled. To help diagnose issues, use this
    -- information to trace back the chain of events leading up to this event.
    scheduledEventId :: Core.Integer,
    -- | The ID of the @LambdaFunctionStarted@ event recorded when this activity
    -- task started. To help diagnose issues, use this information to trace
    -- back the chain of events leading up to this event.
    startedEventId :: Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Integer ->
  -- | 'startedEventId'
  Core.Integer ->
  LambdaFunctionCompletedEventAttributes
newLambdaFunctionCompletedEventAttributes
  pScheduledEventId_
  pStartedEventId_ =
    LambdaFunctionCompletedEventAttributes'
      { result =
          Core.Nothing,
        scheduledEventId =
          pScheduledEventId_,
        startedEventId = pStartedEventId_
      }

-- | The results of the Lambda task.
lambdaFunctionCompletedEventAttributes_result :: Lens.Lens' LambdaFunctionCompletedEventAttributes (Core.Maybe Core.Text)
lambdaFunctionCompletedEventAttributes_result = Lens.lens (\LambdaFunctionCompletedEventAttributes' {result} -> result) (\s@LambdaFunctionCompletedEventAttributes' {} a -> s {result = a} :: LambdaFunctionCompletedEventAttributes)

-- | The ID of the @LambdaFunctionScheduled@ event that was recorded when
-- this Lambda task was scheduled. To help diagnose issues, use this
-- information to trace back the chain of events leading up to this event.
lambdaFunctionCompletedEventAttributes_scheduledEventId :: Lens.Lens' LambdaFunctionCompletedEventAttributes Core.Integer
lambdaFunctionCompletedEventAttributes_scheduledEventId = Lens.lens (\LambdaFunctionCompletedEventAttributes' {scheduledEventId} -> scheduledEventId) (\s@LambdaFunctionCompletedEventAttributes' {} a -> s {scheduledEventId = a} :: LambdaFunctionCompletedEventAttributes)

-- | The ID of the @LambdaFunctionStarted@ event recorded when this activity
-- task started. To help diagnose issues, use this information to trace
-- back the chain of events leading up to this event.
lambdaFunctionCompletedEventAttributes_startedEventId :: Lens.Lens' LambdaFunctionCompletedEventAttributes Core.Integer
lambdaFunctionCompletedEventAttributes_startedEventId = Lens.lens (\LambdaFunctionCompletedEventAttributes' {startedEventId} -> startedEventId) (\s@LambdaFunctionCompletedEventAttributes' {} a -> s {startedEventId = a} :: LambdaFunctionCompletedEventAttributes)

instance
  Core.FromJSON
    LambdaFunctionCompletedEventAttributes
  where
  parseJSON =
    Core.withObject
      "LambdaFunctionCompletedEventAttributes"
      ( \x ->
          LambdaFunctionCompletedEventAttributes'
            Core.<$> (x Core..:? "result")
            Core.<*> (x Core..: "scheduledEventId")
            Core.<*> (x Core..: "startedEventId")
      )

instance
  Core.Hashable
    LambdaFunctionCompletedEventAttributes

instance
  Core.NFData
    LambdaFunctionCompletedEventAttributes
