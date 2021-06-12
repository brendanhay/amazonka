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
-- Module      : Network.AWS.SWF.Types.LambdaFunctionFailedEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.LambdaFunctionFailedEventAttributes where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides the details of the @LambdaFunctionFailed@ event. It isn\'t set
-- for other event types.
--
-- /See:/ 'newLambdaFunctionFailedEventAttributes' smart constructor.
data LambdaFunctionFailedEventAttributes = LambdaFunctionFailedEventAttributes'
  { -- | The details of the failure.
    details :: Core.Maybe Core.Text,
    -- | The reason provided for the failure.
    reason :: Core.Maybe Core.Text,
    -- | The ID of the @LambdaFunctionScheduled@ event that was recorded when
    -- this activity task was scheduled. To help diagnose issues, use this
    -- information to trace back the chain of events leading up to this event.
    scheduledEventId :: Core.Integer,
    -- | The ID of the @LambdaFunctionStarted@ event recorded when this activity
    -- task started. To help diagnose issues, use this information to trace
    -- back the chain of events leading up to this event.
    startedEventId :: Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LambdaFunctionFailedEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'details', 'lambdaFunctionFailedEventAttributes_details' - The details of the failure.
--
-- 'reason', 'lambdaFunctionFailedEventAttributes_reason' - The reason provided for the failure.
--
-- 'scheduledEventId', 'lambdaFunctionFailedEventAttributes_scheduledEventId' - The ID of the @LambdaFunctionScheduled@ event that was recorded when
-- this activity task was scheduled. To help diagnose issues, use this
-- information to trace back the chain of events leading up to this event.
--
-- 'startedEventId', 'lambdaFunctionFailedEventAttributes_startedEventId' - The ID of the @LambdaFunctionStarted@ event recorded when this activity
-- task started. To help diagnose issues, use this information to trace
-- back the chain of events leading up to this event.
newLambdaFunctionFailedEventAttributes ::
  -- | 'scheduledEventId'
  Core.Integer ->
  -- | 'startedEventId'
  Core.Integer ->
  LambdaFunctionFailedEventAttributes
newLambdaFunctionFailedEventAttributes
  pScheduledEventId_
  pStartedEventId_ =
    LambdaFunctionFailedEventAttributes'
      { details =
          Core.Nothing,
        reason = Core.Nothing,
        scheduledEventId = pScheduledEventId_,
        startedEventId = pStartedEventId_
      }

-- | The details of the failure.
lambdaFunctionFailedEventAttributes_details :: Lens.Lens' LambdaFunctionFailedEventAttributes (Core.Maybe Core.Text)
lambdaFunctionFailedEventAttributes_details = Lens.lens (\LambdaFunctionFailedEventAttributes' {details} -> details) (\s@LambdaFunctionFailedEventAttributes' {} a -> s {details = a} :: LambdaFunctionFailedEventAttributes)

-- | The reason provided for the failure.
lambdaFunctionFailedEventAttributes_reason :: Lens.Lens' LambdaFunctionFailedEventAttributes (Core.Maybe Core.Text)
lambdaFunctionFailedEventAttributes_reason = Lens.lens (\LambdaFunctionFailedEventAttributes' {reason} -> reason) (\s@LambdaFunctionFailedEventAttributes' {} a -> s {reason = a} :: LambdaFunctionFailedEventAttributes)

-- | The ID of the @LambdaFunctionScheduled@ event that was recorded when
-- this activity task was scheduled. To help diagnose issues, use this
-- information to trace back the chain of events leading up to this event.
lambdaFunctionFailedEventAttributes_scheduledEventId :: Lens.Lens' LambdaFunctionFailedEventAttributes Core.Integer
lambdaFunctionFailedEventAttributes_scheduledEventId = Lens.lens (\LambdaFunctionFailedEventAttributes' {scheduledEventId} -> scheduledEventId) (\s@LambdaFunctionFailedEventAttributes' {} a -> s {scheduledEventId = a} :: LambdaFunctionFailedEventAttributes)

-- | The ID of the @LambdaFunctionStarted@ event recorded when this activity
-- task started. To help diagnose issues, use this information to trace
-- back the chain of events leading up to this event.
lambdaFunctionFailedEventAttributes_startedEventId :: Lens.Lens' LambdaFunctionFailedEventAttributes Core.Integer
lambdaFunctionFailedEventAttributes_startedEventId = Lens.lens (\LambdaFunctionFailedEventAttributes' {startedEventId} -> startedEventId) (\s@LambdaFunctionFailedEventAttributes' {} a -> s {startedEventId = a} :: LambdaFunctionFailedEventAttributes)

instance
  Core.FromJSON
    LambdaFunctionFailedEventAttributes
  where
  parseJSON =
    Core.withObject
      "LambdaFunctionFailedEventAttributes"
      ( \x ->
          LambdaFunctionFailedEventAttributes'
            Core.<$> (x Core..:? "details")
            Core.<*> (x Core..:? "reason")
            Core.<*> (x Core..: "scheduledEventId")
            Core.<*> (x Core..: "startedEventId")
      )

instance
  Core.Hashable
    LambdaFunctionFailedEventAttributes

instance
  Core.NFData
    LambdaFunctionFailedEventAttributes
