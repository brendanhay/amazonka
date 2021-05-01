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
-- Module      : Network.AWS.SWF.Types.LambdaFunctionFailedEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.LambdaFunctionFailedEventAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides the details of the @LambdaFunctionFailed@ event. It isn\'t set
-- for other event types.
--
-- /See:/ 'newLambdaFunctionFailedEventAttributes' smart constructor.
data LambdaFunctionFailedEventAttributes = LambdaFunctionFailedEventAttributes'
  { -- | The details of the failure.
    details :: Prelude.Maybe Prelude.Text,
    -- | The reason provided for the failure.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The ID of the @LambdaFunctionScheduled@ event that was recorded when
    -- this activity task was scheduled. To help diagnose issues, use this
    -- information to trace back the chain of events leading up to this event.
    scheduledEventId :: Prelude.Integer,
    -- | The ID of the @LambdaFunctionStarted@ event recorded when this activity
    -- task started. To help diagnose issues, use this information to trace
    -- back the chain of events leading up to this event.
    startedEventId :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Integer ->
  -- | 'startedEventId'
  Prelude.Integer ->
  LambdaFunctionFailedEventAttributes
newLambdaFunctionFailedEventAttributes
  pScheduledEventId_
  pStartedEventId_ =
    LambdaFunctionFailedEventAttributes'
      { details =
          Prelude.Nothing,
        reason = Prelude.Nothing,
        scheduledEventId = pScheduledEventId_,
        startedEventId = pStartedEventId_
      }

-- | The details of the failure.
lambdaFunctionFailedEventAttributes_details :: Lens.Lens' LambdaFunctionFailedEventAttributes (Prelude.Maybe Prelude.Text)
lambdaFunctionFailedEventAttributes_details = Lens.lens (\LambdaFunctionFailedEventAttributes' {details} -> details) (\s@LambdaFunctionFailedEventAttributes' {} a -> s {details = a} :: LambdaFunctionFailedEventAttributes)

-- | The reason provided for the failure.
lambdaFunctionFailedEventAttributes_reason :: Lens.Lens' LambdaFunctionFailedEventAttributes (Prelude.Maybe Prelude.Text)
lambdaFunctionFailedEventAttributes_reason = Lens.lens (\LambdaFunctionFailedEventAttributes' {reason} -> reason) (\s@LambdaFunctionFailedEventAttributes' {} a -> s {reason = a} :: LambdaFunctionFailedEventAttributes)

-- | The ID of the @LambdaFunctionScheduled@ event that was recorded when
-- this activity task was scheduled. To help diagnose issues, use this
-- information to trace back the chain of events leading up to this event.
lambdaFunctionFailedEventAttributes_scheduledEventId :: Lens.Lens' LambdaFunctionFailedEventAttributes Prelude.Integer
lambdaFunctionFailedEventAttributes_scheduledEventId = Lens.lens (\LambdaFunctionFailedEventAttributes' {scheduledEventId} -> scheduledEventId) (\s@LambdaFunctionFailedEventAttributes' {} a -> s {scheduledEventId = a} :: LambdaFunctionFailedEventAttributes)

-- | The ID of the @LambdaFunctionStarted@ event recorded when this activity
-- task started. To help diagnose issues, use this information to trace
-- back the chain of events leading up to this event.
lambdaFunctionFailedEventAttributes_startedEventId :: Lens.Lens' LambdaFunctionFailedEventAttributes Prelude.Integer
lambdaFunctionFailedEventAttributes_startedEventId = Lens.lens (\LambdaFunctionFailedEventAttributes' {startedEventId} -> startedEventId) (\s@LambdaFunctionFailedEventAttributes' {} a -> s {startedEventId = a} :: LambdaFunctionFailedEventAttributes)

instance
  Prelude.FromJSON
    LambdaFunctionFailedEventAttributes
  where
  parseJSON =
    Prelude.withObject
      "LambdaFunctionFailedEventAttributes"
      ( \x ->
          LambdaFunctionFailedEventAttributes'
            Prelude.<$> (x Prelude..:? "details")
            Prelude.<*> (x Prelude..:? "reason")
            Prelude.<*> (x Prelude..: "scheduledEventId")
            Prelude.<*> (x Prelude..: "startedEventId")
      )

instance
  Prelude.Hashable
    LambdaFunctionFailedEventAttributes

instance
  Prelude.NFData
    LambdaFunctionFailedEventAttributes
