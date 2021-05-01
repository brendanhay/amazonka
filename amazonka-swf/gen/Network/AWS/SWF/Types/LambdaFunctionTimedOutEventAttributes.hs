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
-- Module      : Network.AWS.SWF.Types.LambdaFunctionTimedOutEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.LambdaFunctionTimedOutEventAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SWF.Types.LambdaFunctionTimeoutType

-- | Provides details of the @LambdaFunctionTimedOut@ event.
--
-- /See:/ 'newLambdaFunctionTimedOutEventAttributes' smart constructor.
data LambdaFunctionTimedOutEventAttributes = LambdaFunctionTimedOutEventAttributes'
  { -- | The type of the timeout that caused this event.
    timeoutType :: Prelude.Maybe LambdaFunctionTimeoutType,
    -- | The ID of the @LambdaFunctionScheduled@ event that was recorded when
    -- this activity task was scheduled. To help diagnose issues, use this
    -- information to trace back the chain of events leading up to this event.
    scheduledEventId :: Prelude.Integer,
    -- | The ID of the @ActivityTaskStarted@ event that was recorded when this
    -- activity task started. To help diagnose issues, use this information to
    -- trace back the chain of events leading up to this event.
    startedEventId :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LambdaFunctionTimedOutEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeoutType', 'lambdaFunctionTimedOutEventAttributes_timeoutType' - The type of the timeout that caused this event.
--
-- 'scheduledEventId', 'lambdaFunctionTimedOutEventAttributes_scheduledEventId' - The ID of the @LambdaFunctionScheduled@ event that was recorded when
-- this activity task was scheduled. To help diagnose issues, use this
-- information to trace back the chain of events leading up to this event.
--
-- 'startedEventId', 'lambdaFunctionTimedOutEventAttributes_startedEventId' - The ID of the @ActivityTaskStarted@ event that was recorded when this
-- activity task started. To help diagnose issues, use this information to
-- trace back the chain of events leading up to this event.
newLambdaFunctionTimedOutEventAttributes ::
  -- | 'scheduledEventId'
  Prelude.Integer ->
  -- | 'startedEventId'
  Prelude.Integer ->
  LambdaFunctionTimedOutEventAttributes
newLambdaFunctionTimedOutEventAttributes
  pScheduledEventId_
  pStartedEventId_ =
    LambdaFunctionTimedOutEventAttributes'
      { timeoutType =
          Prelude.Nothing,
        scheduledEventId =
          pScheduledEventId_,
        startedEventId = pStartedEventId_
      }

-- | The type of the timeout that caused this event.
lambdaFunctionTimedOutEventAttributes_timeoutType :: Lens.Lens' LambdaFunctionTimedOutEventAttributes (Prelude.Maybe LambdaFunctionTimeoutType)
lambdaFunctionTimedOutEventAttributes_timeoutType = Lens.lens (\LambdaFunctionTimedOutEventAttributes' {timeoutType} -> timeoutType) (\s@LambdaFunctionTimedOutEventAttributes' {} a -> s {timeoutType = a} :: LambdaFunctionTimedOutEventAttributes)

-- | The ID of the @LambdaFunctionScheduled@ event that was recorded when
-- this activity task was scheduled. To help diagnose issues, use this
-- information to trace back the chain of events leading up to this event.
lambdaFunctionTimedOutEventAttributes_scheduledEventId :: Lens.Lens' LambdaFunctionTimedOutEventAttributes Prelude.Integer
lambdaFunctionTimedOutEventAttributes_scheduledEventId = Lens.lens (\LambdaFunctionTimedOutEventAttributes' {scheduledEventId} -> scheduledEventId) (\s@LambdaFunctionTimedOutEventAttributes' {} a -> s {scheduledEventId = a} :: LambdaFunctionTimedOutEventAttributes)

-- | The ID of the @ActivityTaskStarted@ event that was recorded when this
-- activity task started. To help diagnose issues, use this information to
-- trace back the chain of events leading up to this event.
lambdaFunctionTimedOutEventAttributes_startedEventId :: Lens.Lens' LambdaFunctionTimedOutEventAttributes Prelude.Integer
lambdaFunctionTimedOutEventAttributes_startedEventId = Lens.lens (\LambdaFunctionTimedOutEventAttributes' {startedEventId} -> startedEventId) (\s@LambdaFunctionTimedOutEventAttributes' {} a -> s {startedEventId = a} :: LambdaFunctionTimedOutEventAttributes)

instance
  Prelude.FromJSON
    LambdaFunctionTimedOutEventAttributes
  where
  parseJSON =
    Prelude.withObject
      "LambdaFunctionTimedOutEventAttributes"
      ( \x ->
          LambdaFunctionTimedOutEventAttributes'
            Prelude.<$> (x Prelude..:? "timeoutType")
            Prelude.<*> (x Prelude..: "scheduledEventId")
            Prelude.<*> (x Prelude..: "startedEventId")
      )

instance
  Prelude.Hashable
    LambdaFunctionTimedOutEventAttributes

instance
  Prelude.NFData
    LambdaFunctionTimedOutEventAttributes
