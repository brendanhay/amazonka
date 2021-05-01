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
-- Module      : Network.AWS.SWF.Types.LambdaFunctionStartedEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.LambdaFunctionStartedEventAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides the details of the @LambdaFunctionStarted@ event. It isn\'t set
-- for other event types.
--
-- /See:/ 'newLambdaFunctionStartedEventAttributes' smart constructor.
data LambdaFunctionStartedEventAttributes = LambdaFunctionStartedEventAttributes'
  { -- | The ID of the @LambdaFunctionScheduled@ event that was recorded when
    -- this activity task was scheduled. To help diagnose issues, use this
    -- information to trace back the chain of events leading up to this event.
    scheduledEventId :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LambdaFunctionStartedEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduledEventId', 'lambdaFunctionStartedEventAttributes_scheduledEventId' - The ID of the @LambdaFunctionScheduled@ event that was recorded when
-- this activity task was scheduled. To help diagnose issues, use this
-- information to trace back the chain of events leading up to this event.
newLambdaFunctionStartedEventAttributes ::
  -- | 'scheduledEventId'
  Prelude.Integer ->
  LambdaFunctionStartedEventAttributes
newLambdaFunctionStartedEventAttributes
  pScheduledEventId_ =
    LambdaFunctionStartedEventAttributes'
      { scheduledEventId =
          pScheduledEventId_
      }

-- | The ID of the @LambdaFunctionScheduled@ event that was recorded when
-- this activity task was scheduled. To help diagnose issues, use this
-- information to trace back the chain of events leading up to this event.
lambdaFunctionStartedEventAttributes_scheduledEventId :: Lens.Lens' LambdaFunctionStartedEventAttributes Prelude.Integer
lambdaFunctionStartedEventAttributes_scheduledEventId = Lens.lens (\LambdaFunctionStartedEventAttributes' {scheduledEventId} -> scheduledEventId) (\s@LambdaFunctionStartedEventAttributes' {} a -> s {scheduledEventId = a} :: LambdaFunctionStartedEventAttributes)

instance
  Prelude.FromJSON
    LambdaFunctionStartedEventAttributes
  where
  parseJSON =
    Prelude.withObject
      "LambdaFunctionStartedEventAttributes"
      ( \x ->
          LambdaFunctionStartedEventAttributes'
            Prelude.<$> (x Prelude..: "scheduledEventId")
      )

instance
  Prelude.Hashable
    LambdaFunctionStartedEventAttributes

instance
  Prelude.NFData
    LambdaFunctionStartedEventAttributes
