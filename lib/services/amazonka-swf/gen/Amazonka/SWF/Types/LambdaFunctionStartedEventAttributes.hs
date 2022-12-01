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
-- Module      : Amazonka.SWF.Types.LambdaFunctionStartedEventAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.LambdaFunctionStartedEventAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Core.FromJSON
    LambdaFunctionStartedEventAttributes
  where
  parseJSON =
    Core.withObject
      "LambdaFunctionStartedEventAttributes"
      ( \x ->
          LambdaFunctionStartedEventAttributes'
            Prelude.<$> (x Core..: "scheduledEventId")
      )

instance
  Prelude.Hashable
    LambdaFunctionStartedEventAttributes
  where
  hashWithSalt
    _salt
    LambdaFunctionStartedEventAttributes' {..} =
      _salt `Prelude.hashWithSalt` scheduledEventId

instance
  Prelude.NFData
    LambdaFunctionStartedEventAttributes
  where
  rnf LambdaFunctionStartedEventAttributes' {..} =
    Prelude.rnf scheduledEventId
