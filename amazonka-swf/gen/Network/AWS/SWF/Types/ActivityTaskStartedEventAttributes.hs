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
-- Module      : Network.AWS.SWF.Types.ActivityTaskStartedEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ActivityTaskStartedEventAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides the details of the @ActivityTaskStarted@ event.
--
-- /See:/ 'newActivityTaskStartedEventAttributes' smart constructor.
data ActivityTaskStartedEventAttributes = ActivityTaskStartedEventAttributes'
  { -- | Identity of the worker that was assigned this task. This aids
    -- diagnostics when problems arise. The form of this identity is user
    -- defined.
    identity :: Prelude.Maybe Prelude.Text,
    -- | The ID of the @ActivityTaskScheduled@ event that was recorded when this
    -- activity task was scheduled. This information can be useful for
    -- diagnosing problems by tracing back the chain of events leading up to
    -- this event.
    scheduledEventId :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ActivityTaskStartedEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identity', 'activityTaskStartedEventAttributes_identity' - Identity of the worker that was assigned this task. This aids
-- diagnostics when problems arise. The form of this identity is user
-- defined.
--
-- 'scheduledEventId', 'activityTaskStartedEventAttributes_scheduledEventId' - The ID of the @ActivityTaskScheduled@ event that was recorded when this
-- activity task was scheduled. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
newActivityTaskStartedEventAttributes ::
  -- | 'scheduledEventId'
  Prelude.Integer ->
  ActivityTaskStartedEventAttributes
newActivityTaskStartedEventAttributes
  pScheduledEventId_ =
    ActivityTaskStartedEventAttributes'
      { identity =
          Prelude.Nothing,
        scheduledEventId = pScheduledEventId_
      }

-- | Identity of the worker that was assigned this task. This aids
-- diagnostics when problems arise. The form of this identity is user
-- defined.
activityTaskStartedEventAttributes_identity :: Lens.Lens' ActivityTaskStartedEventAttributes (Prelude.Maybe Prelude.Text)
activityTaskStartedEventAttributes_identity = Lens.lens (\ActivityTaskStartedEventAttributes' {identity} -> identity) (\s@ActivityTaskStartedEventAttributes' {} a -> s {identity = a} :: ActivityTaskStartedEventAttributes)

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this
-- activity task was scheduled. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
activityTaskStartedEventAttributes_scheduledEventId :: Lens.Lens' ActivityTaskStartedEventAttributes Prelude.Integer
activityTaskStartedEventAttributes_scheduledEventId = Lens.lens (\ActivityTaskStartedEventAttributes' {scheduledEventId} -> scheduledEventId) (\s@ActivityTaskStartedEventAttributes' {} a -> s {scheduledEventId = a} :: ActivityTaskStartedEventAttributes)

instance
  Prelude.FromJSON
    ActivityTaskStartedEventAttributes
  where
  parseJSON =
    Prelude.withObject
      "ActivityTaskStartedEventAttributes"
      ( \x ->
          ActivityTaskStartedEventAttributes'
            Prelude.<$> (x Prelude..:? "identity")
            Prelude.<*> (x Prelude..: "scheduledEventId")
      )

instance
  Prelude.Hashable
    ActivityTaskStartedEventAttributes

instance
  Prelude.NFData
    ActivityTaskStartedEventAttributes
