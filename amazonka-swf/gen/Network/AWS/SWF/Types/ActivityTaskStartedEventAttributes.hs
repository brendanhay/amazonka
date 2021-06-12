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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides the details of the @ActivityTaskStarted@ event.
--
-- /See:/ 'newActivityTaskStartedEventAttributes' smart constructor.
data ActivityTaskStartedEventAttributes = ActivityTaskStartedEventAttributes'
  { -- | Identity of the worker that was assigned this task. This aids
    -- diagnostics when problems arise. The form of this identity is user
    -- defined.
    identity :: Core.Maybe Core.Text,
    -- | The ID of the @ActivityTaskScheduled@ event that was recorded when this
    -- activity task was scheduled. This information can be useful for
    -- diagnosing problems by tracing back the chain of events leading up to
    -- this event.
    scheduledEventId :: Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Integer ->
  ActivityTaskStartedEventAttributes
newActivityTaskStartedEventAttributes
  pScheduledEventId_ =
    ActivityTaskStartedEventAttributes'
      { identity =
          Core.Nothing,
        scheduledEventId = pScheduledEventId_
      }

-- | Identity of the worker that was assigned this task. This aids
-- diagnostics when problems arise. The form of this identity is user
-- defined.
activityTaskStartedEventAttributes_identity :: Lens.Lens' ActivityTaskStartedEventAttributes (Core.Maybe Core.Text)
activityTaskStartedEventAttributes_identity = Lens.lens (\ActivityTaskStartedEventAttributes' {identity} -> identity) (\s@ActivityTaskStartedEventAttributes' {} a -> s {identity = a} :: ActivityTaskStartedEventAttributes)

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this
-- activity task was scheduled. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
activityTaskStartedEventAttributes_scheduledEventId :: Lens.Lens' ActivityTaskStartedEventAttributes Core.Integer
activityTaskStartedEventAttributes_scheduledEventId = Lens.lens (\ActivityTaskStartedEventAttributes' {scheduledEventId} -> scheduledEventId) (\s@ActivityTaskStartedEventAttributes' {} a -> s {scheduledEventId = a} :: ActivityTaskStartedEventAttributes)

instance
  Core.FromJSON
    ActivityTaskStartedEventAttributes
  where
  parseJSON =
    Core.withObject
      "ActivityTaskStartedEventAttributes"
      ( \x ->
          ActivityTaskStartedEventAttributes'
            Core.<$> (x Core..:? "identity")
            Core.<*> (x Core..: "scheduledEventId")
      )

instance
  Core.Hashable
    ActivityTaskStartedEventAttributes

instance
  Core.NFData
    ActivityTaskStartedEventAttributes
