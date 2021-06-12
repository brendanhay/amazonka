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
-- Module      : Network.AWS.CodeDeploy.Types.LifecycleEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.LifecycleEvent where

import Network.AWS.CodeDeploy.Types.Diagnostics
import Network.AWS.CodeDeploy.Types.LifecycleEventStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a deployment lifecycle event.
--
-- /See:/ 'newLifecycleEvent' smart constructor.
data LifecycleEvent = LifecycleEvent'
  { -- | The deployment lifecycle event status:
    --
    -- -   Pending: The deployment lifecycle event is pending.
    --
    -- -   InProgress: The deployment lifecycle event is in progress.
    --
    -- -   Succeeded: The deployment lifecycle event ran successfully.
    --
    -- -   Failed: The deployment lifecycle event has failed.
    --
    -- -   Skipped: The deployment lifecycle event has been skipped.
    --
    -- -   Unknown: The deployment lifecycle event is unknown.
    status :: Core.Maybe LifecycleEventStatus,
    -- | Diagnostic information about the deployment lifecycle event.
    diagnostics :: Core.Maybe Diagnostics,
    -- | A timestamp that indicates when the deployment lifecycle event started.
    startTime :: Core.Maybe Core.POSIX,
    -- | A timestamp that indicates when the deployment lifecycle event ended.
    endTime :: Core.Maybe Core.POSIX,
    -- | The deployment lifecycle event name, such as @ApplicationStop@,
    -- @BeforeInstall@, @AfterInstall@, @ApplicationStart@, or
    -- @ValidateService@.
    lifecycleEventName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LifecycleEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'lifecycleEvent_status' - The deployment lifecycle event status:
--
-- -   Pending: The deployment lifecycle event is pending.
--
-- -   InProgress: The deployment lifecycle event is in progress.
--
-- -   Succeeded: The deployment lifecycle event ran successfully.
--
-- -   Failed: The deployment lifecycle event has failed.
--
-- -   Skipped: The deployment lifecycle event has been skipped.
--
-- -   Unknown: The deployment lifecycle event is unknown.
--
-- 'diagnostics', 'lifecycleEvent_diagnostics' - Diagnostic information about the deployment lifecycle event.
--
-- 'startTime', 'lifecycleEvent_startTime' - A timestamp that indicates when the deployment lifecycle event started.
--
-- 'endTime', 'lifecycleEvent_endTime' - A timestamp that indicates when the deployment lifecycle event ended.
--
-- 'lifecycleEventName', 'lifecycleEvent_lifecycleEventName' - The deployment lifecycle event name, such as @ApplicationStop@,
-- @BeforeInstall@, @AfterInstall@, @ApplicationStart@, or
-- @ValidateService@.
newLifecycleEvent ::
  LifecycleEvent
newLifecycleEvent =
  LifecycleEvent'
    { status = Core.Nothing,
      diagnostics = Core.Nothing,
      startTime = Core.Nothing,
      endTime = Core.Nothing,
      lifecycleEventName = Core.Nothing
    }

-- | The deployment lifecycle event status:
--
-- -   Pending: The deployment lifecycle event is pending.
--
-- -   InProgress: The deployment lifecycle event is in progress.
--
-- -   Succeeded: The deployment lifecycle event ran successfully.
--
-- -   Failed: The deployment lifecycle event has failed.
--
-- -   Skipped: The deployment lifecycle event has been skipped.
--
-- -   Unknown: The deployment lifecycle event is unknown.
lifecycleEvent_status :: Lens.Lens' LifecycleEvent (Core.Maybe LifecycleEventStatus)
lifecycleEvent_status = Lens.lens (\LifecycleEvent' {status} -> status) (\s@LifecycleEvent' {} a -> s {status = a} :: LifecycleEvent)

-- | Diagnostic information about the deployment lifecycle event.
lifecycleEvent_diagnostics :: Lens.Lens' LifecycleEvent (Core.Maybe Diagnostics)
lifecycleEvent_diagnostics = Lens.lens (\LifecycleEvent' {diagnostics} -> diagnostics) (\s@LifecycleEvent' {} a -> s {diagnostics = a} :: LifecycleEvent)

-- | A timestamp that indicates when the deployment lifecycle event started.
lifecycleEvent_startTime :: Lens.Lens' LifecycleEvent (Core.Maybe Core.UTCTime)
lifecycleEvent_startTime = Lens.lens (\LifecycleEvent' {startTime} -> startTime) (\s@LifecycleEvent' {} a -> s {startTime = a} :: LifecycleEvent) Core.. Lens.mapping Core._Time

-- | A timestamp that indicates when the deployment lifecycle event ended.
lifecycleEvent_endTime :: Lens.Lens' LifecycleEvent (Core.Maybe Core.UTCTime)
lifecycleEvent_endTime = Lens.lens (\LifecycleEvent' {endTime} -> endTime) (\s@LifecycleEvent' {} a -> s {endTime = a} :: LifecycleEvent) Core.. Lens.mapping Core._Time

-- | The deployment lifecycle event name, such as @ApplicationStop@,
-- @BeforeInstall@, @AfterInstall@, @ApplicationStart@, or
-- @ValidateService@.
lifecycleEvent_lifecycleEventName :: Lens.Lens' LifecycleEvent (Core.Maybe Core.Text)
lifecycleEvent_lifecycleEventName = Lens.lens (\LifecycleEvent' {lifecycleEventName} -> lifecycleEventName) (\s@LifecycleEvent' {} a -> s {lifecycleEventName = a} :: LifecycleEvent)

instance Core.FromJSON LifecycleEvent where
  parseJSON =
    Core.withObject
      "LifecycleEvent"
      ( \x ->
          LifecycleEvent'
            Core.<$> (x Core..:? "status")
            Core.<*> (x Core..:? "diagnostics")
            Core.<*> (x Core..:? "startTime")
            Core.<*> (x Core..:? "endTime")
            Core.<*> (x Core..:? "lifecycleEventName")
      )

instance Core.Hashable LifecycleEvent

instance Core.NFData LifecycleEvent
