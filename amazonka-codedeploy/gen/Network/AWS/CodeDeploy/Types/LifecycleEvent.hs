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
-- Module      : Network.AWS.CodeDeploy.Types.LifecycleEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.LifecycleEvent where

import Network.AWS.CodeDeploy.Types.Diagnostics
import Network.AWS.CodeDeploy.Types.LifecycleEventStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    status :: Prelude.Maybe LifecycleEventStatus,
    -- | Diagnostic information about the deployment lifecycle event.
    diagnostics :: Prelude.Maybe Diagnostics,
    -- | A timestamp that indicates when the deployment lifecycle event started.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | A timestamp that indicates when the deployment lifecycle event ended.
    endTime :: Prelude.Maybe Prelude.POSIX,
    -- | The deployment lifecycle event name, such as @ApplicationStop@,
    -- @BeforeInstall@, @AfterInstall@, @ApplicationStart@, or
    -- @ValidateService@.
    lifecycleEventName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { status = Prelude.Nothing,
      diagnostics = Prelude.Nothing,
      startTime = Prelude.Nothing,
      endTime = Prelude.Nothing,
      lifecycleEventName = Prelude.Nothing
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
lifecycleEvent_status :: Lens.Lens' LifecycleEvent (Prelude.Maybe LifecycleEventStatus)
lifecycleEvent_status = Lens.lens (\LifecycleEvent' {status} -> status) (\s@LifecycleEvent' {} a -> s {status = a} :: LifecycleEvent)

-- | Diagnostic information about the deployment lifecycle event.
lifecycleEvent_diagnostics :: Lens.Lens' LifecycleEvent (Prelude.Maybe Diagnostics)
lifecycleEvent_diagnostics = Lens.lens (\LifecycleEvent' {diagnostics} -> diagnostics) (\s@LifecycleEvent' {} a -> s {diagnostics = a} :: LifecycleEvent)

-- | A timestamp that indicates when the deployment lifecycle event started.
lifecycleEvent_startTime :: Lens.Lens' LifecycleEvent (Prelude.Maybe Prelude.UTCTime)
lifecycleEvent_startTime = Lens.lens (\LifecycleEvent' {startTime} -> startTime) (\s@LifecycleEvent' {} a -> s {startTime = a} :: LifecycleEvent) Prelude.. Lens.mapping Prelude._Time

-- | A timestamp that indicates when the deployment lifecycle event ended.
lifecycleEvent_endTime :: Lens.Lens' LifecycleEvent (Prelude.Maybe Prelude.UTCTime)
lifecycleEvent_endTime = Lens.lens (\LifecycleEvent' {endTime} -> endTime) (\s@LifecycleEvent' {} a -> s {endTime = a} :: LifecycleEvent) Prelude.. Lens.mapping Prelude._Time

-- | The deployment lifecycle event name, such as @ApplicationStop@,
-- @BeforeInstall@, @AfterInstall@, @ApplicationStart@, or
-- @ValidateService@.
lifecycleEvent_lifecycleEventName :: Lens.Lens' LifecycleEvent (Prelude.Maybe Prelude.Text)
lifecycleEvent_lifecycleEventName = Lens.lens (\LifecycleEvent' {lifecycleEventName} -> lifecycleEventName) (\s@LifecycleEvent' {} a -> s {lifecycleEventName = a} :: LifecycleEvent)

instance Prelude.FromJSON LifecycleEvent where
  parseJSON =
    Prelude.withObject
      "LifecycleEvent"
      ( \x ->
          LifecycleEvent'
            Prelude.<$> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "diagnostics")
            Prelude.<*> (x Prelude..:? "startTime")
            Prelude.<*> (x Prelude..:? "endTime")
            Prelude.<*> (x Prelude..:? "lifecycleEventName")
      )

instance Prelude.Hashable LifecycleEvent

instance Prelude.NFData LifecycleEvent
