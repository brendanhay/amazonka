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
-- Module      : Network.AWS.Config.Types.RemediationExecutionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RemediationExecutionStatus where

import Network.AWS.Config.Types.RemediationExecutionState
import Network.AWS.Config.Types.RemediationExecutionStep
import Network.AWS.Config.Types.ResourceKey
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides details of the current status of the invoked remediation action
-- for that resource.
--
-- /See:/ 'newRemediationExecutionStatus' smart constructor.
data RemediationExecutionStatus = RemediationExecutionStatus'
  { -- | Start time when the remediation was executed.
    invocationTime :: Core.Maybe Core.POSIX,
    resourceKey :: Core.Maybe ResourceKey,
    -- | ENUM of the values.
    state :: Core.Maybe RemediationExecutionState,
    -- | Details of every step.
    stepDetails :: Core.Maybe [RemediationExecutionStep],
    -- | The time when the remediation execution was last updated.
    lastUpdatedTime :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemediationExecutionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invocationTime', 'remediationExecutionStatus_invocationTime' - Start time when the remediation was executed.
--
-- 'resourceKey', 'remediationExecutionStatus_resourceKey' - Undocumented member.
--
-- 'state', 'remediationExecutionStatus_state' - ENUM of the values.
--
-- 'stepDetails', 'remediationExecutionStatus_stepDetails' - Details of every step.
--
-- 'lastUpdatedTime', 'remediationExecutionStatus_lastUpdatedTime' - The time when the remediation execution was last updated.
newRemediationExecutionStatus ::
  RemediationExecutionStatus
newRemediationExecutionStatus =
  RemediationExecutionStatus'
    { invocationTime =
        Core.Nothing,
      resourceKey = Core.Nothing,
      state = Core.Nothing,
      stepDetails = Core.Nothing,
      lastUpdatedTime = Core.Nothing
    }

-- | Start time when the remediation was executed.
remediationExecutionStatus_invocationTime :: Lens.Lens' RemediationExecutionStatus (Core.Maybe Core.UTCTime)
remediationExecutionStatus_invocationTime = Lens.lens (\RemediationExecutionStatus' {invocationTime} -> invocationTime) (\s@RemediationExecutionStatus' {} a -> s {invocationTime = a} :: RemediationExecutionStatus) Core.. Lens.mapping Core._Time

-- | Undocumented member.
remediationExecutionStatus_resourceKey :: Lens.Lens' RemediationExecutionStatus (Core.Maybe ResourceKey)
remediationExecutionStatus_resourceKey = Lens.lens (\RemediationExecutionStatus' {resourceKey} -> resourceKey) (\s@RemediationExecutionStatus' {} a -> s {resourceKey = a} :: RemediationExecutionStatus)

-- | ENUM of the values.
remediationExecutionStatus_state :: Lens.Lens' RemediationExecutionStatus (Core.Maybe RemediationExecutionState)
remediationExecutionStatus_state = Lens.lens (\RemediationExecutionStatus' {state} -> state) (\s@RemediationExecutionStatus' {} a -> s {state = a} :: RemediationExecutionStatus)

-- | Details of every step.
remediationExecutionStatus_stepDetails :: Lens.Lens' RemediationExecutionStatus (Core.Maybe [RemediationExecutionStep])
remediationExecutionStatus_stepDetails = Lens.lens (\RemediationExecutionStatus' {stepDetails} -> stepDetails) (\s@RemediationExecutionStatus' {} a -> s {stepDetails = a} :: RemediationExecutionStatus) Core.. Lens.mapping Lens._Coerce

-- | The time when the remediation execution was last updated.
remediationExecutionStatus_lastUpdatedTime :: Lens.Lens' RemediationExecutionStatus (Core.Maybe Core.UTCTime)
remediationExecutionStatus_lastUpdatedTime = Lens.lens (\RemediationExecutionStatus' {lastUpdatedTime} -> lastUpdatedTime) (\s@RemediationExecutionStatus' {} a -> s {lastUpdatedTime = a} :: RemediationExecutionStatus) Core.. Lens.mapping Core._Time

instance Core.FromJSON RemediationExecutionStatus where
  parseJSON =
    Core.withObject
      "RemediationExecutionStatus"
      ( \x ->
          RemediationExecutionStatus'
            Core.<$> (x Core..:? "InvocationTime")
            Core.<*> (x Core..:? "ResourceKey")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "StepDetails" Core..!= Core.mempty)
            Core.<*> (x Core..:? "LastUpdatedTime")
      )

instance Core.Hashable RemediationExecutionStatus

instance Core.NFData RemediationExecutionStatus
