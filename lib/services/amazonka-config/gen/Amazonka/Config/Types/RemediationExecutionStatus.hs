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
-- Module      : Amazonka.Config.Types.RemediationExecutionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.RemediationExecutionStatus where

import Amazonka.Config.Types.RemediationExecutionState
import Amazonka.Config.Types.RemediationExecutionStep
import Amazonka.Config.Types.ResourceKey
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides details of the current status of the invoked remediation action
-- for that resource.
--
-- /See:/ 'newRemediationExecutionStatus' smart constructor.
data RemediationExecutionStatus = RemediationExecutionStatus'
  { -- | Start time when the remediation was executed.
    invocationTime :: Prelude.Maybe Data.POSIX,
    -- | The time when the remediation execution was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    resourceKey :: Prelude.Maybe ResourceKey,
    -- | ENUM of the values.
    state :: Prelude.Maybe RemediationExecutionState,
    -- | Details of every step.
    stepDetails :: Prelude.Maybe [RemediationExecutionStep]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'lastUpdatedTime', 'remediationExecutionStatus_lastUpdatedTime' - The time when the remediation execution was last updated.
--
-- 'resourceKey', 'remediationExecutionStatus_resourceKey' - Undocumented member.
--
-- 'state', 'remediationExecutionStatus_state' - ENUM of the values.
--
-- 'stepDetails', 'remediationExecutionStatus_stepDetails' - Details of every step.
newRemediationExecutionStatus ::
  RemediationExecutionStatus
newRemediationExecutionStatus =
  RemediationExecutionStatus'
    { invocationTime =
        Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      resourceKey = Prelude.Nothing,
      state = Prelude.Nothing,
      stepDetails = Prelude.Nothing
    }

-- | Start time when the remediation was executed.
remediationExecutionStatus_invocationTime :: Lens.Lens' RemediationExecutionStatus (Prelude.Maybe Prelude.UTCTime)
remediationExecutionStatus_invocationTime = Lens.lens (\RemediationExecutionStatus' {invocationTime} -> invocationTime) (\s@RemediationExecutionStatus' {} a -> s {invocationTime = a} :: RemediationExecutionStatus) Prelude.. Lens.mapping Data._Time

-- | The time when the remediation execution was last updated.
remediationExecutionStatus_lastUpdatedTime :: Lens.Lens' RemediationExecutionStatus (Prelude.Maybe Prelude.UTCTime)
remediationExecutionStatus_lastUpdatedTime = Lens.lens (\RemediationExecutionStatus' {lastUpdatedTime} -> lastUpdatedTime) (\s@RemediationExecutionStatus' {} a -> s {lastUpdatedTime = a} :: RemediationExecutionStatus) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
remediationExecutionStatus_resourceKey :: Lens.Lens' RemediationExecutionStatus (Prelude.Maybe ResourceKey)
remediationExecutionStatus_resourceKey = Lens.lens (\RemediationExecutionStatus' {resourceKey} -> resourceKey) (\s@RemediationExecutionStatus' {} a -> s {resourceKey = a} :: RemediationExecutionStatus)

-- | ENUM of the values.
remediationExecutionStatus_state :: Lens.Lens' RemediationExecutionStatus (Prelude.Maybe RemediationExecutionState)
remediationExecutionStatus_state = Lens.lens (\RemediationExecutionStatus' {state} -> state) (\s@RemediationExecutionStatus' {} a -> s {state = a} :: RemediationExecutionStatus)

-- | Details of every step.
remediationExecutionStatus_stepDetails :: Lens.Lens' RemediationExecutionStatus (Prelude.Maybe [RemediationExecutionStep])
remediationExecutionStatus_stepDetails = Lens.lens (\RemediationExecutionStatus' {stepDetails} -> stepDetails) (\s@RemediationExecutionStatus' {} a -> s {stepDetails = a} :: RemediationExecutionStatus) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON RemediationExecutionStatus where
  parseJSON =
    Data.withObject
      "RemediationExecutionStatus"
      ( \x ->
          RemediationExecutionStatus'
            Prelude.<$> (x Data..:? "InvocationTime")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "ResourceKey")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "StepDetails" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable RemediationExecutionStatus where
  hashWithSalt _salt RemediationExecutionStatus' {..} =
    _salt
      `Prelude.hashWithSalt` invocationTime
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` resourceKey
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` stepDetails

instance Prelude.NFData RemediationExecutionStatus where
  rnf RemediationExecutionStatus' {..} =
    Prelude.rnf invocationTime
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf resourceKey
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf stepDetails
