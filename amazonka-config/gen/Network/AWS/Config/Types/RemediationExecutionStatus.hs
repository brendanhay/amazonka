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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides details of the current status of the invoked remediation action
-- for that resource.
--
-- /See:/ 'newRemediationExecutionStatus' smart constructor.
data RemediationExecutionStatus = RemediationExecutionStatus'
  { -- | Start time when the remediation was executed.
    invocationTime :: Prelude.Maybe Prelude.POSIX,
    resourceKey :: Prelude.Maybe ResourceKey,
    -- | ENUM of the values.
    state :: Prelude.Maybe RemediationExecutionState,
    -- | Details of every step.
    stepDetails :: Prelude.Maybe [RemediationExecutionStep],
    -- | The time when the remediation execution was last updated.
    lastUpdatedTime :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      resourceKey = Prelude.Nothing,
      state = Prelude.Nothing,
      stepDetails = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing
    }

-- | Start time when the remediation was executed.
remediationExecutionStatus_invocationTime :: Lens.Lens' RemediationExecutionStatus (Prelude.Maybe Prelude.UTCTime)
remediationExecutionStatus_invocationTime = Lens.lens (\RemediationExecutionStatus' {invocationTime} -> invocationTime) (\s@RemediationExecutionStatus' {} a -> s {invocationTime = a} :: RemediationExecutionStatus) Prelude.. Lens.mapping Prelude._Time

-- | Undocumented member.
remediationExecutionStatus_resourceKey :: Lens.Lens' RemediationExecutionStatus (Prelude.Maybe ResourceKey)
remediationExecutionStatus_resourceKey = Lens.lens (\RemediationExecutionStatus' {resourceKey} -> resourceKey) (\s@RemediationExecutionStatus' {} a -> s {resourceKey = a} :: RemediationExecutionStatus)

-- | ENUM of the values.
remediationExecutionStatus_state :: Lens.Lens' RemediationExecutionStatus (Prelude.Maybe RemediationExecutionState)
remediationExecutionStatus_state = Lens.lens (\RemediationExecutionStatus' {state} -> state) (\s@RemediationExecutionStatus' {} a -> s {state = a} :: RemediationExecutionStatus)

-- | Details of every step.
remediationExecutionStatus_stepDetails :: Lens.Lens' RemediationExecutionStatus (Prelude.Maybe [RemediationExecutionStep])
remediationExecutionStatus_stepDetails = Lens.lens (\RemediationExecutionStatus' {stepDetails} -> stepDetails) (\s@RemediationExecutionStatus' {} a -> s {stepDetails = a} :: RemediationExecutionStatus) Prelude.. Lens.mapping Prelude._Coerce

-- | The time when the remediation execution was last updated.
remediationExecutionStatus_lastUpdatedTime :: Lens.Lens' RemediationExecutionStatus (Prelude.Maybe Prelude.UTCTime)
remediationExecutionStatus_lastUpdatedTime = Lens.lens (\RemediationExecutionStatus' {lastUpdatedTime} -> lastUpdatedTime) (\s@RemediationExecutionStatus' {} a -> s {lastUpdatedTime = a} :: RemediationExecutionStatus) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON RemediationExecutionStatus where
  parseJSON =
    Prelude.withObject
      "RemediationExecutionStatus"
      ( \x ->
          RemediationExecutionStatus'
            Prelude.<$> (x Prelude..:? "InvocationTime")
            Prelude.<*> (x Prelude..:? "ResourceKey")
            Prelude.<*> (x Prelude..:? "State")
            Prelude.<*> ( x Prelude..:? "StepDetails"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "LastUpdatedTime")
      )

instance Prelude.Hashable RemediationExecutionStatus

instance Prelude.NFData RemediationExecutionStatus
