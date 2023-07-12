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
-- Module      : Amazonka.SSM.Types.AssociationExecution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.AssociationExecution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.AlarmConfiguration
import Amazonka.SSM.Types.AlarmStateInformation

-- | Includes information about the specified association.
--
-- /See:/ 'newAssociationExecution' smart constructor.
data AssociationExecution = AssociationExecution'
  { alarmConfiguration :: Prelude.Maybe AlarmConfiguration,
    -- | The association ID.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The association version.
    associationVersion :: Prelude.Maybe Prelude.Text,
    -- | The time the execution started.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | Detailed status information about the execution.
    detailedStatus :: Prelude.Maybe Prelude.Text,
    -- | The execution ID for the association.
    executionId :: Prelude.Maybe Prelude.Text,
    -- | The date of the last execution.
    lastExecutionDate :: Prelude.Maybe Data.POSIX,
    -- | An aggregate status of the resources in the execution based on the
    -- status type.
    resourceCountByStatus :: Prelude.Maybe Prelude.Text,
    -- | The status of the association execution.
    status :: Prelude.Maybe Prelude.Text,
    -- | The CloudWatch alarms that were invoked by the association.
    triggeredAlarms :: Prelude.Maybe (Prelude.NonEmpty AlarmStateInformation)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociationExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarmConfiguration', 'associationExecution_alarmConfiguration' - Undocumented member.
--
-- 'associationId', 'associationExecution_associationId' - The association ID.
--
-- 'associationVersion', 'associationExecution_associationVersion' - The association version.
--
-- 'createdTime', 'associationExecution_createdTime' - The time the execution started.
--
-- 'detailedStatus', 'associationExecution_detailedStatus' - Detailed status information about the execution.
--
-- 'executionId', 'associationExecution_executionId' - The execution ID for the association.
--
-- 'lastExecutionDate', 'associationExecution_lastExecutionDate' - The date of the last execution.
--
-- 'resourceCountByStatus', 'associationExecution_resourceCountByStatus' - An aggregate status of the resources in the execution based on the
-- status type.
--
-- 'status', 'associationExecution_status' - The status of the association execution.
--
-- 'triggeredAlarms', 'associationExecution_triggeredAlarms' - The CloudWatch alarms that were invoked by the association.
newAssociationExecution ::
  AssociationExecution
newAssociationExecution =
  AssociationExecution'
    { alarmConfiguration =
        Prelude.Nothing,
      associationId = Prelude.Nothing,
      associationVersion = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      detailedStatus = Prelude.Nothing,
      executionId = Prelude.Nothing,
      lastExecutionDate = Prelude.Nothing,
      resourceCountByStatus = Prelude.Nothing,
      status = Prelude.Nothing,
      triggeredAlarms = Prelude.Nothing
    }

-- | Undocumented member.
associationExecution_alarmConfiguration :: Lens.Lens' AssociationExecution (Prelude.Maybe AlarmConfiguration)
associationExecution_alarmConfiguration = Lens.lens (\AssociationExecution' {alarmConfiguration} -> alarmConfiguration) (\s@AssociationExecution' {} a -> s {alarmConfiguration = a} :: AssociationExecution)

-- | The association ID.
associationExecution_associationId :: Lens.Lens' AssociationExecution (Prelude.Maybe Prelude.Text)
associationExecution_associationId = Lens.lens (\AssociationExecution' {associationId} -> associationId) (\s@AssociationExecution' {} a -> s {associationId = a} :: AssociationExecution)

-- | The association version.
associationExecution_associationVersion :: Lens.Lens' AssociationExecution (Prelude.Maybe Prelude.Text)
associationExecution_associationVersion = Lens.lens (\AssociationExecution' {associationVersion} -> associationVersion) (\s@AssociationExecution' {} a -> s {associationVersion = a} :: AssociationExecution)

-- | The time the execution started.
associationExecution_createdTime :: Lens.Lens' AssociationExecution (Prelude.Maybe Prelude.UTCTime)
associationExecution_createdTime = Lens.lens (\AssociationExecution' {createdTime} -> createdTime) (\s@AssociationExecution' {} a -> s {createdTime = a} :: AssociationExecution) Prelude.. Lens.mapping Data._Time

-- | Detailed status information about the execution.
associationExecution_detailedStatus :: Lens.Lens' AssociationExecution (Prelude.Maybe Prelude.Text)
associationExecution_detailedStatus = Lens.lens (\AssociationExecution' {detailedStatus} -> detailedStatus) (\s@AssociationExecution' {} a -> s {detailedStatus = a} :: AssociationExecution)

-- | The execution ID for the association.
associationExecution_executionId :: Lens.Lens' AssociationExecution (Prelude.Maybe Prelude.Text)
associationExecution_executionId = Lens.lens (\AssociationExecution' {executionId} -> executionId) (\s@AssociationExecution' {} a -> s {executionId = a} :: AssociationExecution)

-- | The date of the last execution.
associationExecution_lastExecutionDate :: Lens.Lens' AssociationExecution (Prelude.Maybe Prelude.UTCTime)
associationExecution_lastExecutionDate = Lens.lens (\AssociationExecution' {lastExecutionDate} -> lastExecutionDate) (\s@AssociationExecution' {} a -> s {lastExecutionDate = a} :: AssociationExecution) Prelude.. Lens.mapping Data._Time

-- | An aggregate status of the resources in the execution based on the
-- status type.
associationExecution_resourceCountByStatus :: Lens.Lens' AssociationExecution (Prelude.Maybe Prelude.Text)
associationExecution_resourceCountByStatus = Lens.lens (\AssociationExecution' {resourceCountByStatus} -> resourceCountByStatus) (\s@AssociationExecution' {} a -> s {resourceCountByStatus = a} :: AssociationExecution)

-- | The status of the association execution.
associationExecution_status :: Lens.Lens' AssociationExecution (Prelude.Maybe Prelude.Text)
associationExecution_status = Lens.lens (\AssociationExecution' {status} -> status) (\s@AssociationExecution' {} a -> s {status = a} :: AssociationExecution)

-- | The CloudWatch alarms that were invoked by the association.
associationExecution_triggeredAlarms :: Lens.Lens' AssociationExecution (Prelude.Maybe (Prelude.NonEmpty AlarmStateInformation))
associationExecution_triggeredAlarms = Lens.lens (\AssociationExecution' {triggeredAlarms} -> triggeredAlarms) (\s@AssociationExecution' {} a -> s {triggeredAlarms = a} :: AssociationExecution) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AssociationExecution where
  parseJSON =
    Data.withObject
      "AssociationExecution"
      ( \x ->
          AssociationExecution'
            Prelude.<$> (x Data..:? "AlarmConfiguration")
            Prelude.<*> (x Data..:? "AssociationId")
            Prelude.<*> (x Data..:? "AssociationVersion")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "DetailedStatus")
            Prelude.<*> (x Data..:? "ExecutionId")
            Prelude.<*> (x Data..:? "LastExecutionDate")
            Prelude.<*> (x Data..:? "ResourceCountByStatus")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "TriggeredAlarms")
      )

instance Prelude.Hashable AssociationExecution where
  hashWithSalt _salt AssociationExecution' {..} =
    _salt
      `Prelude.hashWithSalt` alarmConfiguration
      `Prelude.hashWithSalt` associationId
      `Prelude.hashWithSalt` associationVersion
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` detailedStatus
      `Prelude.hashWithSalt` executionId
      `Prelude.hashWithSalt` lastExecutionDate
      `Prelude.hashWithSalt` resourceCountByStatus
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` triggeredAlarms

instance Prelude.NFData AssociationExecution where
  rnf AssociationExecution' {..} =
    Prelude.rnf alarmConfiguration
      `Prelude.seq` Prelude.rnf associationId
      `Prelude.seq` Prelude.rnf associationVersion
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf detailedStatus
      `Prelude.seq` Prelude.rnf executionId
      `Prelude.seq` Prelude.rnf lastExecutionDate
      `Prelude.seq` Prelude.rnf resourceCountByStatus
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf triggeredAlarms
