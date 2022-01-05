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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.AssociationExecution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Includes information about the specified association.
--
-- /See:/ 'newAssociationExecution' smart constructor.
data AssociationExecution = AssociationExecution'
  { -- | The association ID.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | Detailed status information about the execution.
    detailedStatus :: Prelude.Maybe Prelude.Text,
    -- | The status of the association execution.
    status :: Prelude.Maybe Prelude.Text,
    -- | The execution ID for the association.
    executionId :: Prelude.Maybe Prelude.Text,
    -- | The time the execution started.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | An aggregate status of the resources in the execution based on the
    -- status type.
    resourceCountByStatus :: Prelude.Maybe Prelude.Text,
    -- | The date of the last execution.
    lastExecutionDate :: Prelude.Maybe Core.POSIX,
    -- | The association version.
    associationVersion :: Prelude.Maybe Prelude.Text
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
-- 'associationId', 'associationExecution_associationId' - The association ID.
--
-- 'detailedStatus', 'associationExecution_detailedStatus' - Detailed status information about the execution.
--
-- 'status', 'associationExecution_status' - The status of the association execution.
--
-- 'executionId', 'associationExecution_executionId' - The execution ID for the association.
--
-- 'createdTime', 'associationExecution_createdTime' - The time the execution started.
--
-- 'resourceCountByStatus', 'associationExecution_resourceCountByStatus' - An aggregate status of the resources in the execution based on the
-- status type.
--
-- 'lastExecutionDate', 'associationExecution_lastExecutionDate' - The date of the last execution.
--
-- 'associationVersion', 'associationExecution_associationVersion' - The association version.
newAssociationExecution ::
  AssociationExecution
newAssociationExecution =
  AssociationExecution'
    { associationId =
        Prelude.Nothing,
      detailedStatus = Prelude.Nothing,
      status = Prelude.Nothing,
      executionId = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      resourceCountByStatus = Prelude.Nothing,
      lastExecutionDate = Prelude.Nothing,
      associationVersion = Prelude.Nothing
    }

-- | The association ID.
associationExecution_associationId :: Lens.Lens' AssociationExecution (Prelude.Maybe Prelude.Text)
associationExecution_associationId = Lens.lens (\AssociationExecution' {associationId} -> associationId) (\s@AssociationExecution' {} a -> s {associationId = a} :: AssociationExecution)

-- | Detailed status information about the execution.
associationExecution_detailedStatus :: Lens.Lens' AssociationExecution (Prelude.Maybe Prelude.Text)
associationExecution_detailedStatus = Lens.lens (\AssociationExecution' {detailedStatus} -> detailedStatus) (\s@AssociationExecution' {} a -> s {detailedStatus = a} :: AssociationExecution)

-- | The status of the association execution.
associationExecution_status :: Lens.Lens' AssociationExecution (Prelude.Maybe Prelude.Text)
associationExecution_status = Lens.lens (\AssociationExecution' {status} -> status) (\s@AssociationExecution' {} a -> s {status = a} :: AssociationExecution)

-- | The execution ID for the association.
associationExecution_executionId :: Lens.Lens' AssociationExecution (Prelude.Maybe Prelude.Text)
associationExecution_executionId = Lens.lens (\AssociationExecution' {executionId} -> executionId) (\s@AssociationExecution' {} a -> s {executionId = a} :: AssociationExecution)

-- | The time the execution started.
associationExecution_createdTime :: Lens.Lens' AssociationExecution (Prelude.Maybe Prelude.UTCTime)
associationExecution_createdTime = Lens.lens (\AssociationExecution' {createdTime} -> createdTime) (\s@AssociationExecution' {} a -> s {createdTime = a} :: AssociationExecution) Prelude.. Lens.mapping Core._Time

-- | An aggregate status of the resources in the execution based on the
-- status type.
associationExecution_resourceCountByStatus :: Lens.Lens' AssociationExecution (Prelude.Maybe Prelude.Text)
associationExecution_resourceCountByStatus = Lens.lens (\AssociationExecution' {resourceCountByStatus} -> resourceCountByStatus) (\s@AssociationExecution' {} a -> s {resourceCountByStatus = a} :: AssociationExecution)

-- | The date of the last execution.
associationExecution_lastExecutionDate :: Lens.Lens' AssociationExecution (Prelude.Maybe Prelude.UTCTime)
associationExecution_lastExecutionDate = Lens.lens (\AssociationExecution' {lastExecutionDate} -> lastExecutionDate) (\s@AssociationExecution' {} a -> s {lastExecutionDate = a} :: AssociationExecution) Prelude.. Lens.mapping Core._Time

-- | The association version.
associationExecution_associationVersion :: Lens.Lens' AssociationExecution (Prelude.Maybe Prelude.Text)
associationExecution_associationVersion = Lens.lens (\AssociationExecution' {associationVersion} -> associationVersion) (\s@AssociationExecution' {} a -> s {associationVersion = a} :: AssociationExecution)

instance Core.FromJSON AssociationExecution where
  parseJSON =
    Core.withObject
      "AssociationExecution"
      ( \x ->
          AssociationExecution'
            Prelude.<$> (x Core..:? "AssociationId")
            Prelude.<*> (x Core..:? "DetailedStatus")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "ExecutionId")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "ResourceCountByStatus")
            Prelude.<*> (x Core..:? "LastExecutionDate")
            Prelude.<*> (x Core..:? "AssociationVersion")
      )

instance Prelude.Hashable AssociationExecution where
  hashWithSalt _salt AssociationExecution' {..} =
    _salt `Prelude.hashWithSalt` associationId
      `Prelude.hashWithSalt` detailedStatus
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` executionId
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` resourceCountByStatus
      `Prelude.hashWithSalt` lastExecutionDate
      `Prelude.hashWithSalt` associationVersion

instance Prelude.NFData AssociationExecution where
  rnf AssociationExecution' {..} =
    Prelude.rnf associationId
      `Prelude.seq` Prelude.rnf detailedStatus
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf executionId
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf resourceCountByStatus
      `Prelude.seq` Prelude.rnf lastExecutionDate
      `Prelude.seq` Prelude.rnf associationVersion
