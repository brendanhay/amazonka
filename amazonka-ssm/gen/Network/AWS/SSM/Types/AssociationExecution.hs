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
-- Module      : Network.AWS.SSM.Types.AssociationExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationExecution where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Includes information about the specified association.
--
-- /See:/ 'newAssociationExecution' smart constructor.
data AssociationExecution = AssociationExecution'
  { -- | The status of the association execution.
    status :: Core.Maybe Core.Text,
    -- | The date of the last execution.
    lastExecutionDate :: Core.Maybe Core.POSIX,
    -- | Detailed status information about the execution.
    detailedStatus :: Core.Maybe Core.Text,
    -- | An aggregate status of the resources in the execution based on the
    -- status type.
    resourceCountByStatus :: Core.Maybe Core.Text,
    -- | The time the execution started.
    createdTime :: Core.Maybe Core.POSIX,
    -- | The execution ID for the association.
    executionId :: Core.Maybe Core.Text,
    -- | The association ID.
    associationId :: Core.Maybe Core.Text,
    -- | The association version.
    associationVersion :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociationExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'associationExecution_status' - The status of the association execution.
--
-- 'lastExecutionDate', 'associationExecution_lastExecutionDate' - The date of the last execution.
--
-- 'detailedStatus', 'associationExecution_detailedStatus' - Detailed status information about the execution.
--
-- 'resourceCountByStatus', 'associationExecution_resourceCountByStatus' - An aggregate status of the resources in the execution based on the
-- status type.
--
-- 'createdTime', 'associationExecution_createdTime' - The time the execution started.
--
-- 'executionId', 'associationExecution_executionId' - The execution ID for the association.
--
-- 'associationId', 'associationExecution_associationId' - The association ID.
--
-- 'associationVersion', 'associationExecution_associationVersion' - The association version.
newAssociationExecution ::
  AssociationExecution
newAssociationExecution =
  AssociationExecution'
    { status = Core.Nothing,
      lastExecutionDate = Core.Nothing,
      detailedStatus = Core.Nothing,
      resourceCountByStatus = Core.Nothing,
      createdTime = Core.Nothing,
      executionId = Core.Nothing,
      associationId = Core.Nothing,
      associationVersion = Core.Nothing
    }

-- | The status of the association execution.
associationExecution_status :: Lens.Lens' AssociationExecution (Core.Maybe Core.Text)
associationExecution_status = Lens.lens (\AssociationExecution' {status} -> status) (\s@AssociationExecution' {} a -> s {status = a} :: AssociationExecution)

-- | The date of the last execution.
associationExecution_lastExecutionDate :: Lens.Lens' AssociationExecution (Core.Maybe Core.UTCTime)
associationExecution_lastExecutionDate = Lens.lens (\AssociationExecution' {lastExecutionDate} -> lastExecutionDate) (\s@AssociationExecution' {} a -> s {lastExecutionDate = a} :: AssociationExecution) Core.. Lens.mapping Core._Time

-- | Detailed status information about the execution.
associationExecution_detailedStatus :: Lens.Lens' AssociationExecution (Core.Maybe Core.Text)
associationExecution_detailedStatus = Lens.lens (\AssociationExecution' {detailedStatus} -> detailedStatus) (\s@AssociationExecution' {} a -> s {detailedStatus = a} :: AssociationExecution)

-- | An aggregate status of the resources in the execution based on the
-- status type.
associationExecution_resourceCountByStatus :: Lens.Lens' AssociationExecution (Core.Maybe Core.Text)
associationExecution_resourceCountByStatus = Lens.lens (\AssociationExecution' {resourceCountByStatus} -> resourceCountByStatus) (\s@AssociationExecution' {} a -> s {resourceCountByStatus = a} :: AssociationExecution)

-- | The time the execution started.
associationExecution_createdTime :: Lens.Lens' AssociationExecution (Core.Maybe Core.UTCTime)
associationExecution_createdTime = Lens.lens (\AssociationExecution' {createdTime} -> createdTime) (\s@AssociationExecution' {} a -> s {createdTime = a} :: AssociationExecution) Core.. Lens.mapping Core._Time

-- | The execution ID for the association.
associationExecution_executionId :: Lens.Lens' AssociationExecution (Core.Maybe Core.Text)
associationExecution_executionId = Lens.lens (\AssociationExecution' {executionId} -> executionId) (\s@AssociationExecution' {} a -> s {executionId = a} :: AssociationExecution)

-- | The association ID.
associationExecution_associationId :: Lens.Lens' AssociationExecution (Core.Maybe Core.Text)
associationExecution_associationId = Lens.lens (\AssociationExecution' {associationId} -> associationId) (\s@AssociationExecution' {} a -> s {associationId = a} :: AssociationExecution)

-- | The association version.
associationExecution_associationVersion :: Lens.Lens' AssociationExecution (Core.Maybe Core.Text)
associationExecution_associationVersion = Lens.lens (\AssociationExecution' {associationVersion} -> associationVersion) (\s@AssociationExecution' {} a -> s {associationVersion = a} :: AssociationExecution)

instance Core.FromJSON AssociationExecution where
  parseJSON =
    Core.withObject
      "AssociationExecution"
      ( \x ->
          AssociationExecution'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "LastExecutionDate")
            Core.<*> (x Core..:? "DetailedStatus")
            Core.<*> (x Core..:? "ResourceCountByStatus")
            Core.<*> (x Core..:? "CreatedTime")
            Core.<*> (x Core..:? "ExecutionId")
            Core.<*> (x Core..:? "AssociationId")
            Core.<*> (x Core..:? "AssociationVersion")
      )

instance Core.Hashable AssociationExecution

instance Core.NFData AssociationExecution
