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
-- Module      : Network.AWS.SSM.Types.AssociationExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationExecution where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Includes information about the specified association.
--
-- /See:/ 'newAssociationExecution' smart constructor.
data AssociationExecution = AssociationExecution'
  { -- | The status of the association execution.
    status :: Prelude.Maybe Prelude.Text,
    -- | The date of the last execution.
    lastExecutionDate :: Prelude.Maybe Prelude.POSIX,
    -- | Detailed status information about the execution.
    detailedStatus :: Prelude.Maybe Prelude.Text,
    -- | An aggregate status of the resources in the execution based on the
    -- status type.
    resourceCountByStatus :: Prelude.Maybe Prelude.Text,
    -- | The time the execution started.
    createdTime :: Prelude.Maybe Prelude.POSIX,
    -- | The execution ID for the association.
    executionId :: Prelude.Maybe Prelude.Text,
    -- | The association ID.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The association version.
    associationVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { status = Prelude.Nothing,
      lastExecutionDate = Prelude.Nothing,
      detailedStatus = Prelude.Nothing,
      resourceCountByStatus = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      executionId = Prelude.Nothing,
      associationId = Prelude.Nothing,
      associationVersion = Prelude.Nothing
    }

-- | The status of the association execution.
associationExecution_status :: Lens.Lens' AssociationExecution (Prelude.Maybe Prelude.Text)
associationExecution_status = Lens.lens (\AssociationExecution' {status} -> status) (\s@AssociationExecution' {} a -> s {status = a} :: AssociationExecution)

-- | The date of the last execution.
associationExecution_lastExecutionDate :: Lens.Lens' AssociationExecution (Prelude.Maybe Prelude.UTCTime)
associationExecution_lastExecutionDate = Lens.lens (\AssociationExecution' {lastExecutionDate} -> lastExecutionDate) (\s@AssociationExecution' {} a -> s {lastExecutionDate = a} :: AssociationExecution) Prelude.. Lens.mapping Prelude._Time

-- | Detailed status information about the execution.
associationExecution_detailedStatus :: Lens.Lens' AssociationExecution (Prelude.Maybe Prelude.Text)
associationExecution_detailedStatus = Lens.lens (\AssociationExecution' {detailedStatus} -> detailedStatus) (\s@AssociationExecution' {} a -> s {detailedStatus = a} :: AssociationExecution)

-- | An aggregate status of the resources in the execution based on the
-- status type.
associationExecution_resourceCountByStatus :: Lens.Lens' AssociationExecution (Prelude.Maybe Prelude.Text)
associationExecution_resourceCountByStatus = Lens.lens (\AssociationExecution' {resourceCountByStatus} -> resourceCountByStatus) (\s@AssociationExecution' {} a -> s {resourceCountByStatus = a} :: AssociationExecution)

-- | The time the execution started.
associationExecution_createdTime :: Lens.Lens' AssociationExecution (Prelude.Maybe Prelude.UTCTime)
associationExecution_createdTime = Lens.lens (\AssociationExecution' {createdTime} -> createdTime) (\s@AssociationExecution' {} a -> s {createdTime = a} :: AssociationExecution) Prelude.. Lens.mapping Prelude._Time

-- | The execution ID for the association.
associationExecution_executionId :: Lens.Lens' AssociationExecution (Prelude.Maybe Prelude.Text)
associationExecution_executionId = Lens.lens (\AssociationExecution' {executionId} -> executionId) (\s@AssociationExecution' {} a -> s {executionId = a} :: AssociationExecution)

-- | The association ID.
associationExecution_associationId :: Lens.Lens' AssociationExecution (Prelude.Maybe Prelude.Text)
associationExecution_associationId = Lens.lens (\AssociationExecution' {associationId} -> associationId) (\s@AssociationExecution' {} a -> s {associationId = a} :: AssociationExecution)

-- | The association version.
associationExecution_associationVersion :: Lens.Lens' AssociationExecution (Prelude.Maybe Prelude.Text)
associationExecution_associationVersion = Lens.lens (\AssociationExecution' {associationVersion} -> associationVersion) (\s@AssociationExecution' {} a -> s {associationVersion = a} :: AssociationExecution)

instance Prelude.FromJSON AssociationExecution where
  parseJSON =
    Prelude.withObject
      "AssociationExecution"
      ( \x ->
          AssociationExecution'
            Prelude.<$> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "LastExecutionDate")
            Prelude.<*> (x Prelude..:? "DetailedStatus")
            Prelude.<*> (x Prelude..:? "ResourceCountByStatus")
            Prelude.<*> (x Prelude..:? "CreatedTime")
            Prelude.<*> (x Prelude..:? "ExecutionId")
            Prelude.<*> (x Prelude..:? "AssociationId")
            Prelude.<*> (x Prelude..:? "AssociationVersion")
      )

instance Prelude.Hashable AssociationExecution

instance Prelude.NFData AssociationExecution
