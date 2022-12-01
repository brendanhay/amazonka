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
-- Module      : Amazonka.SSM.Types.AssociationExecutionTarget
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.AssociationExecutionTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.OutputSource

-- | Includes information about the specified association execution.
--
-- /See:/ 'newAssociationExecutionTarget' smart constructor.
data AssociationExecutionTarget = AssociationExecutionTarget'
  { -- | The resource ID, for example, the managed node ID where the association
    -- ran.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The resource type, for example, EC2.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The association version.
    associationVersion :: Prelude.Maybe Prelude.Text,
    -- | The association execution status.
    status :: Prelude.Maybe Prelude.Text,
    -- | The execution ID.
    executionId :: Prelude.Maybe Prelude.Text,
    -- | The location where the association details are saved.
    outputSource :: Prelude.Maybe OutputSource,
    -- | Detailed information about the execution status.
    detailedStatus :: Prelude.Maybe Prelude.Text,
    -- | The date of the last execution.
    lastExecutionDate :: Prelude.Maybe Core.POSIX,
    -- | The association ID.
    associationId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociationExecutionTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'associationExecutionTarget_resourceId' - The resource ID, for example, the managed node ID where the association
-- ran.
--
-- 'resourceType', 'associationExecutionTarget_resourceType' - The resource type, for example, EC2.
--
-- 'associationVersion', 'associationExecutionTarget_associationVersion' - The association version.
--
-- 'status', 'associationExecutionTarget_status' - The association execution status.
--
-- 'executionId', 'associationExecutionTarget_executionId' - The execution ID.
--
-- 'outputSource', 'associationExecutionTarget_outputSource' - The location where the association details are saved.
--
-- 'detailedStatus', 'associationExecutionTarget_detailedStatus' - Detailed information about the execution status.
--
-- 'lastExecutionDate', 'associationExecutionTarget_lastExecutionDate' - The date of the last execution.
--
-- 'associationId', 'associationExecutionTarget_associationId' - The association ID.
newAssociationExecutionTarget ::
  AssociationExecutionTarget
newAssociationExecutionTarget =
  AssociationExecutionTarget'
    { resourceId =
        Prelude.Nothing,
      resourceType = Prelude.Nothing,
      associationVersion = Prelude.Nothing,
      status = Prelude.Nothing,
      executionId = Prelude.Nothing,
      outputSource = Prelude.Nothing,
      detailedStatus = Prelude.Nothing,
      lastExecutionDate = Prelude.Nothing,
      associationId = Prelude.Nothing
    }

-- | The resource ID, for example, the managed node ID where the association
-- ran.
associationExecutionTarget_resourceId :: Lens.Lens' AssociationExecutionTarget (Prelude.Maybe Prelude.Text)
associationExecutionTarget_resourceId = Lens.lens (\AssociationExecutionTarget' {resourceId} -> resourceId) (\s@AssociationExecutionTarget' {} a -> s {resourceId = a} :: AssociationExecutionTarget)

-- | The resource type, for example, EC2.
associationExecutionTarget_resourceType :: Lens.Lens' AssociationExecutionTarget (Prelude.Maybe Prelude.Text)
associationExecutionTarget_resourceType = Lens.lens (\AssociationExecutionTarget' {resourceType} -> resourceType) (\s@AssociationExecutionTarget' {} a -> s {resourceType = a} :: AssociationExecutionTarget)

-- | The association version.
associationExecutionTarget_associationVersion :: Lens.Lens' AssociationExecutionTarget (Prelude.Maybe Prelude.Text)
associationExecutionTarget_associationVersion = Lens.lens (\AssociationExecutionTarget' {associationVersion} -> associationVersion) (\s@AssociationExecutionTarget' {} a -> s {associationVersion = a} :: AssociationExecutionTarget)

-- | The association execution status.
associationExecutionTarget_status :: Lens.Lens' AssociationExecutionTarget (Prelude.Maybe Prelude.Text)
associationExecutionTarget_status = Lens.lens (\AssociationExecutionTarget' {status} -> status) (\s@AssociationExecutionTarget' {} a -> s {status = a} :: AssociationExecutionTarget)

-- | The execution ID.
associationExecutionTarget_executionId :: Lens.Lens' AssociationExecutionTarget (Prelude.Maybe Prelude.Text)
associationExecutionTarget_executionId = Lens.lens (\AssociationExecutionTarget' {executionId} -> executionId) (\s@AssociationExecutionTarget' {} a -> s {executionId = a} :: AssociationExecutionTarget)

-- | The location where the association details are saved.
associationExecutionTarget_outputSource :: Lens.Lens' AssociationExecutionTarget (Prelude.Maybe OutputSource)
associationExecutionTarget_outputSource = Lens.lens (\AssociationExecutionTarget' {outputSource} -> outputSource) (\s@AssociationExecutionTarget' {} a -> s {outputSource = a} :: AssociationExecutionTarget)

-- | Detailed information about the execution status.
associationExecutionTarget_detailedStatus :: Lens.Lens' AssociationExecutionTarget (Prelude.Maybe Prelude.Text)
associationExecutionTarget_detailedStatus = Lens.lens (\AssociationExecutionTarget' {detailedStatus} -> detailedStatus) (\s@AssociationExecutionTarget' {} a -> s {detailedStatus = a} :: AssociationExecutionTarget)

-- | The date of the last execution.
associationExecutionTarget_lastExecutionDate :: Lens.Lens' AssociationExecutionTarget (Prelude.Maybe Prelude.UTCTime)
associationExecutionTarget_lastExecutionDate = Lens.lens (\AssociationExecutionTarget' {lastExecutionDate} -> lastExecutionDate) (\s@AssociationExecutionTarget' {} a -> s {lastExecutionDate = a} :: AssociationExecutionTarget) Prelude.. Lens.mapping Core._Time

-- | The association ID.
associationExecutionTarget_associationId :: Lens.Lens' AssociationExecutionTarget (Prelude.Maybe Prelude.Text)
associationExecutionTarget_associationId = Lens.lens (\AssociationExecutionTarget' {associationId} -> associationId) (\s@AssociationExecutionTarget' {} a -> s {associationId = a} :: AssociationExecutionTarget)

instance Core.FromJSON AssociationExecutionTarget where
  parseJSON =
    Core.withObject
      "AssociationExecutionTarget"
      ( \x ->
          AssociationExecutionTarget'
            Prelude.<$> (x Core..:? "ResourceId")
            Prelude.<*> (x Core..:? "ResourceType")
            Prelude.<*> (x Core..:? "AssociationVersion")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "ExecutionId")
            Prelude.<*> (x Core..:? "OutputSource")
            Prelude.<*> (x Core..:? "DetailedStatus")
            Prelude.<*> (x Core..:? "LastExecutionDate")
            Prelude.<*> (x Core..:? "AssociationId")
      )

instance Prelude.Hashable AssociationExecutionTarget where
  hashWithSalt _salt AssociationExecutionTarget' {..} =
    _salt `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` associationVersion
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` executionId
      `Prelude.hashWithSalt` outputSource
      `Prelude.hashWithSalt` detailedStatus
      `Prelude.hashWithSalt` lastExecutionDate
      `Prelude.hashWithSalt` associationId

instance Prelude.NFData AssociationExecutionTarget where
  rnf AssociationExecutionTarget' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf associationVersion
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf executionId
      `Prelude.seq` Prelude.rnf outputSource
      `Prelude.seq` Prelude.rnf detailedStatus
      `Prelude.seq` Prelude.rnf lastExecutionDate
      `Prelude.seq` Prelude.rnf associationId
