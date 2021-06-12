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
-- Module      : Network.AWS.SSM.Types.AssociationExecutionTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationExecutionTarget where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.OutputSource

-- | Includes information about the specified association execution.
--
-- /See:/ 'newAssociationExecutionTarget' smart constructor.
data AssociationExecutionTarget = AssociationExecutionTarget'
  { -- | The resource ID, for example, the instance ID where the association ran.
    resourceId :: Core.Maybe Core.Text,
    -- | The association execution status.
    status :: Core.Maybe Core.Text,
    -- | The date of the last execution.
    lastExecutionDate :: Core.Maybe Core.POSIX,
    -- | Detailed information about the execution status.
    detailedStatus :: Core.Maybe Core.Text,
    -- | The resource type, for example, instance.
    resourceType :: Core.Maybe Core.Text,
    -- | The location where the association details are saved.
    outputSource :: Core.Maybe OutputSource,
    -- | The execution ID.
    executionId :: Core.Maybe Core.Text,
    -- | The association ID.
    associationId :: Core.Maybe Core.Text,
    -- | The association version.
    associationVersion :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociationExecutionTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'associationExecutionTarget_resourceId' - The resource ID, for example, the instance ID where the association ran.
--
-- 'status', 'associationExecutionTarget_status' - The association execution status.
--
-- 'lastExecutionDate', 'associationExecutionTarget_lastExecutionDate' - The date of the last execution.
--
-- 'detailedStatus', 'associationExecutionTarget_detailedStatus' - Detailed information about the execution status.
--
-- 'resourceType', 'associationExecutionTarget_resourceType' - The resource type, for example, instance.
--
-- 'outputSource', 'associationExecutionTarget_outputSource' - The location where the association details are saved.
--
-- 'executionId', 'associationExecutionTarget_executionId' - The execution ID.
--
-- 'associationId', 'associationExecutionTarget_associationId' - The association ID.
--
-- 'associationVersion', 'associationExecutionTarget_associationVersion' - The association version.
newAssociationExecutionTarget ::
  AssociationExecutionTarget
newAssociationExecutionTarget =
  AssociationExecutionTarget'
    { resourceId =
        Core.Nothing,
      status = Core.Nothing,
      lastExecutionDate = Core.Nothing,
      detailedStatus = Core.Nothing,
      resourceType = Core.Nothing,
      outputSource = Core.Nothing,
      executionId = Core.Nothing,
      associationId = Core.Nothing,
      associationVersion = Core.Nothing
    }

-- | The resource ID, for example, the instance ID where the association ran.
associationExecutionTarget_resourceId :: Lens.Lens' AssociationExecutionTarget (Core.Maybe Core.Text)
associationExecutionTarget_resourceId = Lens.lens (\AssociationExecutionTarget' {resourceId} -> resourceId) (\s@AssociationExecutionTarget' {} a -> s {resourceId = a} :: AssociationExecutionTarget)

-- | The association execution status.
associationExecutionTarget_status :: Lens.Lens' AssociationExecutionTarget (Core.Maybe Core.Text)
associationExecutionTarget_status = Lens.lens (\AssociationExecutionTarget' {status} -> status) (\s@AssociationExecutionTarget' {} a -> s {status = a} :: AssociationExecutionTarget)

-- | The date of the last execution.
associationExecutionTarget_lastExecutionDate :: Lens.Lens' AssociationExecutionTarget (Core.Maybe Core.UTCTime)
associationExecutionTarget_lastExecutionDate = Lens.lens (\AssociationExecutionTarget' {lastExecutionDate} -> lastExecutionDate) (\s@AssociationExecutionTarget' {} a -> s {lastExecutionDate = a} :: AssociationExecutionTarget) Core.. Lens.mapping Core._Time

-- | Detailed information about the execution status.
associationExecutionTarget_detailedStatus :: Lens.Lens' AssociationExecutionTarget (Core.Maybe Core.Text)
associationExecutionTarget_detailedStatus = Lens.lens (\AssociationExecutionTarget' {detailedStatus} -> detailedStatus) (\s@AssociationExecutionTarget' {} a -> s {detailedStatus = a} :: AssociationExecutionTarget)

-- | The resource type, for example, instance.
associationExecutionTarget_resourceType :: Lens.Lens' AssociationExecutionTarget (Core.Maybe Core.Text)
associationExecutionTarget_resourceType = Lens.lens (\AssociationExecutionTarget' {resourceType} -> resourceType) (\s@AssociationExecutionTarget' {} a -> s {resourceType = a} :: AssociationExecutionTarget)

-- | The location where the association details are saved.
associationExecutionTarget_outputSource :: Lens.Lens' AssociationExecutionTarget (Core.Maybe OutputSource)
associationExecutionTarget_outputSource = Lens.lens (\AssociationExecutionTarget' {outputSource} -> outputSource) (\s@AssociationExecutionTarget' {} a -> s {outputSource = a} :: AssociationExecutionTarget)

-- | The execution ID.
associationExecutionTarget_executionId :: Lens.Lens' AssociationExecutionTarget (Core.Maybe Core.Text)
associationExecutionTarget_executionId = Lens.lens (\AssociationExecutionTarget' {executionId} -> executionId) (\s@AssociationExecutionTarget' {} a -> s {executionId = a} :: AssociationExecutionTarget)

-- | The association ID.
associationExecutionTarget_associationId :: Lens.Lens' AssociationExecutionTarget (Core.Maybe Core.Text)
associationExecutionTarget_associationId = Lens.lens (\AssociationExecutionTarget' {associationId} -> associationId) (\s@AssociationExecutionTarget' {} a -> s {associationId = a} :: AssociationExecutionTarget)

-- | The association version.
associationExecutionTarget_associationVersion :: Lens.Lens' AssociationExecutionTarget (Core.Maybe Core.Text)
associationExecutionTarget_associationVersion = Lens.lens (\AssociationExecutionTarget' {associationVersion} -> associationVersion) (\s@AssociationExecutionTarget' {} a -> s {associationVersion = a} :: AssociationExecutionTarget)

instance Core.FromJSON AssociationExecutionTarget where
  parseJSON =
    Core.withObject
      "AssociationExecutionTarget"
      ( \x ->
          AssociationExecutionTarget'
            Core.<$> (x Core..:? "ResourceId")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "LastExecutionDate")
            Core.<*> (x Core..:? "DetailedStatus")
            Core.<*> (x Core..:? "ResourceType")
            Core.<*> (x Core..:? "OutputSource")
            Core.<*> (x Core..:? "ExecutionId")
            Core.<*> (x Core..:? "AssociationId")
            Core.<*> (x Core..:? "AssociationVersion")
      )

instance Core.Hashable AssociationExecutionTarget

instance Core.NFData AssociationExecutionTarget
