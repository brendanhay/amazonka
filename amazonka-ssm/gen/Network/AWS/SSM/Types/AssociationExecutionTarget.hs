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
-- Module      : Network.AWS.SSM.Types.AssociationExecutionTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationExecutionTarget where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.OutputSource

-- | Includes information about the specified association execution.
--
-- /See:/ 'newAssociationExecutionTarget' smart constructor.
data AssociationExecutionTarget = AssociationExecutionTarget'
  { -- | The resource ID, for example, the instance ID where the association ran.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The association execution status.
    status :: Prelude.Maybe Prelude.Text,
    -- | The date of the last execution.
    lastExecutionDate :: Prelude.Maybe Prelude.POSIX,
    -- | Detailed information about the execution status.
    detailedStatus :: Prelude.Maybe Prelude.Text,
    -- | The resource type, for example, instance.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The location where the association details are saved.
    outputSource :: Prelude.Maybe OutputSource,
    -- | The execution ID.
    executionId :: Prelude.Maybe Prelude.Text,
    -- | The association ID.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The association version.
    associationVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      status = Prelude.Nothing,
      lastExecutionDate = Prelude.Nothing,
      detailedStatus = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      outputSource = Prelude.Nothing,
      executionId = Prelude.Nothing,
      associationId = Prelude.Nothing,
      associationVersion = Prelude.Nothing
    }

-- | The resource ID, for example, the instance ID where the association ran.
associationExecutionTarget_resourceId :: Lens.Lens' AssociationExecutionTarget (Prelude.Maybe Prelude.Text)
associationExecutionTarget_resourceId = Lens.lens (\AssociationExecutionTarget' {resourceId} -> resourceId) (\s@AssociationExecutionTarget' {} a -> s {resourceId = a} :: AssociationExecutionTarget)

-- | The association execution status.
associationExecutionTarget_status :: Lens.Lens' AssociationExecutionTarget (Prelude.Maybe Prelude.Text)
associationExecutionTarget_status = Lens.lens (\AssociationExecutionTarget' {status} -> status) (\s@AssociationExecutionTarget' {} a -> s {status = a} :: AssociationExecutionTarget)

-- | The date of the last execution.
associationExecutionTarget_lastExecutionDate :: Lens.Lens' AssociationExecutionTarget (Prelude.Maybe Prelude.UTCTime)
associationExecutionTarget_lastExecutionDate = Lens.lens (\AssociationExecutionTarget' {lastExecutionDate} -> lastExecutionDate) (\s@AssociationExecutionTarget' {} a -> s {lastExecutionDate = a} :: AssociationExecutionTarget) Prelude.. Lens.mapping Prelude._Time

-- | Detailed information about the execution status.
associationExecutionTarget_detailedStatus :: Lens.Lens' AssociationExecutionTarget (Prelude.Maybe Prelude.Text)
associationExecutionTarget_detailedStatus = Lens.lens (\AssociationExecutionTarget' {detailedStatus} -> detailedStatus) (\s@AssociationExecutionTarget' {} a -> s {detailedStatus = a} :: AssociationExecutionTarget)

-- | The resource type, for example, instance.
associationExecutionTarget_resourceType :: Lens.Lens' AssociationExecutionTarget (Prelude.Maybe Prelude.Text)
associationExecutionTarget_resourceType = Lens.lens (\AssociationExecutionTarget' {resourceType} -> resourceType) (\s@AssociationExecutionTarget' {} a -> s {resourceType = a} :: AssociationExecutionTarget)

-- | The location where the association details are saved.
associationExecutionTarget_outputSource :: Lens.Lens' AssociationExecutionTarget (Prelude.Maybe OutputSource)
associationExecutionTarget_outputSource = Lens.lens (\AssociationExecutionTarget' {outputSource} -> outputSource) (\s@AssociationExecutionTarget' {} a -> s {outputSource = a} :: AssociationExecutionTarget)

-- | The execution ID.
associationExecutionTarget_executionId :: Lens.Lens' AssociationExecutionTarget (Prelude.Maybe Prelude.Text)
associationExecutionTarget_executionId = Lens.lens (\AssociationExecutionTarget' {executionId} -> executionId) (\s@AssociationExecutionTarget' {} a -> s {executionId = a} :: AssociationExecutionTarget)

-- | The association ID.
associationExecutionTarget_associationId :: Lens.Lens' AssociationExecutionTarget (Prelude.Maybe Prelude.Text)
associationExecutionTarget_associationId = Lens.lens (\AssociationExecutionTarget' {associationId} -> associationId) (\s@AssociationExecutionTarget' {} a -> s {associationId = a} :: AssociationExecutionTarget)

-- | The association version.
associationExecutionTarget_associationVersion :: Lens.Lens' AssociationExecutionTarget (Prelude.Maybe Prelude.Text)
associationExecutionTarget_associationVersion = Lens.lens (\AssociationExecutionTarget' {associationVersion} -> associationVersion) (\s@AssociationExecutionTarget' {} a -> s {associationVersion = a} :: AssociationExecutionTarget)

instance Prelude.FromJSON AssociationExecutionTarget where
  parseJSON =
    Prelude.withObject
      "AssociationExecutionTarget"
      ( \x ->
          AssociationExecutionTarget'
            Prelude.<$> (x Prelude..:? "ResourceId")
            Prelude.<*> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "LastExecutionDate")
            Prelude.<*> (x Prelude..:? "DetailedStatus")
            Prelude.<*> (x Prelude..:? "ResourceType")
            Prelude.<*> (x Prelude..:? "OutputSource")
            Prelude.<*> (x Prelude..:? "ExecutionId")
            Prelude.<*> (x Prelude..:? "AssociationId")
            Prelude.<*> (x Prelude..:? "AssociationVersion")
      )

instance Prelude.Hashable AssociationExecutionTarget

instance Prelude.NFData AssociationExecutionTarget
