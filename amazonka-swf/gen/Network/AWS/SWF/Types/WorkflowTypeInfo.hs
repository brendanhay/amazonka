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
-- Module      : Network.AWS.SWF.Types.WorkflowTypeInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowTypeInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SWF.Types.RegistrationStatus
import Network.AWS.SWF.Types.WorkflowType

-- | Contains information about a workflow type.
--
-- /See:/ 'newWorkflowTypeInfo' smart constructor.
data WorkflowTypeInfo = WorkflowTypeInfo'
  { -- | If the type is in deprecated state, then it is set to the date when the
    -- type was deprecated.
    deprecationDate :: Core.Maybe Core.POSIX,
    -- | The description of the type registered through RegisterWorkflowType.
    description :: Core.Maybe Core.Text,
    -- | The workflow type this information is about.
    workflowType :: WorkflowType,
    -- | The current status of the workflow type.
    status :: RegistrationStatus,
    -- | The date when this type was registered.
    creationDate :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'WorkflowTypeInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deprecationDate', 'workflowTypeInfo_deprecationDate' - If the type is in deprecated state, then it is set to the date when the
-- type was deprecated.
--
-- 'description', 'workflowTypeInfo_description' - The description of the type registered through RegisterWorkflowType.
--
-- 'workflowType', 'workflowTypeInfo_workflowType' - The workflow type this information is about.
--
-- 'status', 'workflowTypeInfo_status' - The current status of the workflow type.
--
-- 'creationDate', 'workflowTypeInfo_creationDate' - The date when this type was registered.
newWorkflowTypeInfo ::
  -- | 'workflowType'
  WorkflowType ->
  -- | 'status'
  RegistrationStatus ->
  -- | 'creationDate'
  Core.UTCTime ->
  WorkflowTypeInfo
newWorkflowTypeInfo
  pWorkflowType_
  pStatus_
  pCreationDate_ =
    WorkflowTypeInfo'
      { deprecationDate = Core.Nothing,
        description = Core.Nothing,
        workflowType = pWorkflowType_,
        status = pStatus_,
        creationDate = Core._Time Lens.# pCreationDate_
      }

-- | If the type is in deprecated state, then it is set to the date when the
-- type was deprecated.
workflowTypeInfo_deprecationDate :: Lens.Lens' WorkflowTypeInfo (Core.Maybe Core.UTCTime)
workflowTypeInfo_deprecationDate = Lens.lens (\WorkflowTypeInfo' {deprecationDate} -> deprecationDate) (\s@WorkflowTypeInfo' {} a -> s {deprecationDate = a} :: WorkflowTypeInfo) Core.. Lens.mapping Core._Time

-- | The description of the type registered through RegisterWorkflowType.
workflowTypeInfo_description :: Lens.Lens' WorkflowTypeInfo (Core.Maybe Core.Text)
workflowTypeInfo_description = Lens.lens (\WorkflowTypeInfo' {description} -> description) (\s@WorkflowTypeInfo' {} a -> s {description = a} :: WorkflowTypeInfo)

-- | The workflow type this information is about.
workflowTypeInfo_workflowType :: Lens.Lens' WorkflowTypeInfo WorkflowType
workflowTypeInfo_workflowType = Lens.lens (\WorkflowTypeInfo' {workflowType} -> workflowType) (\s@WorkflowTypeInfo' {} a -> s {workflowType = a} :: WorkflowTypeInfo)

-- | The current status of the workflow type.
workflowTypeInfo_status :: Lens.Lens' WorkflowTypeInfo RegistrationStatus
workflowTypeInfo_status = Lens.lens (\WorkflowTypeInfo' {status} -> status) (\s@WorkflowTypeInfo' {} a -> s {status = a} :: WorkflowTypeInfo)

-- | The date when this type was registered.
workflowTypeInfo_creationDate :: Lens.Lens' WorkflowTypeInfo Core.UTCTime
workflowTypeInfo_creationDate = Lens.lens (\WorkflowTypeInfo' {creationDate} -> creationDate) (\s@WorkflowTypeInfo' {} a -> s {creationDate = a} :: WorkflowTypeInfo) Core.. Core._Time

instance Core.FromJSON WorkflowTypeInfo where
  parseJSON =
    Core.withObject
      "WorkflowTypeInfo"
      ( \x ->
          WorkflowTypeInfo'
            Core.<$> (x Core..:? "deprecationDate")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..: "workflowType")
            Core.<*> (x Core..: "status")
            Core.<*> (x Core..: "creationDate")
      )

instance Core.Hashable WorkflowTypeInfo

instance Core.NFData WorkflowTypeInfo
