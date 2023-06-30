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
-- Module      : Amazonka.SWF.Types.WorkflowTypeInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.WorkflowTypeInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SWF.Types.RegistrationStatus
import Amazonka.SWF.Types.WorkflowType

-- | Contains information about a workflow type.
--
-- /See:/ 'newWorkflowTypeInfo' smart constructor.
data WorkflowTypeInfo = WorkflowTypeInfo'
  { -- | If the type is in deprecated state, then it is set to the date when the
    -- type was deprecated.
    deprecationDate :: Prelude.Maybe Data.POSIX,
    -- | The description of the type registered through RegisterWorkflowType.
    description :: Prelude.Maybe Prelude.Text,
    -- | The workflow type this information is about.
    workflowType :: WorkflowType,
    -- | The current status of the workflow type.
    status :: RegistrationStatus,
    -- | The date when this type was registered.
    creationDate :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.UTCTime ->
  WorkflowTypeInfo
newWorkflowTypeInfo
  pWorkflowType_
  pStatus_
  pCreationDate_ =
    WorkflowTypeInfo'
      { deprecationDate =
          Prelude.Nothing,
        description = Prelude.Nothing,
        workflowType = pWorkflowType_,
        status = pStatus_,
        creationDate = Data._Time Lens.# pCreationDate_
      }

-- | If the type is in deprecated state, then it is set to the date when the
-- type was deprecated.
workflowTypeInfo_deprecationDate :: Lens.Lens' WorkflowTypeInfo (Prelude.Maybe Prelude.UTCTime)
workflowTypeInfo_deprecationDate = Lens.lens (\WorkflowTypeInfo' {deprecationDate} -> deprecationDate) (\s@WorkflowTypeInfo' {} a -> s {deprecationDate = a} :: WorkflowTypeInfo) Prelude.. Lens.mapping Data._Time

-- | The description of the type registered through RegisterWorkflowType.
workflowTypeInfo_description :: Lens.Lens' WorkflowTypeInfo (Prelude.Maybe Prelude.Text)
workflowTypeInfo_description = Lens.lens (\WorkflowTypeInfo' {description} -> description) (\s@WorkflowTypeInfo' {} a -> s {description = a} :: WorkflowTypeInfo)

-- | The workflow type this information is about.
workflowTypeInfo_workflowType :: Lens.Lens' WorkflowTypeInfo WorkflowType
workflowTypeInfo_workflowType = Lens.lens (\WorkflowTypeInfo' {workflowType} -> workflowType) (\s@WorkflowTypeInfo' {} a -> s {workflowType = a} :: WorkflowTypeInfo)

-- | The current status of the workflow type.
workflowTypeInfo_status :: Lens.Lens' WorkflowTypeInfo RegistrationStatus
workflowTypeInfo_status = Lens.lens (\WorkflowTypeInfo' {status} -> status) (\s@WorkflowTypeInfo' {} a -> s {status = a} :: WorkflowTypeInfo)

-- | The date when this type was registered.
workflowTypeInfo_creationDate :: Lens.Lens' WorkflowTypeInfo Prelude.UTCTime
workflowTypeInfo_creationDate = Lens.lens (\WorkflowTypeInfo' {creationDate} -> creationDate) (\s@WorkflowTypeInfo' {} a -> s {creationDate = a} :: WorkflowTypeInfo) Prelude.. Data._Time

instance Data.FromJSON WorkflowTypeInfo where
  parseJSON =
    Data.withObject
      "WorkflowTypeInfo"
      ( \x ->
          WorkflowTypeInfo'
            Prelude.<$> (x Data..:? "deprecationDate")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..: "workflowType")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "creationDate")
      )

instance Prelude.Hashable WorkflowTypeInfo where
  hashWithSalt _salt WorkflowTypeInfo' {..} =
    _salt
      `Prelude.hashWithSalt` deprecationDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` workflowType
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` creationDate

instance Prelude.NFData WorkflowTypeInfo where
  rnf WorkflowTypeInfo' {..} =
    Prelude.rnf deprecationDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf workflowType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf creationDate
