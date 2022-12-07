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
-- Module      : Amazonka.AuditManager.Types.AssessmentMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.AssessmentMetadata where

import Amazonka.AuditManager.Types.AssessmentReportsDestination
import Amazonka.AuditManager.Types.AssessmentStatus
import Amazonka.AuditManager.Types.Delegation
import Amazonka.AuditManager.Types.Role
import Amazonka.AuditManager.Types.Scope
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The metadata that\'s associated with the specified assessment.
--
-- /See:/ 'newAssessmentMetadata' smart constructor.
data AssessmentMetadata = AssessmentMetadata'
  { -- | The name of the assessment.
    name :: Prelude.Maybe Prelude.Text,
    -- | The overall status of the assessment.
    status :: Prelude.Maybe AssessmentStatus,
    -- | The description of the assessment.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the assessment.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time of the most recent update.
    lastUpdated :: Prelude.Maybe Data.POSIX,
    -- | The wrapper of Amazon Web Services accounts and services that are in
    -- scope for the assessment.
    scope :: Prelude.Maybe Scope,
    -- | Specifies when the assessment was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The delegations that are associated with the assessment.
    delegations :: Prelude.Maybe [Delegation],
    -- | The name of the compliance standard that\'s related to the assessment,
    -- such as PCI-DSS.
    complianceType :: Prelude.Maybe Prelude.Text,
    -- | The destination that evidence reports are stored in for the assessment.
    assessmentReportsDestination :: Prelude.Maybe AssessmentReportsDestination,
    -- | The roles that are associated with the assessment.
    roles :: Prelude.Maybe [Role]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssessmentMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'assessmentMetadata_name' - The name of the assessment.
--
-- 'status', 'assessmentMetadata_status' - The overall status of the assessment.
--
-- 'description', 'assessmentMetadata_description' - The description of the assessment.
--
-- 'id', 'assessmentMetadata_id' - The unique identifier for the assessment.
--
-- 'lastUpdated', 'assessmentMetadata_lastUpdated' - The time of the most recent update.
--
-- 'scope', 'assessmentMetadata_scope' - The wrapper of Amazon Web Services accounts and services that are in
-- scope for the assessment.
--
-- 'creationTime', 'assessmentMetadata_creationTime' - Specifies when the assessment was created.
--
-- 'delegations', 'assessmentMetadata_delegations' - The delegations that are associated with the assessment.
--
-- 'complianceType', 'assessmentMetadata_complianceType' - The name of the compliance standard that\'s related to the assessment,
-- such as PCI-DSS.
--
-- 'assessmentReportsDestination', 'assessmentMetadata_assessmentReportsDestination' - The destination that evidence reports are stored in for the assessment.
--
-- 'roles', 'assessmentMetadata_roles' - The roles that are associated with the assessment.
newAssessmentMetadata ::
  AssessmentMetadata
newAssessmentMetadata =
  AssessmentMetadata'
    { name = Prelude.Nothing,
      status = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      scope = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      delegations = Prelude.Nothing,
      complianceType = Prelude.Nothing,
      assessmentReportsDestination = Prelude.Nothing,
      roles = Prelude.Nothing
    }

-- | The name of the assessment.
assessmentMetadata_name :: Lens.Lens' AssessmentMetadata (Prelude.Maybe Prelude.Text)
assessmentMetadata_name = Lens.lens (\AssessmentMetadata' {name} -> name) (\s@AssessmentMetadata' {} a -> s {name = a} :: AssessmentMetadata)

-- | The overall status of the assessment.
assessmentMetadata_status :: Lens.Lens' AssessmentMetadata (Prelude.Maybe AssessmentStatus)
assessmentMetadata_status = Lens.lens (\AssessmentMetadata' {status} -> status) (\s@AssessmentMetadata' {} a -> s {status = a} :: AssessmentMetadata)

-- | The description of the assessment.
assessmentMetadata_description :: Lens.Lens' AssessmentMetadata (Prelude.Maybe Prelude.Text)
assessmentMetadata_description = Lens.lens (\AssessmentMetadata' {description} -> description) (\s@AssessmentMetadata' {} a -> s {description = a} :: AssessmentMetadata)

-- | The unique identifier for the assessment.
assessmentMetadata_id :: Lens.Lens' AssessmentMetadata (Prelude.Maybe Prelude.Text)
assessmentMetadata_id = Lens.lens (\AssessmentMetadata' {id} -> id) (\s@AssessmentMetadata' {} a -> s {id = a} :: AssessmentMetadata)

-- | The time of the most recent update.
assessmentMetadata_lastUpdated :: Lens.Lens' AssessmentMetadata (Prelude.Maybe Prelude.UTCTime)
assessmentMetadata_lastUpdated = Lens.lens (\AssessmentMetadata' {lastUpdated} -> lastUpdated) (\s@AssessmentMetadata' {} a -> s {lastUpdated = a} :: AssessmentMetadata) Prelude.. Lens.mapping Data._Time

-- | The wrapper of Amazon Web Services accounts and services that are in
-- scope for the assessment.
assessmentMetadata_scope :: Lens.Lens' AssessmentMetadata (Prelude.Maybe Scope)
assessmentMetadata_scope = Lens.lens (\AssessmentMetadata' {scope} -> scope) (\s@AssessmentMetadata' {} a -> s {scope = a} :: AssessmentMetadata)

-- | Specifies when the assessment was created.
assessmentMetadata_creationTime :: Lens.Lens' AssessmentMetadata (Prelude.Maybe Prelude.UTCTime)
assessmentMetadata_creationTime = Lens.lens (\AssessmentMetadata' {creationTime} -> creationTime) (\s@AssessmentMetadata' {} a -> s {creationTime = a} :: AssessmentMetadata) Prelude.. Lens.mapping Data._Time

-- | The delegations that are associated with the assessment.
assessmentMetadata_delegations :: Lens.Lens' AssessmentMetadata (Prelude.Maybe [Delegation])
assessmentMetadata_delegations = Lens.lens (\AssessmentMetadata' {delegations} -> delegations) (\s@AssessmentMetadata' {} a -> s {delegations = a} :: AssessmentMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The name of the compliance standard that\'s related to the assessment,
-- such as PCI-DSS.
assessmentMetadata_complianceType :: Lens.Lens' AssessmentMetadata (Prelude.Maybe Prelude.Text)
assessmentMetadata_complianceType = Lens.lens (\AssessmentMetadata' {complianceType} -> complianceType) (\s@AssessmentMetadata' {} a -> s {complianceType = a} :: AssessmentMetadata)

-- | The destination that evidence reports are stored in for the assessment.
assessmentMetadata_assessmentReportsDestination :: Lens.Lens' AssessmentMetadata (Prelude.Maybe AssessmentReportsDestination)
assessmentMetadata_assessmentReportsDestination = Lens.lens (\AssessmentMetadata' {assessmentReportsDestination} -> assessmentReportsDestination) (\s@AssessmentMetadata' {} a -> s {assessmentReportsDestination = a} :: AssessmentMetadata)

-- | The roles that are associated with the assessment.
assessmentMetadata_roles :: Lens.Lens' AssessmentMetadata (Prelude.Maybe [Role])
assessmentMetadata_roles = Lens.lens (\AssessmentMetadata' {roles} -> roles) (\s@AssessmentMetadata' {} a -> s {roles = a} :: AssessmentMetadata) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AssessmentMetadata where
  parseJSON =
    Data.withObject
      "AssessmentMetadata"
      ( \x ->
          AssessmentMetadata'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "lastUpdated")
            Prelude.<*> (x Data..:? "scope")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "delegations" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "complianceType")
            Prelude.<*> (x Data..:? "assessmentReportsDestination")
            Prelude.<*> (x Data..:? "roles" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AssessmentMetadata where
  hashWithSalt _salt AssessmentMetadata' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastUpdated
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` delegations
      `Prelude.hashWithSalt` complianceType
      `Prelude.hashWithSalt` assessmentReportsDestination
      `Prelude.hashWithSalt` roles

instance Prelude.NFData AssessmentMetadata where
  rnf AssessmentMetadata' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastUpdated
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf delegations
      `Prelude.seq` Prelude.rnf complianceType
      `Prelude.seq` Prelude.rnf assessmentReportsDestination
      `Prelude.seq` Prelude.rnf roles
