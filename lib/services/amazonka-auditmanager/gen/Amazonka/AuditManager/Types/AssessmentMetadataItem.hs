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
-- Module      : Amazonka.AuditManager.Types.AssessmentMetadataItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.AssessmentMetadataItem where

import Amazonka.AuditManager.Types.AssessmentStatus
import Amazonka.AuditManager.Types.Delegation
import Amazonka.AuditManager.Types.Role
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A metadata object that\'s associated with an assessment in Audit
-- Manager.
--
-- /See:/ 'newAssessmentMetadataItem' smart constructor.
data AssessmentMetadataItem = AssessmentMetadataItem'
  { -- | The name of the assessment.
    name :: Prelude.Maybe Prelude.Text,
    -- | The current status of the assessment.
    status :: Prelude.Maybe AssessmentStatus,
    -- | The unique identifier for the assessment.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time of the most recent update.
    lastUpdated :: Prelude.Maybe Core.POSIX,
    -- | Specifies when the assessment was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The delegations that are associated with the assessment.
    delegations :: Prelude.Maybe [Delegation],
    -- | The name of the compliance standard that\'s related to the assessment,
    -- such as PCI-DSS.
    complianceType :: Prelude.Maybe Prelude.Text,
    -- | The roles that are associated with the assessment.
    roles :: Prelude.Maybe [Role]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssessmentMetadataItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'assessmentMetadataItem_name' - The name of the assessment.
--
-- 'status', 'assessmentMetadataItem_status' - The current status of the assessment.
--
-- 'id', 'assessmentMetadataItem_id' - The unique identifier for the assessment.
--
-- 'lastUpdated', 'assessmentMetadataItem_lastUpdated' - The time of the most recent update.
--
-- 'creationTime', 'assessmentMetadataItem_creationTime' - Specifies when the assessment was created.
--
-- 'delegations', 'assessmentMetadataItem_delegations' - The delegations that are associated with the assessment.
--
-- 'complianceType', 'assessmentMetadataItem_complianceType' - The name of the compliance standard that\'s related to the assessment,
-- such as PCI-DSS.
--
-- 'roles', 'assessmentMetadataItem_roles' - The roles that are associated with the assessment.
newAssessmentMetadataItem ::
  AssessmentMetadataItem
newAssessmentMetadataItem =
  AssessmentMetadataItem'
    { name = Prelude.Nothing,
      status = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      delegations = Prelude.Nothing,
      complianceType = Prelude.Nothing,
      roles = Prelude.Nothing
    }

-- | The name of the assessment.
assessmentMetadataItem_name :: Lens.Lens' AssessmentMetadataItem (Prelude.Maybe Prelude.Text)
assessmentMetadataItem_name = Lens.lens (\AssessmentMetadataItem' {name} -> name) (\s@AssessmentMetadataItem' {} a -> s {name = a} :: AssessmentMetadataItem)

-- | The current status of the assessment.
assessmentMetadataItem_status :: Lens.Lens' AssessmentMetadataItem (Prelude.Maybe AssessmentStatus)
assessmentMetadataItem_status = Lens.lens (\AssessmentMetadataItem' {status} -> status) (\s@AssessmentMetadataItem' {} a -> s {status = a} :: AssessmentMetadataItem)

-- | The unique identifier for the assessment.
assessmentMetadataItem_id :: Lens.Lens' AssessmentMetadataItem (Prelude.Maybe Prelude.Text)
assessmentMetadataItem_id = Lens.lens (\AssessmentMetadataItem' {id} -> id) (\s@AssessmentMetadataItem' {} a -> s {id = a} :: AssessmentMetadataItem)

-- | The time of the most recent update.
assessmentMetadataItem_lastUpdated :: Lens.Lens' AssessmentMetadataItem (Prelude.Maybe Prelude.UTCTime)
assessmentMetadataItem_lastUpdated = Lens.lens (\AssessmentMetadataItem' {lastUpdated} -> lastUpdated) (\s@AssessmentMetadataItem' {} a -> s {lastUpdated = a} :: AssessmentMetadataItem) Prelude.. Lens.mapping Core._Time

-- | Specifies when the assessment was created.
assessmentMetadataItem_creationTime :: Lens.Lens' AssessmentMetadataItem (Prelude.Maybe Prelude.UTCTime)
assessmentMetadataItem_creationTime = Lens.lens (\AssessmentMetadataItem' {creationTime} -> creationTime) (\s@AssessmentMetadataItem' {} a -> s {creationTime = a} :: AssessmentMetadataItem) Prelude.. Lens.mapping Core._Time

-- | The delegations that are associated with the assessment.
assessmentMetadataItem_delegations :: Lens.Lens' AssessmentMetadataItem (Prelude.Maybe [Delegation])
assessmentMetadataItem_delegations = Lens.lens (\AssessmentMetadataItem' {delegations} -> delegations) (\s@AssessmentMetadataItem' {} a -> s {delegations = a} :: AssessmentMetadataItem) Prelude.. Lens.mapping Lens.coerced

-- | The name of the compliance standard that\'s related to the assessment,
-- such as PCI-DSS.
assessmentMetadataItem_complianceType :: Lens.Lens' AssessmentMetadataItem (Prelude.Maybe Prelude.Text)
assessmentMetadataItem_complianceType = Lens.lens (\AssessmentMetadataItem' {complianceType} -> complianceType) (\s@AssessmentMetadataItem' {} a -> s {complianceType = a} :: AssessmentMetadataItem)

-- | The roles that are associated with the assessment.
assessmentMetadataItem_roles :: Lens.Lens' AssessmentMetadataItem (Prelude.Maybe [Role])
assessmentMetadataItem_roles = Lens.lens (\AssessmentMetadataItem' {roles} -> roles) (\s@AssessmentMetadataItem' {} a -> s {roles = a} :: AssessmentMetadataItem) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON AssessmentMetadataItem where
  parseJSON =
    Core.withObject
      "AssessmentMetadataItem"
      ( \x ->
          AssessmentMetadataItem'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "lastUpdated")
            Prelude.<*> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "delegations" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "complianceType")
            Prelude.<*> (x Core..:? "roles" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable AssessmentMetadataItem where
  hashWithSalt _salt AssessmentMetadataItem' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastUpdated
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` delegations
      `Prelude.hashWithSalt` complianceType
      `Prelude.hashWithSalt` roles

instance Prelude.NFData AssessmentMetadataItem where
  rnf AssessmentMetadataItem' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastUpdated
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf delegations
      `Prelude.seq` Prelude.rnf complianceType
      `Prelude.seq` Prelude.rnf roles
