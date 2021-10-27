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
-- Module      : Network.AWS.AuditManager.Types.AssessmentMetadataItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AuditManager.Types.AssessmentMetadataItem where

import Network.AWS.AuditManager.Types.AssessmentStatus
import Network.AWS.AuditManager.Types.Delegation
import Network.AWS.AuditManager.Types.Role
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A metadata object associated with an assessment in Audit Manager.
--
-- /See:/ 'newAssessmentMetadataItem' smart constructor.
data AssessmentMetadataItem = AssessmentMetadataItem'
  { -- | Specifies when the assessment was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The current status of the assessment.
    status :: Prelude.Maybe AssessmentStatus,
    -- | The time of the most recent update.
    lastUpdated :: Prelude.Maybe Core.POSIX,
    -- | The roles associated with the assessment.
    roles :: Prelude.Maybe [Role],
    -- | The delegations associated with the assessment.
    delegations :: Prelude.Maybe [Delegation],
    -- | The name of the assessment.
    name :: Prelude.Maybe Prelude.Text,
    -- | The name of the compliance standard related to the assessment, such as
    -- PCI-DSS.
    complianceType :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the assessment.
    id :: Prelude.Maybe Prelude.Text
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
-- 'creationTime', 'assessmentMetadataItem_creationTime' - Specifies when the assessment was created.
--
-- 'status', 'assessmentMetadataItem_status' - The current status of the assessment.
--
-- 'lastUpdated', 'assessmentMetadataItem_lastUpdated' - The time of the most recent update.
--
-- 'roles', 'assessmentMetadataItem_roles' - The roles associated with the assessment.
--
-- 'delegations', 'assessmentMetadataItem_delegations' - The delegations associated with the assessment.
--
-- 'name', 'assessmentMetadataItem_name' - The name of the assessment.
--
-- 'complianceType', 'assessmentMetadataItem_complianceType' - The name of the compliance standard related to the assessment, such as
-- PCI-DSS.
--
-- 'id', 'assessmentMetadataItem_id' - The unique identifier for the assessment.
newAssessmentMetadataItem ::
  AssessmentMetadataItem
newAssessmentMetadataItem =
  AssessmentMetadataItem'
    { creationTime =
        Prelude.Nothing,
      status = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      roles = Prelude.Nothing,
      delegations = Prelude.Nothing,
      name = Prelude.Nothing,
      complianceType = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | Specifies when the assessment was created.
assessmentMetadataItem_creationTime :: Lens.Lens' AssessmentMetadataItem (Prelude.Maybe Prelude.UTCTime)
assessmentMetadataItem_creationTime = Lens.lens (\AssessmentMetadataItem' {creationTime} -> creationTime) (\s@AssessmentMetadataItem' {} a -> s {creationTime = a} :: AssessmentMetadataItem) Prelude.. Lens.mapping Core._Time

-- | The current status of the assessment.
assessmentMetadataItem_status :: Lens.Lens' AssessmentMetadataItem (Prelude.Maybe AssessmentStatus)
assessmentMetadataItem_status = Lens.lens (\AssessmentMetadataItem' {status} -> status) (\s@AssessmentMetadataItem' {} a -> s {status = a} :: AssessmentMetadataItem)

-- | The time of the most recent update.
assessmentMetadataItem_lastUpdated :: Lens.Lens' AssessmentMetadataItem (Prelude.Maybe Prelude.UTCTime)
assessmentMetadataItem_lastUpdated = Lens.lens (\AssessmentMetadataItem' {lastUpdated} -> lastUpdated) (\s@AssessmentMetadataItem' {} a -> s {lastUpdated = a} :: AssessmentMetadataItem) Prelude.. Lens.mapping Core._Time

-- | The roles associated with the assessment.
assessmentMetadataItem_roles :: Lens.Lens' AssessmentMetadataItem (Prelude.Maybe [Role])
assessmentMetadataItem_roles = Lens.lens (\AssessmentMetadataItem' {roles} -> roles) (\s@AssessmentMetadataItem' {} a -> s {roles = a} :: AssessmentMetadataItem) Prelude.. Lens.mapping Lens.coerced

-- | The delegations associated with the assessment.
assessmentMetadataItem_delegations :: Lens.Lens' AssessmentMetadataItem (Prelude.Maybe [Delegation])
assessmentMetadataItem_delegations = Lens.lens (\AssessmentMetadataItem' {delegations} -> delegations) (\s@AssessmentMetadataItem' {} a -> s {delegations = a} :: AssessmentMetadataItem) Prelude.. Lens.mapping Lens.coerced

-- | The name of the assessment.
assessmentMetadataItem_name :: Lens.Lens' AssessmentMetadataItem (Prelude.Maybe Prelude.Text)
assessmentMetadataItem_name = Lens.lens (\AssessmentMetadataItem' {name} -> name) (\s@AssessmentMetadataItem' {} a -> s {name = a} :: AssessmentMetadataItem)

-- | The name of the compliance standard related to the assessment, such as
-- PCI-DSS.
assessmentMetadataItem_complianceType :: Lens.Lens' AssessmentMetadataItem (Prelude.Maybe Prelude.Text)
assessmentMetadataItem_complianceType = Lens.lens (\AssessmentMetadataItem' {complianceType} -> complianceType) (\s@AssessmentMetadataItem' {} a -> s {complianceType = a} :: AssessmentMetadataItem)

-- | The unique identifier for the assessment.
assessmentMetadataItem_id :: Lens.Lens' AssessmentMetadataItem (Prelude.Maybe Prelude.Text)
assessmentMetadataItem_id = Lens.lens (\AssessmentMetadataItem' {id} -> id) (\s@AssessmentMetadataItem' {} a -> s {id = a} :: AssessmentMetadataItem)

instance Core.FromJSON AssessmentMetadataItem where
  parseJSON =
    Core.withObject
      "AssessmentMetadataItem"
      ( \x ->
          AssessmentMetadataItem'
            Prelude.<$> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "lastUpdated")
            Prelude.<*> (x Core..:? "roles" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "delegations" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "complianceType")
            Prelude.<*> (x Core..:? "id")
      )

instance Prelude.Hashable AssessmentMetadataItem

instance Prelude.NFData AssessmentMetadataItem
