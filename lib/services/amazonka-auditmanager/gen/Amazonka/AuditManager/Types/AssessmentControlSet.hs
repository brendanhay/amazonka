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
-- Module      : Amazonka.AuditManager.Types.AssessmentControlSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.AssessmentControlSet where

import Amazonka.AuditManager.Types.AssessmentControl
import Amazonka.AuditManager.Types.ControlSetStatus
import Amazonka.AuditManager.Types.Delegation
import Amazonka.AuditManager.Types.Role
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a set of controls in an Audit Manager assessment.
--
-- /See:/ 'newAssessmentControlSet' smart constructor.
data AssessmentControlSet = AssessmentControlSet'
  { -- | Specifies the current status of the control set.
    status :: Prelude.Maybe ControlSetStatus,
    -- | The list of controls contained with the control set.
    controls :: Prelude.Maybe [AssessmentControl],
    -- | The roles associated with the control set.
    roles :: Prelude.Maybe [Role],
    -- | The total number of evidence objects uploaded manually to the control
    -- set.
    manualEvidenceCount :: Prelude.Maybe Prelude.Int,
    -- | The delegations associated with the control set.
    delegations :: Prelude.Maybe [Delegation],
    -- | The total number of evidence objects retrieved automatically for the
    -- control set.
    systemEvidenceCount :: Prelude.Maybe Prelude.Int,
    -- | The identifier of the control set in the assessment. This is the control
    -- set name in a plain string format.
    id :: Prelude.Maybe Prelude.Text,
    -- | The description for the control set.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssessmentControlSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'assessmentControlSet_status' - Specifies the current status of the control set.
--
-- 'controls', 'assessmentControlSet_controls' - The list of controls contained with the control set.
--
-- 'roles', 'assessmentControlSet_roles' - The roles associated with the control set.
--
-- 'manualEvidenceCount', 'assessmentControlSet_manualEvidenceCount' - The total number of evidence objects uploaded manually to the control
-- set.
--
-- 'delegations', 'assessmentControlSet_delegations' - The delegations associated with the control set.
--
-- 'systemEvidenceCount', 'assessmentControlSet_systemEvidenceCount' - The total number of evidence objects retrieved automatically for the
-- control set.
--
-- 'id', 'assessmentControlSet_id' - The identifier of the control set in the assessment. This is the control
-- set name in a plain string format.
--
-- 'description', 'assessmentControlSet_description' - The description for the control set.
newAssessmentControlSet ::
  AssessmentControlSet
newAssessmentControlSet =
  AssessmentControlSet'
    { status = Prelude.Nothing,
      controls = Prelude.Nothing,
      roles = Prelude.Nothing,
      manualEvidenceCount = Prelude.Nothing,
      delegations = Prelude.Nothing,
      systemEvidenceCount = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | Specifies the current status of the control set.
assessmentControlSet_status :: Lens.Lens' AssessmentControlSet (Prelude.Maybe ControlSetStatus)
assessmentControlSet_status = Lens.lens (\AssessmentControlSet' {status} -> status) (\s@AssessmentControlSet' {} a -> s {status = a} :: AssessmentControlSet)

-- | The list of controls contained with the control set.
assessmentControlSet_controls :: Lens.Lens' AssessmentControlSet (Prelude.Maybe [AssessmentControl])
assessmentControlSet_controls = Lens.lens (\AssessmentControlSet' {controls} -> controls) (\s@AssessmentControlSet' {} a -> s {controls = a} :: AssessmentControlSet) Prelude.. Lens.mapping Lens.coerced

-- | The roles associated with the control set.
assessmentControlSet_roles :: Lens.Lens' AssessmentControlSet (Prelude.Maybe [Role])
assessmentControlSet_roles = Lens.lens (\AssessmentControlSet' {roles} -> roles) (\s@AssessmentControlSet' {} a -> s {roles = a} :: AssessmentControlSet) Prelude.. Lens.mapping Lens.coerced

-- | The total number of evidence objects uploaded manually to the control
-- set.
assessmentControlSet_manualEvidenceCount :: Lens.Lens' AssessmentControlSet (Prelude.Maybe Prelude.Int)
assessmentControlSet_manualEvidenceCount = Lens.lens (\AssessmentControlSet' {manualEvidenceCount} -> manualEvidenceCount) (\s@AssessmentControlSet' {} a -> s {manualEvidenceCount = a} :: AssessmentControlSet)

-- | The delegations associated with the control set.
assessmentControlSet_delegations :: Lens.Lens' AssessmentControlSet (Prelude.Maybe [Delegation])
assessmentControlSet_delegations = Lens.lens (\AssessmentControlSet' {delegations} -> delegations) (\s@AssessmentControlSet' {} a -> s {delegations = a} :: AssessmentControlSet) Prelude.. Lens.mapping Lens.coerced

-- | The total number of evidence objects retrieved automatically for the
-- control set.
assessmentControlSet_systemEvidenceCount :: Lens.Lens' AssessmentControlSet (Prelude.Maybe Prelude.Int)
assessmentControlSet_systemEvidenceCount = Lens.lens (\AssessmentControlSet' {systemEvidenceCount} -> systemEvidenceCount) (\s@AssessmentControlSet' {} a -> s {systemEvidenceCount = a} :: AssessmentControlSet)

-- | The identifier of the control set in the assessment. This is the control
-- set name in a plain string format.
assessmentControlSet_id :: Lens.Lens' AssessmentControlSet (Prelude.Maybe Prelude.Text)
assessmentControlSet_id = Lens.lens (\AssessmentControlSet' {id} -> id) (\s@AssessmentControlSet' {} a -> s {id = a} :: AssessmentControlSet)

-- | The description for the control set.
assessmentControlSet_description :: Lens.Lens' AssessmentControlSet (Prelude.Maybe Prelude.Text)
assessmentControlSet_description = Lens.lens (\AssessmentControlSet' {description} -> description) (\s@AssessmentControlSet' {} a -> s {description = a} :: AssessmentControlSet)

instance Core.FromJSON AssessmentControlSet where
  parseJSON =
    Core.withObject
      "AssessmentControlSet"
      ( \x ->
          AssessmentControlSet'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "controls" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "roles" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "manualEvidenceCount")
            Prelude.<*> (x Core..:? "delegations" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "systemEvidenceCount")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "description")
      )

instance Prelude.Hashable AssessmentControlSet

instance Prelude.NFData AssessmentControlSet
