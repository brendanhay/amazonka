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
-- Module      : Network.AWS.IoT.Types.AuditMitigationActionsTaskTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditMitigationActionsTaskTarget where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Used in MitigationActionParams, this information identifies the target
-- findings to which the mitigation actions are applied. Only one entry
-- appears.
--
-- /See:/ 'newAuditMitigationActionsTaskTarget' smart constructor.
data AuditMitigationActionsTaskTarget = AuditMitigationActionsTaskTarget'
  { -- | If the task will apply a mitigation action to one or more listed
    -- findings, this value uniquely identifies those findings.
    findingIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | If the task will apply a mitigation action to findings from a specific
    -- audit, this value uniquely identifies the audit.
    auditTaskId :: Prelude.Maybe Prelude.Text,
    -- | Specifies a filter in the form of an audit check and set of reason codes
    -- that identify the findings from the audit to which the audit mitigation
    -- actions task apply.
    auditCheckToReasonCodeFilter :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text))
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuditMitigationActionsTaskTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'findingIds', 'auditMitigationActionsTaskTarget_findingIds' - If the task will apply a mitigation action to one or more listed
-- findings, this value uniquely identifies those findings.
--
-- 'auditTaskId', 'auditMitigationActionsTaskTarget_auditTaskId' - If the task will apply a mitigation action to findings from a specific
-- audit, this value uniquely identifies the audit.
--
-- 'auditCheckToReasonCodeFilter', 'auditMitigationActionsTaskTarget_auditCheckToReasonCodeFilter' - Specifies a filter in the form of an audit check and set of reason codes
-- that identify the findings from the audit to which the audit mitigation
-- actions task apply.
newAuditMitigationActionsTaskTarget ::
  AuditMitigationActionsTaskTarget
newAuditMitigationActionsTaskTarget =
  AuditMitigationActionsTaskTarget'
    { findingIds =
        Prelude.Nothing,
      auditTaskId = Prelude.Nothing,
      auditCheckToReasonCodeFilter =
        Prelude.Nothing
    }

-- | If the task will apply a mitigation action to one or more listed
-- findings, this value uniquely identifies those findings.
auditMitigationActionsTaskTarget_findingIds :: Lens.Lens' AuditMitigationActionsTaskTarget (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
auditMitigationActionsTaskTarget_findingIds = Lens.lens (\AuditMitigationActionsTaskTarget' {findingIds} -> findingIds) (\s@AuditMitigationActionsTaskTarget' {} a -> s {findingIds = a} :: AuditMitigationActionsTaskTarget) Prelude.. Lens.mapping Lens._Coerce

-- | If the task will apply a mitigation action to findings from a specific
-- audit, this value uniquely identifies the audit.
auditMitigationActionsTaskTarget_auditTaskId :: Lens.Lens' AuditMitigationActionsTaskTarget (Prelude.Maybe Prelude.Text)
auditMitigationActionsTaskTarget_auditTaskId = Lens.lens (\AuditMitigationActionsTaskTarget' {auditTaskId} -> auditTaskId) (\s@AuditMitigationActionsTaskTarget' {} a -> s {auditTaskId = a} :: AuditMitigationActionsTaskTarget)

-- | Specifies a filter in the form of an audit check and set of reason codes
-- that identify the findings from the audit to which the audit mitigation
-- actions task apply.
auditMitigationActionsTaskTarget_auditCheckToReasonCodeFilter :: Lens.Lens' AuditMitigationActionsTaskTarget (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)))
auditMitigationActionsTaskTarget_auditCheckToReasonCodeFilter = Lens.lens (\AuditMitigationActionsTaskTarget' {auditCheckToReasonCodeFilter} -> auditCheckToReasonCodeFilter) (\s@AuditMitigationActionsTaskTarget' {} a -> s {auditCheckToReasonCodeFilter = a} :: AuditMitigationActionsTaskTarget) Prelude.. Lens.mapping Lens._Coerce

instance
  Core.FromJSON
    AuditMitigationActionsTaskTarget
  where
  parseJSON =
    Core.withObject
      "AuditMitigationActionsTaskTarget"
      ( \x ->
          AuditMitigationActionsTaskTarget'
            Prelude.<$> (x Core..:? "findingIds")
            Prelude.<*> (x Core..:? "auditTaskId")
            Prelude.<*> ( x Core..:? "auditCheckToReasonCodeFilter"
                            Core..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    AuditMitigationActionsTaskTarget

instance
  Prelude.NFData
    AuditMitigationActionsTaskTarget

instance Core.ToJSON AuditMitigationActionsTaskTarget where
  toJSON AuditMitigationActionsTaskTarget' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("findingIds" Core..=) Prelude.<$> findingIds,
            ("auditTaskId" Core..=) Prelude.<$> auditTaskId,
            ("auditCheckToReasonCodeFilter" Core..=)
              Prelude.<$> auditCheckToReasonCodeFilter
          ]
      )
