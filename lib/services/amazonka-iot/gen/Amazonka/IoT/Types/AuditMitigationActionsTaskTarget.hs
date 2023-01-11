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
-- Module      : Amazonka.IoT.Types.AuditMitigationActionsTaskTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AuditMitigationActionsTaskTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Used in MitigationActionParams, this information identifies the target
-- findings to which the mitigation actions are applied. Only one entry
-- appears.
--
-- /See:/ 'newAuditMitigationActionsTaskTarget' smart constructor.
data AuditMitigationActionsTaskTarget = AuditMitigationActionsTaskTarget'
  { -- | Specifies a filter in the form of an audit check and set of reason codes
    -- that identify the findings from the audit to which the audit mitigation
    -- actions task apply.
    auditCheckToReasonCodeFilter :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)),
    -- | If the task will apply a mitigation action to findings from a specific
    -- audit, this value uniquely identifies the audit.
    auditTaskId :: Prelude.Maybe Prelude.Text,
    -- | If the task will apply a mitigation action to one or more listed
    -- findings, this value uniquely identifies those findings.
    findingIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
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
-- 'auditCheckToReasonCodeFilter', 'auditMitigationActionsTaskTarget_auditCheckToReasonCodeFilter' - Specifies a filter in the form of an audit check and set of reason codes
-- that identify the findings from the audit to which the audit mitigation
-- actions task apply.
--
-- 'auditTaskId', 'auditMitigationActionsTaskTarget_auditTaskId' - If the task will apply a mitigation action to findings from a specific
-- audit, this value uniquely identifies the audit.
--
-- 'findingIds', 'auditMitigationActionsTaskTarget_findingIds' - If the task will apply a mitigation action to one or more listed
-- findings, this value uniquely identifies those findings.
newAuditMitigationActionsTaskTarget ::
  AuditMitigationActionsTaskTarget
newAuditMitigationActionsTaskTarget =
  AuditMitigationActionsTaskTarget'
    { auditCheckToReasonCodeFilter =
        Prelude.Nothing,
      auditTaskId = Prelude.Nothing,
      findingIds = Prelude.Nothing
    }

-- | Specifies a filter in the form of an audit check and set of reason codes
-- that identify the findings from the audit to which the audit mitigation
-- actions task apply.
auditMitigationActionsTaskTarget_auditCheckToReasonCodeFilter :: Lens.Lens' AuditMitigationActionsTaskTarget (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)))
auditMitigationActionsTaskTarget_auditCheckToReasonCodeFilter = Lens.lens (\AuditMitigationActionsTaskTarget' {auditCheckToReasonCodeFilter} -> auditCheckToReasonCodeFilter) (\s@AuditMitigationActionsTaskTarget' {} a -> s {auditCheckToReasonCodeFilter = a} :: AuditMitigationActionsTaskTarget) Prelude.. Lens.mapping Lens.coerced

-- | If the task will apply a mitigation action to findings from a specific
-- audit, this value uniquely identifies the audit.
auditMitigationActionsTaskTarget_auditTaskId :: Lens.Lens' AuditMitigationActionsTaskTarget (Prelude.Maybe Prelude.Text)
auditMitigationActionsTaskTarget_auditTaskId = Lens.lens (\AuditMitigationActionsTaskTarget' {auditTaskId} -> auditTaskId) (\s@AuditMitigationActionsTaskTarget' {} a -> s {auditTaskId = a} :: AuditMitigationActionsTaskTarget)

-- | If the task will apply a mitigation action to one or more listed
-- findings, this value uniquely identifies those findings.
auditMitigationActionsTaskTarget_findingIds :: Lens.Lens' AuditMitigationActionsTaskTarget (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
auditMitigationActionsTaskTarget_findingIds = Lens.lens (\AuditMitigationActionsTaskTarget' {findingIds} -> findingIds) (\s@AuditMitigationActionsTaskTarget' {} a -> s {findingIds = a} :: AuditMitigationActionsTaskTarget) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    AuditMitigationActionsTaskTarget
  where
  parseJSON =
    Data.withObject
      "AuditMitigationActionsTaskTarget"
      ( \x ->
          AuditMitigationActionsTaskTarget'
            Prelude.<$> ( x Data..:? "auditCheckToReasonCodeFilter"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "auditTaskId")
            Prelude.<*> (x Data..:? "findingIds")
      )

instance
  Prelude.Hashable
    AuditMitigationActionsTaskTarget
  where
  hashWithSalt
    _salt
    AuditMitigationActionsTaskTarget' {..} =
      _salt
        `Prelude.hashWithSalt` auditCheckToReasonCodeFilter
        `Prelude.hashWithSalt` auditTaskId
        `Prelude.hashWithSalt` findingIds

instance
  Prelude.NFData
    AuditMitigationActionsTaskTarget
  where
  rnf AuditMitigationActionsTaskTarget' {..} =
    Prelude.rnf auditCheckToReasonCodeFilter
      `Prelude.seq` Prelude.rnf auditTaskId
      `Prelude.seq` Prelude.rnf findingIds

instance Data.ToJSON AuditMitigationActionsTaskTarget where
  toJSON AuditMitigationActionsTaskTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("auditCheckToReasonCodeFilter" Data..=)
              Prelude.<$> auditCheckToReasonCodeFilter,
            ("auditTaskId" Data..=) Prelude.<$> auditTaskId,
            ("findingIds" Data..=) Prelude.<$> findingIds
          ]
      )
