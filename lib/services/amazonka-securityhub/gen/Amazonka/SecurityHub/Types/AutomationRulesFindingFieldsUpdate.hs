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
-- Module      : Amazonka.SecurityHub.Types.AutomationRulesFindingFieldsUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AutomationRulesFindingFieldsUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.NoteUpdate
import Amazonka.SecurityHub.Types.RelatedFinding
import Amazonka.SecurityHub.Types.SeverityUpdate
import Amazonka.SecurityHub.Types.VerificationState
import Amazonka.SecurityHub.Types.WorkflowUpdate

-- | Identifies the finding fields that the automation rule action will
-- update when a finding matches the defined criteria.
--
-- /See:/ 'newAutomationRulesFindingFieldsUpdate' smart constructor.
data AutomationRulesFindingFieldsUpdate = AutomationRulesFindingFieldsUpdate'
  { -- | The rule action will update the @Confidence@ field of a finding.
    confidence :: Prelude.Maybe Prelude.Natural,
    -- | The rule action will update the @Criticality@ field of a finding.
    criticality :: Prelude.Maybe Prelude.Natural,
    note :: Prelude.Maybe NoteUpdate,
    -- | A list of findings that are related to a finding.
    relatedFindings :: Prelude.Maybe [RelatedFinding],
    severity :: Prelude.Maybe SeverityUpdate,
    -- | The rule action will update the @Types@ field of a finding.
    types :: Prelude.Maybe [Prelude.Text],
    -- | The rule action will update the @UserDefinedFields@ field of a finding.
    userDefinedFields :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The rule action will update the @VerificationState@ field of a finding.
    verificationState :: Prelude.Maybe VerificationState,
    workflow :: Prelude.Maybe WorkflowUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutomationRulesFindingFieldsUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confidence', 'automationRulesFindingFieldsUpdate_confidence' - The rule action will update the @Confidence@ field of a finding.
--
-- 'criticality', 'automationRulesFindingFieldsUpdate_criticality' - The rule action will update the @Criticality@ field of a finding.
--
-- 'note', 'automationRulesFindingFieldsUpdate_note' - Undocumented member.
--
-- 'relatedFindings', 'automationRulesFindingFieldsUpdate_relatedFindings' - A list of findings that are related to a finding.
--
-- 'severity', 'automationRulesFindingFieldsUpdate_severity' - Undocumented member.
--
-- 'types', 'automationRulesFindingFieldsUpdate_types' - The rule action will update the @Types@ field of a finding.
--
-- 'userDefinedFields', 'automationRulesFindingFieldsUpdate_userDefinedFields' - The rule action will update the @UserDefinedFields@ field of a finding.
--
-- 'verificationState', 'automationRulesFindingFieldsUpdate_verificationState' - The rule action will update the @VerificationState@ field of a finding.
--
-- 'workflow', 'automationRulesFindingFieldsUpdate_workflow' - Undocumented member.
newAutomationRulesFindingFieldsUpdate ::
  AutomationRulesFindingFieldsUpdate
newAutomationRulesFindingFieldsUpdate =
  AutomationRulesFindingFieldsUpdate'
    { confidence =
        Prelude.Nothing,
      criticality = Prelude.Nothing,
      note = Prelude.Nothing,
      relatedFindings = Prelude.Nothing,
      severity = Prelude.Nothing,
      types = Prelude.Nothing,
      userDefinedFields = Prelude.Nothing,
      verificationState = Prelude.Nothing,
      workflow = Prelude.Nothing
    }

-- | The rule action will update the @Confidence@ field of a finding.
automationRulesFindingFieldsUpdate_confidence :: Lens.Lens' AutomationRulesFindingFieldsUpdate (Prelude.Maybe Prelude.Natural)
automationRulesFindingFieldsUpdate_confidence = Lens.lens (\AutomationRulesFindingFieldsUpdate' {confidence} -> confidence) (\s@AutomationRulesFindingFieldsUpdate' {} a -> s {confidence = a} :: AutomationRulesFindingFieldsUpdate)

-- | The rule action will update the @Criticality@ field of a finding.
automationRulesFindingFieldsUpdate_criticality :: Lens.Lens' AutomationRulesFindingFieldsUpdate (Prelude.Maybe Prelude.Natural)
automationRulesFindingFieldsUpdate_criticality = Lens.lens (\AutomationRulesFindingFieldsUpdate' {criticality} -> criticality) (\s@AutomationRulesFindingFieldsUpdate' {} a -> s {criticality = a} :: AutomationRulesFindingFieldsUpdate)

-- | Undocumented member.
automationRulesFindingFieldsUpdate_note :: Lens.Lens' AutomationRulesFindingFieldsUpdate (Prelude.Maybe NoteUpdate)
automationRulesFindingFieldsUpdate_note = Lens.lens (\AutomationRulesFindingFieldsUpdate' {note} -> note) (\s@AutomationRulesFindingFieldsUpdate' {} a -> s {note = a} :: AutomationRulesFindingFieldsUpdate)

-- | A list of findings that are related to a finding.
automationRulesFindingFieldsUpdate_relatedFindings :: Lens.Lens' AutomationRulesFindingFieldsUpdate (Prelude.Maybe [RelatedFinding])
automationRulesFindingFieldsUpdate_relatedFindings = Lens.lens (\AutomationRulesFindingFieldsUpdate' {relatedFindings} -> relatedFindings) (\s@AutomationRulesFindingFieldsUpdate' {} a -> s {relatedFindings = a} :: AutomationRulesFindingFieldsUpdate) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
automationRulesFindingFieldsUpdate_severity :: Lens.Lens' AutomationRulesFindingFieldsUpdate (Prelude.Maybe SeverityUpdate)
automationRulesFindingFieldsUpdate_severity = Lens.lens (\AutomationRulesFindingFieldsUpdate' {severity} -> severity) (\s@AutomationRulesFindingFieldsUpdate' {} a -> s {severity = a} :: AutomationRulesFindingFieldsUpdate)

-- | The rule action will update the @Types@ field of a finding.
automationRulesFindingFieldsUpdate_types :: Lens.Lens' AutomationRulesFindingFieldsUpdate (Prelude.Maybe [Prelude.Text])
automationRulesFindingFieldsUpdate_types = Lens.lens (\AutomationRulesFindingFieldsUpdate' {types} -> types) (\s@AutomationRulesFindingFieldsUpdate' {} a -> s {types = a} :: AutomationRulesFindingFieldsUpdate) Prelude.. Lens.mapping Lens.coerced

-- | The rule action will update the @UserDefinedFields@ field of a finding.
automationRulesFindingFieldsUpdate_userDefinedFields :: Lens.Lens' AutomationRulesFindingFieldsUpdate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
automationRulesFindingFieldsUpdate_userDefinedFields = Lens.lens (\AutomationRulesFindingFieldsUpdate' {userDefinedFields} -> userDefinedFields) (\s@AutomationRulesFindingFieldsUpdate' {} a -> s {userDefinedFields = a} :: AutomationRulesFindingFieldsUpdate) Prelude.. Lens.mapping Lens.coerced

-- | The rule action will update the @VerificationState@ field of a finding.
automationRulesFindingFieldsUpdate_verificationState :: Lens.Lens' AutomationRulesFindingFieldsUpdate (Prelude.Maybe VerificationState)
automationRulesFindingFieldsUpdate_verificationState = Lens.lens (\AutomationRulesFindingFieldsUpdate' {verificationState} -> verificationState) (\s@AutomationRulesFindingFieldsUpdate' {} a -> s {verificationState = a} :: AutomationRulesFindingFieldsUpdate)

-- | Undocumented member.
automationRulesFindingFieldsUpdate_workflow :: Lens.Lens' AutomationRulesFindingFieldsUpdate (Prelude.Maybe WorkflowUpdate)
automationRulesFindingFieldsUpdate_workflow = Lens.lens (\AutomationRulesFindingFieldsUpdate' {workflow} -> workflow) (\s@AutomationRulesFindingFieldsUpdate' {} a -> s {workflow = a} :: AutomationRulesFindingFieldsUpdate)

instance
  Data.FromJSON
    AutomationRulesFindingFieldsUpdate
  where
  parseJSON =
    Data.withObject
      "AutomationRulesFindingFieldsUpdate"
      ( \x ->
          AutomationRulesFindingFieldsUpdate'
            Prelude.<$> (x Data..:? "Confidence")
            Prelude.<*> (x Data..:? "Criticality")
            Prelude.<*> (x Data..:? "Note")
            Prelude.<*> ( x
                            Data..:? "RelatedFindings"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Severity")
            Prelude.<*> (x Data..:? "Types" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "UserDefinedFields"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "VerificationState")
            Prelude.<*> (x Data..:? "Workflow")
      )

instance
  Prelude.Hashable
    AutomationRulesFindingFieldsUpdate
  where
  hashWithSalt
    _salt
    AutomationRulesFindingFieldsUpdate' {..} =
      _salt
        `Prelude.hashWithSalt` confidence
        `Prelude.hashWithSalt` criticality
        `Prelude.hashWithSalt` note
        `Prelude.hashWithSalt` relatedFindings
        `Prelude.hashWithSalt` severity
        `Prelude.hashWithSalt` types
        `Prelude.hashWithSalt` userDefinedFields
        `Prelude.hashWithSalt` verificationState
        `Prelude.hashWithSalt` workflow

instance
  Prelude.NFData
    AutomationRulesFindingFieldsUpdate
  where
  rnf AutomationRulesFindingFieldsUpdate' {..} =
    Prelude.rnf confidence
      `Prelude.seq` Prelude.rnf criticality
      `Prelude.seq` Prelude.rnf note
      `Prelude.seq` Prelude.rnf relatedFindings
      `Prelude.seq` Prelude.rnf severity
      `Prelude.seq` Prelude.rnf types
      `Prelude.seq` Prelude.rnf userDefinedFields
      `Prelude.seq` Prelude.rnf verificationState
      `Prelude.seq` Prelude.rnf workflow

instance
  Data.ToJSON
    AutomationRulesFindingFieldsUpdate
  where
  toJSON AutomationRulesFindingFieldsUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Confidence" Data..=) Prelude.<$> confidence,
            ("Criticality" Data..=) Prelude.<$> criticality,
            ("Note" Data..=) Prelude.<$> note,
            ("RelatedFindings" Data..=)
              Prelude.<$> relatedFindings,
            ("Severity" Data..=) Prelude.<$> severity,
            ("Types" Data..=) Prelude.<$> types,
            ("UserDefinedFields" Data..=)
              Prelude.<$> userDefinedFields,
            ("VerificationState" Data..=)
              Prelude.<$> verificationState,
            ("Workflow" Data..=) Prelude.<$> workflow
          ]
      )
