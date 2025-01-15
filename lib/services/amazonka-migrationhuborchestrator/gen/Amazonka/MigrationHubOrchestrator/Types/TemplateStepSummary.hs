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
-- Module      : Amazonka.MigrationHubOrchestrator.Types.TemplateStepSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubOrchestrator.Types.TemplateStepSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubOrchestrator.Types.Owner
import Amazonka.MigrationHubOrchestrator.Types.StepActionType
import Amazonka.MigrationHubOrchestrator.Types.TargetType
import qualified Amazonka.Prelude as Prelude

-- | The summary of the step.
--
-- /See:/ 'newTemplateStepSummary' smart constructor.
data TemplateStepSummary = TemplateStepSummary'
  { -- | The ID of the step.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the step.
    name :: Prelude.Maybe Prelude.Text,
    -- | The next step.
    next :: Prelude.Maybe [Prelude.Text],
    -- | The owner of the step.
    owner :: Prelude.Maybe Owner,
    -- | The previous step.
    previous :: Prelude.Maybe [Prelude.Text],
    -- | The action type of the step. You must run and update the status of a
    -- manual step for the workflow to continue after the completion of the
    -- step.
    stepActionType :: Prelude.Maybe StepActionType,
    -- | The ID of the step group.
    stepGroupId :: Prelude.Maybe Prelude.Text,
    -- | The servers on which to run the script.
    targetType :: Prelude.Maybe TargetType,
    -- | The ID of the template.
    templateId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TemplateStepSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'templateStepSummary_id' - The ID of the step.
--
-- 'name', 'templateStepSummary_name' - The name of the step.
--
-- 'next', 'templateStepSummary_next' - The next step.
--
-- 'owner', 'templateStepSummary_owner' - The owner of the step.
--
-- 'previous', 'templateStepSummary_previous' - The previous step.
--
-- 'stepActionType', 'templateStepSummary_stepActionType' - The action type of the step. You must run and update the status of a
-- manual step for the workflow to continue after the completion of the
-- step.
--
-- 'stepGroupId', 'templateStepSummary_stepGroupId' - The ID of the step group.
--
-- 'targetType', 'templateStepSummary_targetType' - The servers on which to run the script.
--
-- 'templateId', 'templateStepSummary_templateId' - The ID of the template.
newTemplateStepSummary ::
  TemplateStepSummary
newTemplateStepSummary =
  TemplateStepSummary'
    { id = Prelude.Nothing,
      name = Prelude.Nothing,
      next = Prelude.Nothing,
      owner = Prelude.Nothing,
      previous = Prelude.Nothing,
      stepActionType = Prelude.Nothing,
      stepGroupId = Prelude.Nothing,
      targetType = Prelude.Nothing,
      templateId = Prelude.Nothing
    }

-- | The ID of the step.
templateStepSummary_id :: Lens.Lens' TemplateStepSummary (Prelude.Maybe Prelude.Text)
templateStepSummary_id = Lens.lens (\TemplateStepSummary' {id} -> id) (\s@TemplateStepSummary' {} a -> s {id = a} :: TemplateStepSummary)

-- | The name of the step.
templateStepSummary_name :: Lens.Lens' TemplateStepSummary (Prelude.Maybe Prelude.Text)
templateStepSummary_name = Lens.lens (\TemplateStepSummary' {name} -> name) (\s@TemplateStepSummary' {} a -> s {name = a} :: TemplateStepSummary)

-- | The next step.
templateStepSummary_next :: Lens.Lens' TemplateStepSummary (Prelude.Maybe [Prelude.Text])
templateStepSummary_next = Lens.lens (\TemplateStepSummary' {next} -> next) (\s@TemplateStepSummary' {} a -> s {next = a} :: TemplateStepSummary) Prelude.. Lens.mapping Lens.coerced

-- | The owner of the step.
templateStepSummary_owner :: Lens.Lens' TemplateStepSummary (Prelude.Maybe Owner)
templateStepSummary_owner = Lens.lens (\TemplateStepSummary' {owner} -> owner) (\s@TemplateStepSummary' {} a -> s {owner = a} :: TemplateStepSummary)

-- | The previous step.
templateStepSummary_previous :: Lens.Lens' TemplateStepSummary (Prelude.Maybe [Prelude.Text])
templateStepSummary_previous = Lens.lens (\TemplateStepSummary' {previous} -> previous) (\s@TemplateStepSummary' {} a -> s {previous = a} :: TemplateStepSummary) Prelude.. Lens.mapping Lens.coerced

-- | The action type of the step. You must run and update the status of a
-- manual step for the workflow to continue after the completion of the
-- step.
templateStepSummary_stepActionType :: Lens.Lens' TemplateStepSummary (Prelude.Maybe StepActionType)
templateStepSummary_stepActionType = Lens.lens (\TemplateStepSummary' {stepActionType} -> stepActionType) (\s@TemplateStepSummary' {} a -> s {stepActionType = a} :: TemplateStepSummary)

-- | The ID of the step group.
templateStepSummary_stepGroupId :: Lens.Lens' TemplateStepSummary (Prelude.Maybe Prelude.Text)
templateStepSummary_stepGroupId = Lens.lens (\TemplateStepSummary' {stepGroupId} -> stepGroupId) (\s@TemplateStepSummary' {} a -> s {stepGroupId = a} :: TemplateStepSummary)

-- | The servers on which to run the script.
templateStepSummary_targetType :: Lens.Lens' TemplateStepSummary (Prelude.Maybe TargetType)
templateStepSummary_targetType = Lens.lens (\TemplateStepSummary' {targetType} -> targetType) (\s@TemplateStepSummary' {} a -> s {targetType = a} :: TemplateStepSummary)

-- | The ID of the template.
templateStepSummary_templateId :: Lens.Lens' TemplateStepSummary (Prelude.Maybe Prelude.Text)
templateStepSummary_templateId = Lens.lens (\TemplateStepSummary' {templateId} -> templateId) (\s@TemplateStepSummary' {} a -> s {templateId = a} :: TemplateStepSummary)

instance Data.FromJSON TemplateStepSummary where
  parseJSON =
    Data.withObject
      "TemplateStepSummary"
      ( \x ->
          TemplateStepSummary'
            Prelude.<$> (x Data..:? "id")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "next" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "owner")
            Prelude.<*> (x Data..:? "previous" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "stepActionType")
            Prelude.<*> (x Data..:? "stepGroupId")
            Prelude.<*> (x Data..:? "targetType")
            Prelude.<*> (x Data..:? "templateId")
      )

instance Prelude.Hashable TemplateStepSummary where
  hashWithSalt _salt TemplateStepSummary' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` next
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` previous
      `Prelude.hashWithSalt` stepActionType
      `Prelude.hashWithSalt` stepGroupId
      `Prelude.hashWithSalt` targetType
      `Prelude.hashWithSalt` templateId

instance Prelude.NFData TemplateStepSummary where
  rnf TemplateStepSummary' {..} =
    Prelude.rnf id `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf next `Prelude.seq`
          Prelude.rnf owner `Prelude.seq`
            Prelude.rnf previous `Prelude.seq`
              Prelude.rnf stepActionType `Prelude.seq`
                Prelude.rnf stepGroupId `Prelude.seq`
                  Prelude.rnf targetType `Prelude.seq`
                    Prelude.rnf templateId
