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
-- Module      : Amazonka.FIS.Types.ExperimentTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.ExperimentTemplate where

import qualified Amazonka.Core as Core
import Amazonka.FIS.Types.ExperimentTemplateAction
import Amazonka.FIS.Types.ExperimentTemplateStopCondition
import Amazonka.FIS.Types.ExperimentTemplateTarget
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an experiment template.
--
-- /See:/ 'newExperimentTemplate' smart constructor.
data ExperimentTemplate = ExperimentTemplate'
  { -- | The time the experiment template was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The actions for the experiment.
    actions :: Prelude.Maybe (Prelude.HashMap Prelude.Text ExperimentTemplateAction),
    -- | The stop conditions for the experiment.
    stopConditions :: Prelude.Maybe [ExperimentTemplateStopCondition],
    -- | The targets for the experiment.
    targets :: Prelude.Maybe (Prelude.HashMap Prelude.Text ExperimentTemplateTarget),
    -- | The ID of the experiment template.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time the experiment template was last updated.
    lastUpdateTime :: Prelude.Maybe Core.POSIX,
    -- | The description for the experiment template.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags for the experiment template.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) of an IAM role.
    roleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperimentTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'experimentTemplate_creationTime' - The time the experiment template was created.
--
-- 'actions', 'experimentTemplate_actions' - The actions for the experiment.
--
-- 'stopConditions', 'experimentTemplate_stopConditions' - The stop conditions for the experiment.
--
-- 'targets', 'experimentTemplate_targets' - The targets for the experiment.
--
-- 'id', 'experimentTemplate_id' - The ID of the experiment template.
--
-- 'lastUpdateTime', 'experimentTemplate_lastUpdateTime' - The time the experiment template was last updated.
--
-- 'description', 'experimentTemplate_description' - The description for the experiment template.
--
-- 'tags', 'experimentTemplate_tags' - The tags for the experiment template.
--
-- 'roleArn', 'experimentTemplate_roleArn' - The Amazon Resource Name (ARN) of an IAM role.
newExperimentTemplate ::
  ExperimentTemplate
newExperimentTemplate =
  ExperimentTemplate'
    { creationTime = Prelude.Nothing,
      actions = Prelude.Nothing,
      stopConditions = Prelude.Nothing,
      targets = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | The time the experiment template was created.
experimentTemplate_creationTime :: Lens.Lens' ExperimentTemplate (Prelude.Maybe Prelude.UTCTime)
experimentTemplate_creationTime = Lens.lens (\ExperimentTemplate' {creationTime} -> creationTime) (\s@ExperimentTemplate' {} a -> s {creationTime = a} :: ExperimentTemplate) Prelude.. Lens.mapping Core._Time

-- | The actions for the experiment.
experimentTemplate_actions :: Lens.Lens' ExperimentTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text ExperimentTemplateAction))
experimentTemplate_actions = Lens.lens (\ExperimentTemplate' {actions} -> actions) (\s@ExperimentTemplate' {} a -> s {actions = a} :: ExperimentTemplate) Prelude.. Lens.mapping Lens.coerced

-- | The stop conditions for the experiment.
experimentTemplate_stopConditions :: Lens.Lens' ExperimentTemplate (Prelude.Maybe [ExperimentTemplateStopCondition])
experimentTemplate_stopConditions = Lens.lens (\ExperimentTemplate' {stopConditions} -> stopConditions) (\s@ExperimentTemplate' {} a -> s {stopConditions = a} :: ExperimentTemplate) Prelude.. Lens.mapping Lens.coerced

-- | The targets for the experiment.
experimentTemplate_targets :: Lens.Lens' ExperimentTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text ExperimentTemplateTarget))
experimentTemplate_targets = Lens.lens (\ExperimentTemplate' {targets} -> targets) (\s@ExperimentTemplate' {} a -> s {targets = a} :: ExperimentTemplate) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the experiment template.
experimentTemplate_id :: Lens.Lens' ExperimentTemplate (Prelude.Maybe Prelude.Text)
experimentTemplate_id = Lens.lens (\ExperimentTemplate' {id} -> id) (\s@ExperimentTemplate' {} a -> s {id = a} :: ExperimentTemplate)

-- | The time the experiment template was last updated.
experimentTemplate_lastUpdateTime :: Lens.Lens' ExperimentTemplate (Prelude.Maybe Prelude.UTCTime)
experimentTemplate_lastUpdateTime = Lens.lens (\ExperimentTemplate' {lastUpdateTime} -> lastUpdateTime) (\s@ExperimentTemplate' {} a -> s {lastUpdateTime = a} :: ExperimentTemplate) Prelude.. Lens.mapping Core._Time

-- | The description for the experiment template.
experimentTemplate_description :: Lens.Lens' ExperimentTemplate (Prelude.Maybe Prelude.Text)
experimentTemplate_description = Lens.lens (\ExperimentTemplate' {description} -> description) (\s@ExperimentTemplate' {} a -> s {description = a} :: ExperimentTemplate)

-- | The tags for the experiment template.
experimentTemplate_tags :: Lens.Lens' ExperimentTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
experimentTemplate_tags = Lens.lens (\ExperimentTemplate' {tags} -> tags) (\s@ExperimentTemplate' {} a -> s {tags = a} :: ExperimentTemplate) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of an IAM role.
experimentTemplate_roleArn :: Lens.Lens' ExperimentTemplate (Prelude.Maybe Prelude.Text)
experimentTemplate_roleArn = Lens.lens (\ExperimentTemplate' {roleArn} -> roleArn) (\s@ExperimentTemplate' {} a -> s {roleArn = a} :: ExperimentTemplate)

instance Core.FromJSON ExperimentTemplate where
  parseJSON =
    Core.withObject
      "ExperimentTemplate"
      ( \x ->
          ExperimentTemplate'
            Prelude.<$> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "actions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "stopConditions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "targets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "lastUpdateTime")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "roleArn")
      )

instance Prelude.Hashable ExperimentTemplate where
  hashWithSalt _salt ExperimentTemplate' {..} =
    _salt `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` stopConditions
      `Prelude.hashWithSalt` targets
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData ExperimentTemplate where
  rnf ExperimentTemplate' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf actions
      `Prelude.seq` Prelude.rnf stopConditions
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf roleArn
