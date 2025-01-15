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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.ExperimentTemplate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FIS.Types.ExperimentTemplateAction
import Amazonka.FIS.Types.ExperimentTemplateLogConfiguration
import Amazonka.FIS.Types.ExperimentTemplateStopCondition
import Amazonka.FIS.Types.ExperimentTemplateTarget
import qualified Amazonka.Prelude as Prelude

-- | Describes an experiment template.
--
-- /See:/ 'newExperimentTemplate' smart constructor.
data ExperimentTemplate = ExperimentTemplate'
  { -- | The actions for the experiment.
    actions :: Prelude.Maybe (Prelude.HashMap Prelude.Text ExperimentTemplateAction),
    -- | The time the experiment template was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The description for the experiment template.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the experiment template.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time the experiment template was last updated.
    lastUpdateTime :: Prelude.Maybe Data.POSIX,
    -- | The configuration for experiment logging.
    logConfiguration :: Prelude.Maybe ExperimentTemplateLogConfiguration,
    -- | The Amazon Resource Name (ARN) of an IAM role.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The stop conditions for the experiment.
    stopConditions :: Prelude.Maybe [ExperimentTemplateStopCondition],
    -- | The tags for the experiment template.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The targets for the experiment.
    targets :: Prelude.Maybe (Prelude.HashMap Prelude.Text ExperimentTemplateTarget)
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
-- 'actions', 'experimentTemplate_actions' - The actions for the experiment.
--
-- 'creationTime', 'experimentTemplate_creationTime' - The time the experiment template was created.
--
-- 'description', 'experimentTemplate_description' - The description for the experiment template.
--
-- 'id', 'experimentTemplate_id' - The ID of the experiment template.
--
-- 'lastUpdateTime', 'experimentTemplate_lastUpdateTime' - The time the experiment template was last updated.
--
-- 'logConfiguration', 'experimentTemplate_logConfiguration' - The configuration for experiment logging.
--
-- 'roleArn', 'experimentTemplate_roleArn' - The Amazon Resource Name (ARN) of an IAM role.
--
-- 'stopConditions', 'experimentTemplate_stopConditions' - The stop conditions for the experiment.
--
-- 'tags', 'experimentTemplate_tags' - The tags for the experiment template.
--
-- 'targets', 'experimentTemplate_targets' - The targets for the experiment.
newExperimentTemplate ::
  ExperimentTemplate
newExperimentTemplate =
  ExperimentTemplate'
    { actions = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      logConfiguration = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      stopConditions = Prelude.Nothing,
      tags = Prelude.Nothing,
      targets = Prelude.Nothing
    }

-- | The actions for the experiment.
experimentTemplate_actions :: Lens.Lens' ExperimentTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text ExperimentTemplateAction))
experimentTemplate_actions = Lens.lens (\ExperimentTemplate' {actions} -> actions) (\s@ExperimentTemplate' {} a -> s {actions = a} :: ExperimentTemplate) Prelude.. Lens.mapping Lens.coerced

-- | The time the experiment template was created.
experimentTemplate_creationTime :: Lens.Lens' ExperimentTemplate (Prelude.Maybe Prelude.UTCTime)
experimentTemplate_creationTime = Lens.lens (\ExperimentTemplate' {creationTime} -> creationTime) (\s@ExperimentTemplate' {} a -> s {creationTime = a} :: ExperimentTemplate) Prelude.. Lens.mapping Data._Time

-- | The description for the experiment template.
experimentTemplate_description :: Lens.Lens' ExperimentTemplate (Prelude.Maybe Prelude.Text)
experimentTemplate_description = Lens.lens (\ExperimentTemplate' {description} -> description) (\s@ExperimentTemplate' {} a -> s {description = a} :: ExperimentTemplate)

-- | The ID of the experiment template.
experimentTemplate_id :: Lens.Lens' ExperimentTemplate (Prelude.Maybe Prelude.Text)
experimentTemplate_id = Lens.lens (\ExperimentTemplate' {id} -> id) (\s@ExperimentTemplate' {} a -> s {id = a} :: ExperimentTemplate)

-- | The time the experiment template was last updated.
experimentTemplate_lastUpdateTime :: Lens.Lens' ExperimentTemplate (Prelude.Maybe Prelude.UTCTime)
experimentTemplate_lastUpdateTime = Lens.lens (\ExperimentTemplate' {lastUpdateTime} -> lastUpdateTime) (\s@ExperimentTemplate' {} a -> s {lastUpdateTime = a} :: ExperimentTemplate) Prelude.. Lens.mapping Data._Time

-- | The configuration for experiment logging.
experimentTemplate_logConfiguration :: Lens.Lens' ExperimentTemplate (Prelude.Maybe ExperimentTemplateLogConfiguration)
experimentTemplate_logConfiguration = Lens.lens (\ExperimentTemplate' {logConfiguration} -> logConfiguration) (\s@ExperimentTemplate' {} a -> s {logConfiguration = a} :: ExperimentTemplate)

-- | The Amazon Resource Name (ARN) of an IAM role.
experimentTemplate_roleArn :: Lens.Lens' ExperimentTemplate (Prelude.Maybe Prelude.Text)
experimentTemplate_roleArn = Lens.lens (\ExperimentTemplate' {roleArn} -> roleArn) (\s@ExperimentTemplate' {} a -> s {roleArn = a} :: ExperimentTemplate)

-- | The stop conditions for the experiment.
experimentTemplate_stopConditions :: Lens.Lens' ExperimentTemplate (Prelude.Maybe [ExperimentTemplateStopCondition])
experimentTemplate_stopConditions = Lens.lens (\ExperimentTemplate' {stopConditions} -> stopConditions) (\s@ExperimentTemplate' {} a -> s {stopConditions = a} :: ExperimentTemplate) Prelude.. Lens.mapping Lens.coerced

-- | The tags for the experiment template.
experimentTemplate_tags :: Lens.Lens' ExperimentTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
experimentTemplate_tags = Lens.lens (\ExperimentTemplate' {tags} -> tags) (\s@ExperimentTemplate' {} a -> s {tags = a} :: ExperimentTemplate) Prelude.. Lens.mapping Lens.coerced

-- | The targets for the experiment.
experimentTemplate_targets :: Lens.Lens' ExperimentTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text ExperimentTemplateTarget))
experimentTemplate_targets = Lens.lens (\ExperimentTemplate' {targets} -> targets) (\s@ExperimentTemplate' {} a -> s {targets = a} :: ExperimentTemplate) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ExperimentTemplate where
  parseJSON =
    Data.withObject
      "ExperimentTemplate"
      ( \x ->
          ExperimentTemplate'
            Prelude.<$> (x Data..:? "actions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "lastUpdateTime")
            Prelude.<*> (x Data..:? "logConfiguration")
            Prelude.<*> (x Data..:? "roleArn")
            Prelude.<*> (x Data..:? "stopConditions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "targets" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ExperimentTemplate where
  hashWithSalt _salt ExperimentTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` logConfiguration
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` stopConditions
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` targets

instance Prelude.NFData ExperimentTemplate where
  rnf ExperimentTemplate' {..} =
    Prelude.rnf actions `Prelude.seq`
      Prelude.rnf creationTime `Prelude.seq`
        Prelude.rnf description `Prelude.seq`
          Prelude.rnf id `Prelude.seq`
            Prelude.rnf lastUpdateTime `Prelude.seq`
              Prelude.rnf logConfiguration `Prelude.seq`
                Prelude.rnf roleArn `Prelude.seq`
                  Prelude.rnf stopConditions `Prelude.seq`
                    Prelude.rnf tags `Prelude.seq`
                      Prelude.rnf targets
