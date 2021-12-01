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
-- Module      : Amazonka.AuditManager.Types.Control
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.Control where

import Amazonka.AuditManager.Types.ControlMappingSource
import Amazonka.AuditManager.Types.ControlType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A control in Audit Manager.
--
-- /See:/ 'newControl' smart constructor.
data Control = Control'
  { -- | The IAM user or role that most recently updated the control.
    lastUpdatedBy :: Prelude.Maybe Prelude.Text,
    -- | The steps to follow to determine if the control has been satisfied.
    testingInformation :: Prelude.Maybe Prelude.Text,
    -- | Specifies when the control was most recently updated.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the specified control.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Specifies when the control was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The data mapping sources for the specified control.
    controlMappingSources :: Prelude.Maybe (Prelude.NonEmpty ControlMappingSource),
    -- | The IAM user or role that created the control.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The recommended actions to carry out if the control is not fulfilled.
    actionPlanInstructions :: Prelude.Maybe Prelude.Text,
    -- | The data source that determines from where Audit Manager collects
    -- evidence for the control.
    controlSources :: Prelude.Maybe Prelude.Text,
    -- | The name of the specified control.
    name :: Prelude.Maybe Prelude.Text,
    -- | The title of the action plan for remediating the control.
    actionPlanTitle :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the control.
    id :: Prelude.Maybe Prelude.Text,
    -- | The type of control, such as custom or standard.
    type' :: Prelude.Maybe ControlType,
    -- | The description of the specified control.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags associated with the control.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Control' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedBy', 'control_lastUpdatedBy' - The IAM user or role that most recently updated the control.
--
-- 'testingInformation', 'control_testingInformation' - The steps to follow to determine if the control has been satisfied.
--
-- 'lastUpdatedAt', 'control_lastUpdatedAt' - Specifies when the control was most recently updated.
--
-- 'arn', 'control_arn' - The Amazon Resource Name (ARN) of the specified control.
--
-- 'createdAt', 'control_createdAt' - Specifies when the control was created.
--
-- 'controlMappingSources', 'control_controlMappingSources' - The data mapping sources for the specified control.
--
-- 'createdBy', 'control_createdBy' - The IAM user or role that created the control.
--
-- 'actionPlanInstructions', 'control_actionPlanInstructions' - The recommended actions to carry out if the control is not fulfilled.
--
-- 'controlSources', 'control_controlSources' - The data source that determines from where Audit Manager collects
-- evidence for the control.
--
-- 'name', 'control_name' - The name of the specified control.
--
-- 'actionPlanTitle', 'control_actionPlanTitle' - The title of the action plan for remediating the control.
--
-- 'id', 'control_id' - The unique identifier for the control.
--
-- 'type'', 'control_type' - The type of control, such as custom or standard.
--
-- 'description', 'control_description' - The description of the specified control.
--
-- 'tags', 'control_tags' - The tags associated with the control.
newControl ::
  Control
newControl =
  Control'
    { lastUpdatedBy = Prelude.Nothing,
      testingInformation = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      controlMappingSources = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      actionPlanInstructions = Prelude.Nothing,
      controlSources = Prelude.Nothing,
      name = Prelude.Nothing,
      actionPlanTitle = Prelude.Nothing,
      id = Prelude.Nothing,
      type' = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The IAM user or role that most recently updated the control.
control_lastUpdatedBy :: Lens.Lens' Control (Prelude.Maybe Prelude.Text)
control_lastUpdatedBy = Lens.lens (\Control' {lastUpdatedBy} -> lastUpdatedBy) (\s@Control' {} a -> s {lastUpdatedBy = a} :: Control)

-- | The steps to follow to determine if the control has been satisfied.
control_testingInformation :: Lens.Lens' Control (Prelude.Maybe Prelude.Text)
control_testingInformation = Lens.lens (\Control' {testingInformation} -> testingInformation) (\s@Control' {} a -> s {testingInformation = a} :: Control)

-- | Specifies when the control was most recently updated.
control_lastUpdatedAt :: Lens.Lens' Control (Prelude.Maybe Prelude.UTCTime)
control_lastUpdatedAt = Lens.lens (\Control' {lastUpdatedAt} -> lastUpdatedAt) (\s@Control' {} a -> s {lastUpdatedAt = a} :: Control) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the specified control.
control_arn :: Lens.Lens' Control (Prelude.Maybe Prelude.Text)
control_arn = Lens.lens (\Control' {arn} -> arn) (\s@Control' {} a -> s {arn = a} :: Control)

-- | Specifies when the control was created.
control_createdAt :: Lens.Lens' Control (Prelude.Maybe Prelude.UTCTime)
control_createdAt = Lens.lens (\Control' {createdAt} -> createdAt) (\s@Control' {} a -> s {createdAt = a} :: Control) Prelude.. Lens.mapping Core._Time

-- | The data mapping sources for the specified control.
control_controlMappingSources :: Lens.Lens' Control (Prelude.Maybe (Prelude.NonEmpty ControlMappingSource))
control_controlMappingSources = Lens.lens (\Control' {controlMappingSources} -> controlMappingSources) (\s@Control' {} a -> s {controlMappingSources = a} :: Control) Prelude.. Lens.mapping Lens.coerced

-- | The IAM user or role that created the control.
control_createdBy :: Lens.Lens' Control (Prelude.Maybe Prelude.Text)
control_createdBy = Lens.lens (\Control' {createdBy} -> createdBy) (\s@Control' {} a -> s {createdBy = a} :: Control)

-- | The recommended actions to carry out if the control is not fulfilled.
control_actionPlanInstructions :: Lens.Lens' Control (Prelude.Maybe Prelude.Text)
control_actionPlanInstructions = Lens.lens (\Control' {actionPlanInstructions} -> actionPlanInstructions) (\s@Control' {} a -> s {actionPlanInstructions = a} :: Control)

-- | The data source that determines from where Audit Manager collects
-- evidence for the control.
control_controlSources :: Lens.Lens' Control (Prelude.Maybe Prelude.Text)
control_controlSources = Lens.lens (\Control' {controlSources} -> controlSources) (\s@Control' {} a -> s {controlSources = a} :: Control)

-- | The name of the specified control.
control_name :: Lens.Lens' Control (Prelude.Maybe Prelude.Text)
control_name = Lens.lens (\Control' {name} -> name) (\s@Control' {} a -> s {name = a} :: Control)

-- | The title of the action plan for remediating the control.
control_actionPlanTitle :: Lens.Lens' Control (Prelude.Maybe Prelude.Text)
control_actionPlanTitle = Lens.lens (\Control' {actionPlanTitle} -> actionPlanTitle) (\s@Control' {} a -> s {actionPlanTitle = a} :: Control)

-- | The unique identifier for the control.
control_id :: Lens.Lens' Control (Prelude.Maybe Prelude.Text)
control_id = Lens.lens (\Control' {id} -> id) (\s@Control' {} a -> s {id = a} :: Control)

-- | The type of control, such as custom or standard.
control_type :: Lens.Lens' Control (Prelude.Maybe ControlType)
control_type = Lens.lens (\Control' {type'} -> type') (\s@Control' {} a -> s {type' = a} :: Control)

-- | The description of the specified control.
control_description :: Lens.Lens' Control (Prelude.Maybe Prelude.Text)
control_description = Lens.lens (\Control' {description} -> description) (\s@Control' {} a -> s {description = a} :: Control)

-- | The tags associated with the control.
control_tags :: Lens.Lens' Control (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
control_tags = Lens.lens (\Control' {tags} -> tags) (\s@Control' {} a -> s {tags = a} :: Control) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Control where
  parseJSON =
    Core.withObject
      "Control"
      ( \x ->
          Control'
            Prelude.<$> (x Core..:? "lastUpdatedBy")
            Prelude.<*> (x Core..:? "testingInformation")
            Prelude.<*> (x Core..:? "lastUpdatedAt")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "controlMappingSources")
            Prelude.<*> (x Core..:? "createdBy")
            Prelude.<*> (x Core..:? "actionPlanInstructions")
            Prelude.<*> (x Core..:? "controlSources")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "actionPlanTitle")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "type")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Control where
  hashWithSalt salt' Control' {..} =
    salt' `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` actionPlanTitle
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` controlSources
      `Prelude.hashWithSalt` actionPlanInstructions
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` controlMappingSources
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` testingInformation
      `Prelude.hashWithSalt` lastUpdatedBy

instance Prelude.NFData Control where
  rnf Control' {..} =
    Prelude.rnf lastUpdatedBy
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf actionPlanTitle
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf controlSources
      `Prelude.seq` Prelude.rnf actionPlanInstructions
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf controlMappingSources
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf testingInformation
