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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.Control where

import Amazonka.AuditManager.Types.ControlMappingSource
import Amazonka.AuditManager.Types.ControlType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A control in Audit Manager.
--
-- /See:/ 'newControl' smart constructor.
data Control = Control'
  { -- | The tags associated with the control.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the control.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of control, such as a custom control or a standard control.
    type' :: Prelude.Maybe ControlType,
    -- | Specifies when the control was most recently updated.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | The recommended actions to carry out if the control isn\'t fulfilled.
    actionPlanInstructions :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the control.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The description of the control.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the control.
    id :: Prelude.Maybe Prelude.Text,
    -- | The title of the action plan for remediating the control.
    actionPlanTitle :: Prelude.Maybe Prelude.Text,
    -- | The data mapping sources for the control.
    controlMappingSources :: Prelude.Maybe (Prelude.NonEmpty ControlMappingSource),
    -- | The IAM user or role that created the control.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | Specifies when the control was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The IAM user or role that most recently updated the control.
    lastUpdatedBy :: Prelude.Maybe Prelude.Text,
    -- | The data source types that determine where Audit Manager collects
    -- evidence from for the control.
    controlSources :: Prelude.Maybe Prelude.Text,
    -- | The steps that you should follow to determine if the control has been
    -- satisfied.
    testingInformation :: Prelude.Maybe Prelude.Text
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
-- 'tags', 'control_tags' - The tags associated with the control.
--
-- 'name', 'control_name' - The name of the control.
--
-- 'type'', 'control_type' - The type of control, such as a custom control or a standard control.
--
-- 'lastUpdatedAt', 'control_lastUpdatedAt' - Specifies when the control was most recently updated.
--
-- 'actionPlanInstructions', 'control_actionPlanInstructions' - The recommended actions to carry out if the control isn\'t fulfilled.
--
-- 'arn', 'control_arn' - The Amazon Resource Name (ARN) of the control.
--
-- 'description', 'control_description' - The description of the control.
--
-- 'id', 'control_id' - The unique identifier for the control.
--
-- 'actionPlanTitle', 'control_actionPlanTitle' - The title of the action plan for remediating the control.
--
-- 'controlMappingSources', 'control_controlMappingSources' - The data mapping sources for the control.
--
-- 'createdBy', 'control_createdBy' - The IAM user or role that created the control.
--
-- 'createdAt', 'control_createdAt' - Specifies when the control was created.
--
-- 'lastUpdatedBy', 'control_lastUpdatedBy' - The IAM user or role that most recently updated the control.
--
-- 'controlSources', 'control_controlSources' - The data source types that determine where Audit Manager collects
-- evidence from for the control.
--
-- 'testingInformation', 'control_testingInformation' - The steps that you should follow to determine if the control has been
-- satisfied.
newControl ::
  Control
newControl =
  Control'
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      actionPlanInstructions = Prelude.Nothing,
      arn = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      actionPlanTitle = Prelude.Nothing,
      controlMappingSources = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      lastUpdatedBy = Prelude.Nothing,
      controlSources = Prelude.Nothing,
      testingInformation = Prelude.Nothing
    }

-- | The tags associated with the control.
control_tags :: Lens.Lens' Control (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
control_tags = Lens.lens (\Control' {tags} -> tags) (\s@Control' {} a -> s {tags = a} :: Control) Prelude.. Lens.mapping Lens.coerced

-- | The name of the control.
control_name :: Lens.Lens' Control (Prelude.Maybe Prelude.Text)
control_name = Lens.lens (\Control' {name} -> name) (\s@Control' {} a -> s {name = a} :: Control)

-- | The type of control, such as a custom control or a standard control.
control_type :: Lens.Lens' Control (Prelude.Maybe ControlType)
control_type = Lens.lens (\Control' {type'} -> type') (\s@Control' {} a -> s {type' = a} :: Control)

-- | Specifies when the control was most recently updated.
control_lastUpdatedAt :: Lens.Lens' Control (Prelude.Maybe Prelude.UTCTime)
control_lastUpdatedAt = Lens.lens (\Control' {lastUpdatedAt} -> lastUpdatedAt) (\s@Control' {} a -> s {lastUpdatedAt = a} :: Control) Prelude.. Lens.mapping Data._Time

-- | The recommended actions to carry out if the control isn\'t fulfilled.
control_actionPlanInstructions :: Lens.Lens' Control (Prelude.Maybe Prelude.Text)
control_actionPlanInstructions = Lens.lens (\Control' {actionPlanInstructions} -> actionPlanInstructions) (\s@Control' {} a -> s {actionPlanInstructions = a} :: Control)

-- | The Amazon Resource Name (ARN) of the control.
control_arn :: Lens.Lens' Control (Prelude.Maybe Prelude.Text)
control_arn = Lens.lens (\Control' {arn} -> arn) (\s@Control' {} a -> s {arn = a} :: Control)

-- | The description of the control.
control_description :: Lens.Lens' Control (Prelude.Maybe Prelude.Text)
control_description = Lens.lens (\Control' {description} -> description) (\s@Control' {} a -> s {description = a} :: Control)

-- | The unique identifier for the control.
control_id :: Lens.Lens' Control (Prelude.Maybe Prelude.Text)
control_id = Lens.lens (\Control' {id} -> id) (\s@Control' {} a -> s {id = a} :: Control)

-- | The title of the action plan for remediating the control.
control_actionPlanTitle :: Lens.Lens' Control (Prelude.Maybe Prelude.Text)
control_actionPlanTitle = Lens.lens (\Control' {actionPlanTitle} -> actionPlanTitle) (\s@Control' {} a -> s {actionPlanTitle = a} :: Control)

-- | The data mapping sources for the control.
control_controlMappingSources :: Lens.Lens' Control (Prelude.Maybe (Prelude.NonEmpty ControlMappingSource))
control_controlMappingSources = Lens.lens (\Control' {controlMappingSources} -> controlMappingSources) (\s@Control' {} a -> s {controlMappingSources = a} :: Control) Prelude.. Lens.mapping Lens.coerced

-- | The IAM user or role that created the control.
control_createdBy :: Lens.Lens' Control (Prelude.Maybe Prelude.Text)
control_createdBy = Lens.lens (\Control' {createdBy} -> createdBy) (\s@Control' {} a -> s {createdBy = a} :: Control)

-- | Specifies when the control was created.
control_createdAt :: Lens.Lens' Control (Prelude.Maybe Prelude.UTCTime)
control_createdAt = Lens.lens (\Control' {createdAt} -> createdAt) (\s@Control' {} a -> s {createdAt = a} :: Control) Prelude.. Lens.mapping Data._Time

-- | The IAM user or role that most recently updated the control.
control_lastUpdatedBy :: Lens.Lens' Control (Prelude.Maybe Prelude.Text)
control_lastUpdatedBy = Lens.lens (\Control' {lastUpdatedBy} -> lastUpdatedBy) (\s@Control' {} a -> s {lastUpdatedBy = a} :: Control)

-- | The data source types that determine where Audit Manager collects
-- evidence from for the control.
control_controlSources :: Lens.Lens' Control (Prelude.Maybe Prelude.Text)
control_controlSources = Lens.lens (\Control' {controlSources} -> controlSources) (\s@Control' {} a -> s {controlSources = a} :: Control)

-- | The steps that you should follow to determine if the control has been
-- satisfied.
control_testingInformation :: Lens.Lens' Control (Prelude.Maybe Prelude.Text)
control_testingInformation = Lens.lens (\Control' {testingInformation} -> testingInformation) (\s@Control' {} a -> s {testingInformation = a} :: Control)

instance Data.FromJSON Control where
  parseJSON =
    Data.withObject
      "Control"
      ( \x ->
          Control'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..:? "lastUpdatedAt")
            Prelude.<*> (x Data..:? "actionPlanInstructions")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "actionPlanTitle")
            Prelude.<*> (x Data..:? "controlMappingSources")
            Prelude.<*> (x Data..:? "createdBy")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "lastUpdatedBy")
            Prelude.<*> (x Data..:? "controlSources")
            Prelude.<*> (x Data..:? "testingInformation")
      )

instance Prelude.Hashable Control where
  hashWithSalt _salt Control' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` actionPlanInstructions
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` actionPlanTitle
      `Prelude.hashWithSalt` controlMappingSources
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` lastUpdatedBy
      `Prelude.hashWithSalt` controlSources
      `Prelude.hashWithSalt` testingInformation

instance Prelude.NFData Control where
  rnf Control' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf actionPlanInstructions
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf actionPlanTitle
      `Prelude.seq` Prelude.rnf controlMappingSources
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastUpdatedBy
      `Prelude.seq` Prelude.rnf controlSources
      `Prelude.seq` Prelude.rnf testingInformation
