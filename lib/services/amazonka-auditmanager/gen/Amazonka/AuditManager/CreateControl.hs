{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AuditManager.CreateControl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new custom control in Audit Manager.
module Amazonka.AuditManager.CreateControl
  ( -- * Creating a Request
    CreateControl (..),
    newCreateControl,

    -- * Request Lenses
    createControl_tags,
    createControl_actionPlanInstructions,
    createControl_description,
    createControl_actionPlanTitle,
    createControl_testingInformation,
    createControl_name,
    createControl_controlMappingSources,

    -- * Destructuring the Response
    CreateControlResponse (..),
    newCreateControlResponse,

    -- * Response Lenses
    createControlResponse_control,
    createControlResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateControl' smart constructor.
data CreateControl = CreateControl'
  { -- | The tags that are associated with the control.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The recommended actions to carry out if the control isn\'t fulfilled.
    actionPlanInstructions :: Prelude.Maybe Prelude.Text,
    -- | The description of the control.
    description :: Prelude.Maybe Prelude.Text,
    -- | The title of the action plan for remediating the control.
    actionPlanTitle :: Prelude.Maybe Prelude.Text,
    -- | The steps to follow to determine if the control is satisfied.
    testingInformation :: Prelude.Maybe Prelude.Text,
    -- | The name of the control.
    name :: Prelude.Text,
    -- | The data mapping sources for the control.
    controlMappingSources :: Prelude.NonEmpty CreateControlMappingSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createControl_tags' - The tags that are associated with the control.
--
-- 'actionPlanInstructions', 'createControl_actionPlanInstructions' - The recommended actions to carry out if the control isn\'t fulfilled.
--
-- 'description', 'createControl_description' - The description of the control.
--
-- 'actionPlanTitle', 'createControl_actionPlanTitle' - The title of the action plan for remediating the control.
--
-- 'testingInformation', 'createControl_testingInformation' - The steps to follow to determine if the control is satisfied.
--
-- 'name', 'createControl_name' - The name of the control.
--
-- 'controlMappingSources', 'createControl_controlMappingSources' - The data mapping sources for the control.
newCreateControl ::
  -- | 'name'
  Prelude.Text ->
  -- | 'controlMappingSources'
  Prelude.NonEmpty CreateControlMappingSource ->
  CreateControl
newCreateControl pName_ pControlMappingSources_ =
  CreateControl'
    { tags = Prelude.Nothing,
      actionPlanInstructions = Prelude.Nothing,
      description = Prelude.Nothing,
      actionPlanTitle = Prelude.Nothing,
      testingInformation = Prelude.Nothing,
      name = pName_,
      controlMappingSources =
        Lens.coerced Lens.# pControlMappingSources_
    }

-- | The tags that are associated with the control.
createControl_tags :: Lens.Lens' CreateControl (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createControl_tags = Lens.lens (\CreateControl' {tags} -> tags) (\s@CreateControl' {} a -> s {tags = a} :: CreateControl) Prelude.. Lens.mapping Lens.coerced

-- | The recommended actions to carry out if the control isn\'t fulfilled.
createControl_actionPlanInstructions :: Lens.Lens' CreateControl (Prelude.Maybe Prelude.Text)
createControl_actionPlanInstructions = Lens.lens (\CreateControl' {actionPlanInstructions} -> actionPlanInstructions) (\s@CreateControl' {} a -> s {actionPlanInstructions = a} :: CreateControl)

-- | The description of the control.
createControl_description :: Lens.Lens' CreateControl (Prelude.Maybe Prelude.Text)
createControl_description = Lens.lens (\CreateControl' {description} -> description) (\s@CreateControl' {} a -> s {description = a} :: CreateControl)

-- | The title of the action plan for remediating the control.
createControl_actionPlanTitle :: Lens.Lens' CreateControl (Prelude.Maybe Prelude.Text)
createControl_actionPlanTitle = Lens.lens (\CreateControl' {actionPlanTitle} -> actionPlanTitle) (\s@CreateControl' {} a -> s {actionPlanTitle = a} :: CreateControl)

-- | The steps to follow to determine if the control is satisfied.
createControl_testingInformation :: Lens.Lens' CreateControl (Prelude.Maybe Prelude.Text)
createControl_testingInformation = Lens.lens (\CreateControl' {testingInformation} -> testingInformation) (\s@CreateControl' {} a -> s {testingInformation = a} :: CreateControl)

-- | The name of the control.
createControl_name :: Lens.Lens' CreateControl Prelude.Text
createControl_name = Lens.lens (\CreateControl' {name} -> name) (\s@CreateControl' {} a -> s {name = a} :: CreateControl)

-- | The data mapping sources for the control.
createControl_controlMappingSources :: Lens.Lens' CreateControl (Prelude.NonEmpty CreateControlMappingSource)
createControl_controlMappingSources = Lens.lens (\CreateControl' {controlMappingSources} -> controlMappingSources) (\s@CreateControl' {} a -> s {controlMappingSources = a} :: CreateControl) Prelude.. Lens.coerced

instance Core.AWSRequest CreateControl where
  type
    AWSResponse CreateControl =
      CreateControlResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateControlResponse'
            Prelude.<$> (x Core..?> "control")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateControl where
  hashWithSalt _salt CreateControl' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` actionPlanInstructions
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` actionPlanTitle
      `Prelude.hashWithSalt` testingInformation
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` controlMappingSources

instance Prelude.NFData CreateControl where
  rnf CreateControl' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf actionPlanInstructions
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf actionPlanTitle
      `Prelude.seq` Prelude.rnf testingInformation
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf controlMappingSources

instance Core.ToHeaders CreateControl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateControl where
  toJSON CreateControl' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("actionPlanInstructions" Core..=)
              Prelude.<$> actionPlanInstructions,
            ("description" Core..=) Prelude.<$> description,
            ("actionPlanTitle" Core..=)
              Prelude.<$> actionPlanTitle,
            ("testingInformation" Core..=)
              Prelude.<$> testingInformation,
            Prelude.Just ("name" Core..= name),
            Prelude.Just
              ( "controlMappingSources"
                  Core..= controlMappingSources
              )
          ]
      )

instance Core.ToPath CreateControl where
  toPath = Prelude.const "/controls"

instance Core.ToQuery CreateControl where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateControlResponse' smart constructor.
data CreateControlResponse = CreateControlResponse'
  { -- | The new control that the @CreateControl@ API returned.
    control :: Prelude.Maybe Control,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateControlResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'control', 'createControlResponse_control' - The new control that the @CreateControl@ API returned.
--
-- 'httpStatus', 'createControlResponse_httpStatus' - The response's http status code.
newCreateControlResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateControlResponse
newCreateControlResponse pHttpStatus_ =
  CreateControlResponse'
    { control = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The new control that the @CreateControl@ API returned.
createControlResponse_control :: Lens.Lens' CreateControlResponse (Prelude.Maybe Control)
createControlResponse_control = Lens.lens (\CreateControlResponse' {control} -> control) (\s@CreateControlResponse' {} a -> s {control = a} :: CreateControlResponse)

-- | The response's http status code.
createControlResponse_httpStatus :: Lens.Lens' CreateControlResponse Prelude.Int
createControlResponse_httpStatus = Lens.lens (\CreateControlResponse' {httpStatus} -> httpStatus) (\s@CreateControlResponse' {} a -> s {httpStatus = a} :: CreateControlResponse)

instance Prelude.NFData CreateControlResponse where
  rnf CreateControlResponse' {..} =
    Prelude.rnf control
      `Prelude.seq` Prelude.rnf httpStatus
