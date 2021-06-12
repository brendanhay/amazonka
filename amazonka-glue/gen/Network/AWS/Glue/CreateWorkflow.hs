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
-- Module      : Network.AWS.Glue.CreateWorkflow
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new workflow.
module Network.AWS.Glue.CreateWorkflow
  ( -- * Creating a Request
    CreateWorkflow (..),
    newCreateWorkflow,

    -- * Request Lenses
    createWorkflow_defaultRunProperties,
    createWorkflow_maxConcurrentRuns,
    createWorkflow_tags,
    createWorkflow_description,
    createWorkflow_name,

    -- * Destructuring the Response
    CreateWorkflowResponse (..),
    newCreateWorkflowResponse,

    -- * Response Lenses
    createWorkflowResponse_name,
    createWorkflowResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateWorkflow' smart constructor.
data CreateWorkflow = CreateWorkflow'
  { -- | A collection of properties to be used as part of each execution of the
    -- workflow.
    defaultRunProperties :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | You can use this parameter to prevent unwanted multiple updates to data,
    -- to control costs, or in some cases, to prevent exceeding the maximum
    -- number of concurrent runs of any of the component jobs. If you leave
    -- this parameter blank, there is no limit to the number of concurrent
    -- workflow runs.
    maxConcurrentRuns :: Core.Maybe Core.Int,
    -- | The tags to be used with this workflow.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A description of the workflow.
    description :: Core.Maybe Core.Text,
    -- | The name to be assigned to the workflow. It should be unique within your
    -- account.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateWorkflow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultRunProperties', 'createWorkflow_defaultRunProperties' - A collection of properties to be used as part of each execution of the
-- workflow.
--
-- 'maxConcurrentRuns', 'createWorkflow_maxConcurrentRuns' - You can use this parameter to prevent unwanted multiple updates to data,
-- to control costs, or in some cases, to prevent exceeding the maximum
-- number of concurrent runs of any of the component jobs. If you leave
-- this parameter blank, there is no limit to the number of concurrent
-- workflow runs.
--
-- 'tags', 'createWorkflow_tags' - The tags to be used with this workflow.
--
-- 'description', 'createWorkflow_description' - A description of the workflow.
--
-- 'name', 'createWorkflow_name' - The name to be assigned to the workflow. It should be unique within your
-- account.
newCreateWorkflow ::
  -- | 'name'
  Core.Text ->
  CreateWorkflow
newCreateWorkflow pName_ =
  CreateWorkflow'
    { defaultRunProperties =
        Core.Nothing,
      maxConcurrentRuns = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      name = pName_
    }

-- | A collection of properties to be used as part of each execution of the
-- workflow.
createWorkflow_defaultRunProperties :: Lens.Lens' CreateWorkflow (Core.Maybe (Core.HashMap Core.Text Core.Text))
createWorkflow_defaultRunProperties = Lens.lens (\CreateWorkflow' {defaultRunProperties} -> defaultRunProperties) (\s@CreateWorkflow' {} a -> s {defaultRunProperties = a} :: CreateWorkflow) Core.. Lens.mapping Lens._Coerce

-- | You can use this parameter to prevent unwanted multiple updates to data,
-- to control costs, or in some cases, to prevent exceeding the maximum
-- number of concurrent runs of any of the component jobs. If you leave
-- this parameter blank, there is no limit to the number of concurrent
-- workflow runs.
createWorkflow_maxConcurrentRuns :: Lens.Lens' CreateWorkflow (Core.Maybe Core.Int)
createWorkflow_maxConcurrentRuns = Lens.lens (\CreateWorkflow' {maxConcurrentRuns} -> maxConcurrentRuns) (\s@CreateWorkflow' {} a -> s {maxConcurrentRuns = a} :: CreateWorkflow)

-- | The tags to be used with this workflow.
createWorkflow_tags :: Lens.Lens' CreateWorkflow (Core.Maybe (Core.HashMap Core.Text Core.Text))
createWorkflow_tags = Lens.lens (\CreateWorkflow' {tags} -> tags) (\s@CreateWorkflow' {} a -> s {tags = a} :: CreateWorkflow) Core.. Lens.mapping Lens._Coerce

-- | A description of the workflow.
createWorkflow_description :: Lens.Lens' CreateWorkflow (Core.Maybe Core.Text)
createWorkflow_description = Lens.lens (\CreateWorkflow' {description} -> description) (\s@CreateWorkflow' {} a -> s {description = a} :: CreateWorkflow)

-- | The name to be assigned to the workflow. It should be unique within your
-- account.
createWorkflow_name :: Lens.Lens' CreateWorkflow Core.Text
createWorkflow_name = Lens.lens (\CreateWorkflow' {name} -> name) (\s@CreateWorkflow' {} a -> s {name = a} :: CreateWorkflow)

instance Core.AWSRequest CreateWorkflow where
  type
    AWSResponse CreateWorkflow =
      CreateWorkflowResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWorkflowResponse'
            Core.<$> (x Core..?> "Name")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateWorkflow

instance Core.NFData CreateWorkflow

instance Core.ToHeaders CreateWorkflow where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.CreateWorkflow" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateWorkflow where
  toJSON CreateWorkflow' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DefaultRunProperties" Core..=)
              Core.<$> defaultRunProperties,
            ("MaxConcurrentRuns" Core..=)
              Core.<$> maxConcurrentRuns,
            ("Tags" Core..=) Core.<$> tags,
            ("Description" Core..=) Core.<$> description,
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath CreateWorkflow where
  toPath = Core.const "/"

instance Core.ToQuery CreateWorkflow where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateWorkflowResponse' smart constructor.
data CreateWorkflowResponse = CreateWorkflowResponse'
  { -- | The name of the workflow which was provided as part of the request.
    name :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateWorkflowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createWorkflowResponse_name' - The name of the workflow which was provided as part of the request.
--
-- 'httpStatus', 'createWorkflowResponse_httpStatus' - The response's http status code.
newCreateWorkflowResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateWorkflowResponse
newCreateWorkflowResponse pHttpStatus_ =
  CreateWorkflowResponse'
    { name = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the workflow which was provided as part of the request.
createWorkflowResponse_name :: Lens.Lens' CreateWorkflowResponse (Core.Maybe Core.Text)
createWorkflowResponse_name = Lens.lens (\CreateWorkflowResponse' {name} -> name) (\s@CreateWorkflowResponse' {} a -> s {name = a} :: CreateWorkflowResponse)

-- | The response's http status code.
createWorkflowResponse_httpStatus :: Lens.Lens' CreateWorkflowResponse Core.Int
createWorkflowResponse_httpStatus = Lens.lens (\CreateWorkflowResponse' {httpStatus} -> httpStatus) (\s@CreateWorkflowResponse' {} a -> s {httpStatus = a} :: CreateWorkflowResponse)

instance Core.NFData CreateWorkflowResponse
