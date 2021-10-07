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
-- Module      : Network.AWS.Glue.UpdateWorkflow
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing workflow.
module Network.AWS.Glue.UpdateWorkflow
  ( -- * Creating a Request
    UpdateWorkflow (..),
    newUpdateWorkflow,

    -- * Request Lenses
    updateWorkflow_defaultRunProperties,
    updateWorkflow_maxConcurrentRuns,
    updateWorkflow_description,
    updateWorkflow_name,

    -- * Destructuring the Response
    UpdateWorkflowResponse (..),
    newUpdateWorkflowResponse,

    -- * Response Lenses
    updateWorkflowResponse_name,
    updateWorkflowResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateWorkflow' smart constructor.
data UpdateWorkflow = UpdateWorkflow'
  { -- | A collection of properties to be used as part of each execution of the
    -- workflow.
    defaultRunProperties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | You can use this parameter to prevent unwanted multiple updates to data,
    -- to control costs, or in some cases, to prevent exceeding the maximum
    -- number of concurrent runs of any of the component jobs. If you leave
    -- this parameter blank, there is no limit to the number of concurrent
    -- workflow runs.
    maxConcurrentRuns :: Prelude.Maybe Prelude.Int,
    -- | The description of the workflow.
    description :: Prelude.Maybe Prelude.Text,
    -- | Name of the workflow to be updated.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkflow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultRunProperties', 'updateWorkflow_defaultRunProperties' - A collection of properties to be used as part of each execution of the
-- workflow.
--
-- 'maxConcurrentRuns', 'updateWorkflow_maxConcurrentRuns' - You can use this parameter to prevent unwanted multiple updates to data,
-- to control costs, or in some cases, to prevent exceeding the maximum
-- number of concurrent runs of any of the component jobs. If you leave
-- this parameter blank, there is no limit to the number of concurrent
-- workflow runs.
--
-- 'description', 'updateWorkflow_description' - The description of the workflow.
--
-- 'name', 'updateWorkflow_name' - Name of the workflow to be updated.
newUpdateWorkflow ::
  -- | 'name'
  Prelude.Text ->
  UpdateWorkflow
newUpdateWorkflow pName_ =
  UpdateWorkflow'
    { defaultRunProperties =
        Prelude.Nothing,
      maxConcurrentRuns = Prelude.Nothing,
      description = Prelude.Nothing,
      name = pName_
    }

-- | A collection of properties to be used as part of each execution of the
-- workflow.
updateWorkflow_defaultRunProperties :: Lens.Lens' UpdateWorkflow (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateWorkflow_defaultRunProperties = Lens.lens (\UpdateWorkflow' {defaultRunProperties} -> defaultRunProperties) (\s@UpdateWorkflow' {} a -> s {defaultRunProperties = a} :: UpdateWorkflow) Prelude.. Lens.mapping Lens._Coerce

-- | You can use this parameter to prevent unwanted multiple updates to data,
-- to control costs, or in some cases, to prevent exceeding the maximum
-- number of concurrent runs of any of the component jobs. If you leave
-- this parameter blank, there is no limit to the number of concurrent
-- workflow runs.
updateWorkflow_maxConcurrentRuns :: Lens.Lens' UpdateWorkflow (Prelude.Maybe Prelude.Int)
updateWorkflow_maxConcurrentRuns = Lens.lens (\UpdateWorkflow' {maxConcurrentRuns} -> maxConcurrentRuns) (\s@UpdateWorkflow' {} a -> s {maxConcurrentRuns = a} :: UpdateWorkflow)

-- | The description of the workflow.
updateWorkflow_description :: Lens.Lens' UpdateWorkflow (Prelude.Maybe Prelude.Text)
updateWorkflow_description = Lens.lens (\UpdateWorkflow' {description} -> description) (\s@UpdateWorkflow' {} a -> s {description = a} :: UpdateWorkflow)

-- | Name of the workflow to be updated.
updateWorkflow_name :: Lens.Lens' UpdateWorkflow Prelude.Text
updateWorkflow_name = Lens.lens (\UpdateWorkflow' {name} -> name) (\s@UpdateWorkflow' {} a -> s {name = a} :: UpdateWorkflow)

instance Core.AWSRequest UpdateWorkflow where
  type
    AWSResponse UpdateWorkflow =
      UpdateWorkflowResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateWorkflowResponse'
            Prelude.<$> (x Core..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateWorkflow

instance Prelude.NFData UpdateWorkflow

instance Core.ToHeaders UpdateWorkflow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.UpdateWorkflow" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateWorkflow where
  toJSON UpdateWorkflow' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DefaultRunProperties" Core..=)
              Prelude.<$> defaultRunProperties,
            ("MaxConcurrentRuns" Core..=)
              Prelude.<$> maxConcurrentRuns,
            ("Description" Core..=) Prelude.<$> description,
            Prelude.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath UpdateWorkflow where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateWorkflow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateWorkflowResponse' smart constructor.
data UpdateWorkflowResponse = UpdateWorkflowResponse'
  { -- | The name of the workflow which was specified in input.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkflowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateWorkflowResponse_name' - The name of the workflow which was specified in input.
--
-- 'httpStatus', 'updateWorkflowResponse_httpStatus' - The response's http status code.
newUpdateWorkflowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateWorkflowResponse
newUpdateWorkflowResponse pHttpStatus_ =
  UpdateWorkflowResponse'
    { name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the workflow which was specified in input.
updateWorkflowResponse_name :: Lens.Lens' UpdateWorkflowResponse (Prelude.Maybe Prelude.Text)
updateWorkflowResponse_name = Lens.lens (\UpdateWorkflowResponse' {name} -> name) (\s@UpdateWorkflowResponse' {} a -> s {name = a} :: UpdateWorkflowResponse)

-- | The response's http status code.
updateWorkflowResponse_httpStatus :: Lens.Lens' UpdateWorkflowResponse Prelude.Int
updateWorkflowResponse_httpStatus = Lens.lens (\UpdateWorkflowResponse' {httpStatus} -> httpStatus) (\s@UpdateWorkflowResponse' {} a -> s {httpStatus = a} :: UpdateWorkflowResponse)

instance Prelude.NFData UpdateWorkflowResponse
