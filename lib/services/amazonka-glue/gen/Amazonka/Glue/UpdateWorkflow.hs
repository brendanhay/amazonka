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
-- Module      : Amazonka.Glue.UpdateWorkflow
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing workflow.
module Amazonka.Glue.UpdateWorkflow
  ( -- * Creating a Request
    UpdateWorkflow (..),
    newUpdateWorkflow,

    -- * Request Lenses
    updateWorkflow_maxConcurrentRuns,
    updateWorkflow_defaultRunProperties,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateWorkflow' smart constructor.
data UpdateWorkflow = UpdateWorkflow'
  { -- | You can use this parameter to prevent unwanted multiple updates to data,
    -- to control costs, or in some cases, to prevent exceeding the maximum
    -- number of concurrent runs of any of the component jobs. If you leave
    -- this parameter blank, there is no limit to the number of concurrent
    -- workflow runs.
    maxConcurrentRuns :: Prelude.Maybe Prelude.Int,
    -- | A collection of properties to be used as part of each execution of the
    -- workflow.
    defaultRunProperties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
-- 'maxConcurrentRuns', 'updateWorkflow_maxConcurrentRuns' - You can use this parameter to prevent unwanted multiple updates to data,
-- to control costs, or in some cases, to prevent exceeding the maximum
-- number of concurrent runs of any of the component jobs. If you leave
-- this parameter blank, there is no limit to the number of concurrent
-- workflow runs.
--
-- 'defaultRunProperties', 'updateWorkflow_defaultRunProperties' - A collection of properties to be used as part of each execution of the
-- workflow.
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
    { maxConcurrentRuns =
        Prelude.Nothing,
      defaultRunProperties = Prelude.Nothing,
      description = Prelude.Nothing,
      name = pName_
    }

-- | You can use this parameter to prevent unwanted multiple updates to data,
-- to control costs, or in some cases, to prevent exceeding the maximum
-- number of concurrent runs of any of the component jobs. If you leave
-- this parameter blank, there is no limit to the number of concurrent
-- workflow runs.
updateWorkflow_maxConcurrentRuns :: Lens.Lens' UpdateWorkflow (Prelude.Maybe Prelude.Int)
updateWorkflow_maxConcurrentRuns = Lens.lens (\UpdateWorkflow' {maxConcurrentRuns} -> maxConcurrentRuns) (\s@UpdateWorkflow' {} a -> s {maxConcurrentRuns = a} :: UpdateWorkflow)

-- | A collection of properties to be used as part of each execution of the
-- workflow.
updateWorkflow_defaultRunProperties :: Lens.Lens' UpdateWorkflow (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateWorkflow_defaultRunProperties = Lens.lens (\UpdateWorkflow' {defaultRunProperties} -> defaultRunProperties) (\s@UpdateWorkflow' {} a -> s {defaultRunProperties = a} :: UpdateWorkflow) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateWorkflowResponse'
            Prelude.<$> (x Data..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateWorkflow where
  hashWithSalt _salt UpdateWorkflow' {..} =
    _salt `Prelude.hashWithSalt` maxConcurrentRuns
      `Prelude.hashWithSalt` defaultRunProperties
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateWorkflow where
  rnf UpdateWorkflow' {..} =
    Prelude.rnf maxConcurrentRuns
      `Prelude.seq` Prelude.rnf defaultRunProperties
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdateWorkflow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.UpdateWorkflow" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateWorkflow where
  toJSON UpdateWorkflow' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxConcurrentRuns" Data..=)
              Prelude.<$> maxConcurrentRuns,
            ("DefaultRunProperties" Data..=)
              Prelude.<$> defaultRunProperties,
            ("Description" Data..=) Prelude.<$> description,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath UpdateWorkflow where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateWorkflow where
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

instance Prelude.NFData UpdateWorkflowResponse where
  rnf UpdateWorkflowResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf httpStatus
