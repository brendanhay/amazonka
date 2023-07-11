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
-- Module      : Amazonka.Omics.UpdateWorkflow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a workflow.
module Amazonka.Omics.UpdateWorkflow
  ( -- * Creating a Request
    UpdateWorkflow (..),
    newUpdateWorkflow,

    -- * Request Lenses
    updateWorkflow_description,
    updateWorkflow_name,
    updateWorkflow_id,

    -- * Destructuring the Response
    UpdateWorkflowResponse (..),
    newUpdateWorkflowResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateWorkflow' smart constructor.
data UpdateWorkflow = UpdateWorkflow'
  { -- | A description for the workflow.
    description :: Prelude.Maybe Prelude.Text,
    -- | A name for the workflow.
    name :: Prelude.Maybe Prelude.Text,
    -- | The workflow\'s ID.
    id :: Prelude.Text
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
-- 'description', 'updateWorkflow_description' - A description for the workflow.
--
-- 'name', 'updateWorkflow_name' - A name for the workflow.
--
-- 'id', 'updateWorkflow_id' - The workflow\'s ID.
newUpdateWorkflow ::
  -- | 'id'
  Prelude.Text ->
  UpdateWorkflow
newUpdateWorkflow pId_ =
  UpdateWorkflow'
    { description = Prelude.Nothing,
      name = Prelude.Nothing,
      id = pId_
    }

-- | A description for the workflow.
updateWorkflow_description :: Lens.Lens' UpdateWorkflow (Prelude.Maybe Prelude.Text)
updateWorkflow_description = Lens.lens (\UpdateWorkflow' {description} -> description) (\s@UpdateWorkflow' {} a -> s {description = a} :: UpdateWorkflow)

-- | A name for the workflow.
updateWorkflow_name :: Lens.Lens' UpdateWorkflow (Prelude.Maybe Prelude.Text)
updateWorkflow_name = Lens.lens (\UpdateWorkflow' {name} -> name) (\s@UpdateWorkflow' {} a -> s {name = a} :: UpdateWorkflow)

-- | The workflow\'s ID.
updateWorkflow_id :: Lens.Lens' UpdateWorkflow Prelude.Text
updateWorkflow_id = Lens.lens (\UpdateWorkflow' {id} -> id) (\s@UpdateWorkflow' {} a -> s {id = a} :: UpdateWorkflow)

instance Core.AWSRequest UpdateWorkflow where
  type
    AWSResponse UpdateWorkflow =
      UpdateWorkflowResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateWorkflowResponse'

instance Prelude.Hashable UpdateWorkflow where
  hashWithSalt _salt UpdateWorkflow' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateWorkflow where
  rnf UpdateWorkflow' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders UpdateWorkflow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateWorkflow where
  toJSON UpdateWorkflow' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("name" Data..=) Prelude.<$> name
          ]
      )

instance Data.ToPath UpdateWorkflow where
  toPath UpdateWorkflow' {..} =
    Prelude.mconcat ["/workflow/", Data.toBS id]

instance Data.ToQuery UpdateWorkflow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateWorkflowResponse' smart constructor.
data UpdateWorkflowResponse = UpdateWorkflowResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkflowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateWorkflowResponse ::
  UpdateWorkflowResponse
newUpdateWorkflowResponse = UpdateWorkflowResponse'

instance Prelude.NFData UpdateWorkflowResponse where
  rnf _ = ()
