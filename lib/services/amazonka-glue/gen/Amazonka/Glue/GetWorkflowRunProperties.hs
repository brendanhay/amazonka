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
-- Module      : Amazonka.Glue.GetWorkflowRunProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the workflow run properties which were set during the run.
module Amazonka.Glue.GetWorkflowRunProperties
  ( -- * Creating a Request
    GetWorkflowRunProperties (..),
    newGetWorkflowRunProperties,

    -- * Request Lenses
    getWorkflowRunProperties_name,
    getWorkflowRunProperties_runId,

    -- * Destructuring the Response
    GetWorkflowRunPropertiesResponse (..),
    newGetWorkflowRunPropertiesResponse,

    -- * Response Lenses
    getWorkflowRunPropertiesResponse_runProperties,
    getWorkflowRunPropertiesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetWorkflowRunProperties' smart constructor.
data GetWorkflowRunProperties = GetWorkflowRunProperties'
  { -- | Name of the workflow which was run.
    name :: Prelude.Text,
    -- | The ID of the workflow run whose run properties should be returned.
    runId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkflowRunProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getWorkflowRunProperties_name' - Name of the workflow which was run.
--
-- 'runId', 'getWorkflowRunProperties_runId' - The ID of the workflow run whose run properties should be returned.
newGetWorkflowRunProperties ::
  -- | 'name'
  Prelude.Text ->
  -- | 'runId'
  Prelude.Text ->
  GetWorkflowRunProperties
newGetWorkflowRunProperties pName_ pRunId_ =
  GetWorkflowRunProperties'
    { name = pName_,
      runId = pRunId_
    }

-- | Name of the workflow which was run.
getWorkflowRunProperties_name :: Lens.Lens' GetWorkflowRunProperties Prelude.Text
getWorkflowRunProperties_name = Lens.lens (\GetWorkflowRunProperties' {name} -> name) (\s@GetWorkflowRunProperties' {} a -> s {name = a} :: GetWorkflowRunProperties)

-- | The ID of the workflow run whose run properties should be returned.
getWorkflowRunProperties_runId :: Lens.Lens' GetWorkflowRunProperties Prelude.Text
getWorkflowRunProperties_runId = Lens.lens (\GetWorkflowRunProperties' {runId} -> runId) (\s@GetWorkflowRunProperties' {} a -> s {runId = a} :: GetWorkflowRunProperties)

instance Core.AWSRequest GetWorkflowRunProperties where
  type
    AWSResponse GetWorkflowRunProperties =
      GetWorkflowRunPropertiesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWorkflowRunPropertiesResponse'
            Prelude.<$> (x Data..?> "RunProperties" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetWorkflowRunProperties where
  hashWithSalt _salt GetWorkflowRunProperties' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` runId

instance Prelude.NFData GetWorkflowRunProperties where
  rnf GetWorkflowRunProperties' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf runId

instance Data.ToHeaders GetWorkflowRunProperties where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.GetWorkflowRunProperties" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetWorkflowRunProperties where
  toJSON GetWorkflowRunProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("RunId" Data..= runId)
          ]
      )

instance Data.ToPath GetWorkflowRunProperties where
  toPath = Prelude.const "/"

instance Data.ToQuery GetWorkflowRunProperties where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetWorkflowRunPropertiesResponse' smart constructor.
data GetWorkflowRunPropertiesResponse = GetWorkflowRunPropertiesResponse'
  { -- | The workflow run properties which were set during the specified run.
    runProperties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkflowRunPropertiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runProperties', 'getWorkflowRunPropertiesResponse_runProperties' - The workflow run properties which were set during the specified run.
--
-- 'httpStatus', 'getWorkflowRunPropertiesResponse_httpStatus' - The response's http status code.
newGetWorkflowRunPropertiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetWorkflowRunPropertiesResponse
newGetWorkflowRunPropertiesResponse pHttpStatus_ =
  GetWorkflowRunPropertiesResponse'
    { runProperties =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The workflow run properties which were set during the specified run.
getWorkflowRunPropertiesResponse_runProperties :: Lens.Lens' GetWorkflowRunPropertiesResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getWorkflowRunPropertiesResponse_runProperties = Lens.lens (\GetWorkflowRunPropertiesResponse' {runProperties} -> runProperties) (\s@GetWorkflowRunPropertiesResponse' {} a -> s {runProperties = a} :: GetWorkflowRunPropertiesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getWorkflowRunPropertiesResponse_httpStatus :: Lens.Lens' GetWorkflowRunPropertiesResponse Prelude.Int
getWorkflowRunPropertiesResponse_httpStatus = Lens.lens (\GetWorkflowRunPropertiesResponse' {httpStatus} -> httpStatus) (\s@GetWorkflowRunPropertiesResponse' {} a -> s {httpStatus = a} :: GetWorkflowRunPropertiesResponse)

instance
  Prelude.NFData
    GetWorkflowRunPropertiesResponse
  where
  rnf GetWorkflowRunPropertiesResponse' {..} =
    Prelude.rnf runProperties `Prelude.seq`
      Prelude.rnf httpStatus
