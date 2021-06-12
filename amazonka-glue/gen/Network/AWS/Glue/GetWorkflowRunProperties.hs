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
-- Module      : Network.AWS.Glue.GetWorkflowRunProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the workflow run properties which were set during the run.
module Network.AWS.Glue.GetWorkflowRunProperties
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

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetWorkflowRunProperties' smart constructor.
data GetWorkflowRunProperties = GetWorkflowRunProperties'
  { -- | Name of the workflow which was run.
    name :: Core.Text,
    -- | The ID of the workflow run whose run properties should be returned.
    runId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'runId'
  Core.Text ->
  GetWorkflowRunProperties
newGetWorkflowRunProperties pName_ pRunId_ =
  GetWorkflowRunProperties'
    { name = pName_,
      runId = pRunId_
    }

-- | Name of the workflow which was run.
getWorkflowRunProperties_name :: Lens.Lens' GetWorkflowRunProperties Core.Text
getWorkflowRunProperties_name = Lens.lens (\GetWorkflowRunProperties' {name} -> name) (\s@GetWorkflowRunProperties' {} a -> s {name = a} :: GetWorkflowRunProperties)

-- | The ID of the workflow run whose run properties should be returned.
getWorkflowRunProperties_runId :: Lens.Lens' GetWorkflowRunProperties Core.Text
getWorkflowRunProperties_runId = Lens.lens (\GetWorkflowRunProperties' {runId} -> runId) (\s@GetWorkflowRunProperties' {} a -> s {runId = a} :: GetWorkflowRunProperties)

instance Core.AWSRequest GetWorkflowRunProperties where
  type
    AWSResponse GetWorkflowRunProperties =
      GetWorkflowRunPropertiesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWorkflowRunPropertiesResponse'
            Core.<$> (x Core..?> "RunProperties" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetWorkflowRunProperties

instance Core.NFData GetWorkflowRunProperties

instance Core.ToHeaders GetWorkflowRunProperties where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.GetWorkflowRunProperties" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetWorkflowRunProperties where
  toJSON GetWorkflowRunProperties' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("RunId" Core..= runId)
          ]
      )

instance Core.ToPath GetWorkflowRunProperties where
  toPath = Core.const "/"

instance Core.ToQuery GetWorkflowRunProperties where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetWorkflowRunPropertiesResponse' smart constructor.
data GetWorkflowRunPropertiesResponse = GetWorkflowRunPropertiesResponse'
  { -- | The workflow run properties which were set during the specified run.
    runProperties :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetWorkflowRunPropertiesResponse
newGetWorkflowRunPropertiesResponse pHttpStatus_ =
  GetWorkflowRunPropertiesResponse'
    { runProperties =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The workflow run properties which were set during the specified run.
getWorkflowRunPropertiesResponse_runProperties :: Lens.Lens' GetWorkflowRunPropertiesResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
getWorkflowRunPropertiesResponse_runProperties = Lens.lens (\GetWorkflowRunPropertiesResponse' {runProperties} -> runProperties) (\s@GetWorkflowRunPropertiesResponse' {} a -> s {runProperties = a} :: GetWorkflowRunPropertiesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getWorkflowRunPropertiesResponse_httpStatus :: Lens.Lens' GetWorkflowRunPropertiesResponse Core.Int
getWorkflowRunPropertiesResponse_httpStatus = Lens.lens (\GetWorkflowRunPropertiesResponse' {httpStatus} -> httpStatus) (\s@GetWorkflowRunPropertiesResponse' {} a -> s {httpStatus = a} :: GetWorkflowRunPropertiesResponse)

instance Core.NFData GetWorkflowRunPropertiesResponse
