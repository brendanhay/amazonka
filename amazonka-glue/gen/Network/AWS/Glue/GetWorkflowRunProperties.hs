{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetWorkflowRunProperties' smart constructor.
data GetWorkflowRunProperties = GetWorkflowRunProperties'
  { -- | Name of the workflow which was run.
    name :: Prelude.Text,
    -- | The ID of the workflow run whose run properties should be returned.
    runId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest GetWorkflowRunProperties where
  type
    Rs GetWorkflowRunProperties =
      GetWorkflowRunPropertiesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWorkflowRunPropertiesResponse'
            Prelude.<$> ( x Prelude..?> "RunProperties"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetWorkflowRunProperties

instance Prelude.NFData GetWorkflowRunProperties

instance Prelude.ToHeaders GetWorkflowRunProperties where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSGlue.GetWorkflowRunProperties" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetWorkflowRunProperties where
  toJSON GetWorkflowRunProperties' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Prelude..= name),
            Prelude.Just ("RunId" Prelude..= runId)
          ]
      )

instance Prelude.ToPath GetWorkflowRunProperties where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetWorkflowRunProperties where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetWorkflowRunPropertiesResponse' smart constructor.
data GetWorkflowRunPropertiesResponse = GetWorkflowRunPropertiesResponse'
  { -- | The workflow run properties which were set during the specified run.
    runProperties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
getWorkflowRunPropertiesResponse_runProperties = Lens.lens (\GetWorkflowRunPropertiesResponse' {runProperties} -> runProperties) (\s@GetWorkflowRunPropertiesResponse' {} a -> s {runProperties = a} :: GetWorkflowRunPropertiesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
getWorkflowRunPropertiesResponse_httpStatus :: Lens.Lens' GetWorkflowRunPropertiesResponse Prelude.Int
getWorkflowRunPropertiesResponse_httpStatus = Lens.lens (\GetWorkflowRunPropertiesResponse' {httpStatus} -> httpStatus) (\s@GetWorkflowRunPropertiesResponse' {} a -> s {httpStatus = a} :: GetWorkflowRunPropertiesResponse)

instance
  Prelude.NFData
    GetWorkflowRunPropertiesResponse
