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
-- Module      : Network.AWS.Glue.PutWorkflowRunProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Puts the specified workflow run properties for the given workflow run.
-- If a property already exists for the specified run, then it overrides
-- the value otherwise adds the property to existing properties.
module Network.AWS.Glue.PutWorkflowRunProperties
  ( -- * Creating a Request
    PutWorkflowRunProperties (..),
    newPutWorkflowRunProperties,

    -- * Request Lenses
    putWorkflowRunProperties_name,
    putWorkflowRunProperties_runId,
    putWorkflowRunProperties_runProperties,

    -- * Destructuring the Response
    PutWorkflowRunPropertiesResponse (..),
    newPutWorkflowRunPropertiesResponse,

    -- * Response Lenses
    putWorkflowRunPropertiesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutWorkflowRunProperties' smart constructor.
data PutWorkflowRunProperties = PutWorkflowRunProperties'
  { -- | Name of the workflow which was run.
    name :: Prelude.Text,
    -- | The ID of the workflow run for which the run properties should be
    -- updated.
    runId :: Prelude.Text,
    -- | The properties to put for the specified run.
    runProperties :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutWorkflowRunProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'putWorkflowRunProperties_name' - Name of the workflow which was run.
--
-- 'runId', 'putWorkflowRunProperties_runId' - The ID of the workflow run for which the run properties should be
-- updated.
--
-- 'runProperties', 'putWorkflowRunProperties_runProperties' - The properties to put for the specified run.
newPutWorkflowRunProperties ::
  -- | 'name'
  Prelude.Text ->
  -- | 'runId'
  Prelude.Text ->
  PutWorkflowRunProperties
newPutWorkflowRunProperties pName_ pRunId_ =
  PutWorkflowRunProperties'
    { name = pName_,
      runId = pRunId_,
      runProperties = Prelude.mempty
    }

-- | Name of the workflow which was run.
putWorkflowRunProperties_name :: Lens.Lens' PutWorkflowRunProperties Prelude.Text
putWorkflowRunProperties_name = Lens.lens (\PutWorkflowRunProperties' {name} -> name) (\s@PutWorkflowRunProperties' {} a -> s {name = a} :: PutWorkflowRunProperties)

-- | The ID of the workflow run for which the run properties should be
-- updated.
putWorkflowRunProperties_runId :: Lens.Lens' PutWorkflowRunProperties Prelude.Text
putWorkflowRunProperties_runId = Lens.lens (\PutWorkflowRunProperties' {runId} -> runId) (\s@PutWorkflowRunProperties' {} a -> s {runId = a} :: PutWorkflowRunProperties)

-- | The properties to put for the specified run.
putWorkflowRunProperties_runProperties :: Lens.Lens' PutWorkflowRunProperties (Prelude.HashMap Prelude.Text Prelude.Text)
putWorkflowRunProperties_runProperties = Lens.lens (\PutWorkflowRunProperties' {runProperties} -> runProperties) (\s@PutWorkflowRunProperties' {} a -> s {runProperties = a} :: PutWorkflowRunProperties) Prelude.. Lens._Coerce

instance Core.AWSRequest PutWorkflowRunProperties where
  type
    AWSResponse PutWorkflowRunProperties =
      PutWorkflowRunPropertiesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutWorkflowRunPropertiesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutWorkflowRunProperties

instance Prelude.NFData PutWorkflowRunProperties

instance Core.ToHeaders PutWorkflowRunProperties where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.PutWorkflowRunProperties" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutWorkflowRunProperties where
  toJSON PutWorkflowRunProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Core..= name),
            Prelude.Just ("RunId" Core..= runId),
            Prelude.Just
              ("RunProperties" Core..= runProperties)
          ]
      )

instance Core.ToPath PutWorkflowRunProperties where
  toPath = Prelude.const "/"

instance Core.ToQuery PutWorkflowRunProperties where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutWorkflowRunPropertiesResponse' smart constructor.
data PutWorkflowRunPropertiesResponse = PutWorkflowRunPropertiesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutWorkflowRunPropertiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putWorkflowRunPropertiesResponse_httpStatus' - The response's http status code.
newPutWorkflowRunPropertiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutWorkflowRunPropertiesResponse
newPutWorkflowRunPropertiesResponse pHttpStatus_ =
  PutWorkflowRunPropertiesResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putWorkflowRunPropertiesResponse_httpStatus :: Lens.Lens' PutWorkflowRunPropertiesResponse Prelude.Int
putWorkflowRunPropertiesResponse_httpStatus = Lens.lens (\PutWorkflowRunPropertiesResponse' {httpStatus} -> httpStatus) (\s@PutWorkflowRunPropertiesResponse' {} a -> s {httpStatus = a} :: PutWorkflowRunPropertiesResponse)

instance
  Prelude.NFData
    PutWorkflowRunPropertiesResponse
