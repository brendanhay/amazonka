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
-- Module      : Amazonka.Glue.PutWorkflowRunProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Puts the specified workflow run properties for the given workflow run.
-- If a property already exists for the specified run, then it overrides
-- the value otherwise adds the property to existing properties.
module Amazonka.Glue.PutWorkflowRunProperties
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
putWorkflowRunProperties_runProperties = Lens.lens (\PutWorkflowRunProperties' {runProperties} -> runProperties) (\s@PutWorkflowRunProperties' {} a -> s {runProperties = a} :: PutWorkflowRunProperties) Prelude.. Lens.coerced

instance Core.AWSRequest PutWorkflowRunProperties where
  type
    AWSResponse PutWorkflowRunProperties =
      PutWorkflowRunPropertiesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutWorkflowRunPropertiesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutWorkflowRunProperties where
  hashWithSalt _salt PutWorkflowRunProperties' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` runId
      `Prelude.hashWithSalt` runProperties

instance Prelude.NFData PutWorkflowRunProperties where
  rnf PutWorkflowRunProperties' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf runId
      `Prelude.seq` Prelude.rnf runProperties

instance Data.ToHeaders PutWorkflowRunProperties where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.PutWorkflowRunProperties" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutWorkflowRunProperties where
  toJSON PutWorkflowRunProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("RunId" Data..= runId),
            Prelude.Just
              ("RunProperties" Data..= runProperties)
          ]
      )

instance Data.ToPath PutWorkflowRunProperties where
  toPath = Prelude.const "/"

instance Data.ToQuery PutWorkflowRunProperties where
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
  where
  rnf PutWorkflowRunPropertiesResponse' {..} =
    Prelude.rnf httpStatus
