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
-- Module      : Network.AWS.Glue.BatchGetDevEndpoints
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of resource metadata for a given list of development
-- endpoint names. After calling the @ListDevEndpoints@ operation, you can
-- call this operation to access the data to which you have been granted
-- permissions. This operation supports all IAM permissions, including
-- permission conditions that uses tags.
module Network.AWS.Glue.BatchGetDevEndpoints
  ( -- * Creating a Request
    BatchGetDevEndpoints (..),
    newBatchGetDevEndpoints,

    -- * Request Lenses
    batchGetDevEndpoints_devEndpointNames,

    -- * Destructuring the Response
    BatchGetDevEndpointsResponse (..),
    newBatchGetDevEndpointsResponse,

    -- * Response Lenses
    batchGetDevEndpointsResponse_devEndpoints,
    batchGetDevEndpointsResponse_devEndpointsNotFound,
    batchGetDevEndpointsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchGetDevEndpoints' smart constructor.
data BatchGetDevEndpoints = BatchGetDevEndpoints'
  { -- | The list of @DevEndpoint@ names, which might be the names returned from
    -- the @ListDevEndpoint@ operation.
    devEndpointNames :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetDevEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'devEndpointNames', 'batchGetDevEndpoints_devEndpointNames' - The list of @DevEndpoint@ names, which might be the names returned from
-- the @ListDevEndpoint@ operation.
newBatchGetDevEndpoints ::
  -- | 'devEndpointNames'
  Prelude.NonEmpty Prelude.Text ->
  BatchGetDevEndpoints
newBatchGetDevEndpoints pDevEndpointNames_ =
  BatchGetDevEndpoints'
    { devEndpointNames =
        Lens._Coerce Lens.# pDevEndpointNames_
    }

-- | The list of @DevEndpoint@ names, which might be the names returned from
-- the @ListDevEndpoint@ operation.
batchGetDevEndpoints_devEndpointNames :: Lens.Lens' BatchGetDevEndpoints (Prelude.NonEmpty Prelude.Text)
batchGetDevEndpoints_devEndpointNames = Lens.lens (\BatchGetDevEndpoints' {devEndpointNames} -> devEndpointNames) (\s@BatchGetDevEndpoints' {} a -> s {devEndpointNames = a} :: BatchGetDevEndpoints) Prelude.. Lens._Coerce

instance Core.AWSRequest BatchGetDevEndpoints where
  type
    AWSResponse BatchGetDevEndpoints =
      BatchGetDevEndpointsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetDevEndpointsResponse'
            Prelude.<$> (x Core..?> "DevEndpoints" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "DevEndpointsNotFound")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetDevEndpoints

instance Prelude.NFData BatchGetDevEndpoints

instance Core.ToHeaders BatchGetDevEndpoints where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.BatchGetDevEndpoints" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchGetDevEndpoints where
  toJSON BatchGetDevEndpoints' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DevEndpointNames" Core..= devEndpointNames)
          ]
      )

instance Core.ToPath BatchGetDevEndpoints where
  toPath = Prelude.const "/"

instance Core.ToQuery BatchGetDevEndpoints where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetDevEndpointsResponse' smart constructor.
data BatchGetDevEndpointsResponse = BatchGetDevEndpointsResponse'
  { -- | A list of @DevEndpoint@ definitions.
    devEndpoints :: Prelude.Maybe [DevEndpoint],
    -- | A list of @DevEndpoints@ not found.
    devEndpointsNotFound :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetDevEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'devEndpoints', 'batchGetDevEndpointsResponse_devEndpoints' - A list of @DevEndpoint@ definitions.
--
-- 'devEndpointsNotFound', 'batchGetDevEndpointsResponse_devEndpointsNotFound' - A list of @DevEndpoints@ not found.
--
-- 'httpStatus', 'batchGetDevEndpointsResponse_httpStatus' - The response's http status code.
newBatchGetDevEndpointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetDevEndpointsResponse
newBatchGetDevEndpointsResponse pHttpStatus_ =
  BatchGetDevEndpointsResponse'
    { devEndpoints =
        Prelude.Nothing,
      devEndpointsNotFound = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @DevEndpoint@ definitions.
batchGetDevEndpointsResponse_devEndpoints :: Lens.Lens' BatchGetDevEndpointsResponse (Prelude.Maybe [DevEndpoint])
batchGetDevEndpointsResponse_devEndpoints = Lens.lens (\BatchGetDevEndpointsResponse' {devEndpoints} -> devEndpoints) (\s@BatchGetDevEndpointsResponse' {} a -> s {devEndpoints = a} :: BatchGetDevEndpointsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A list of @DevEndpoints@ not found.
batchGetDevEndpointsResponse_devEndpointsNotFound :: Lens.Lens' BatchGetDevEndpointsResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
batchGetDevEndpointsResponse_devEndpointsNotFound = Lens.lens (\BatchGetDevEndpointsResponse' {devEndpointsNotFound} -> devEndpointsNotFound) (\s@BatchGetDevEndpointsResponse' {} a -> s {devEndpointsNotFound = a} :: BatchGetDevEndpointsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchGetDevEndpointsResponse_httpStatus :: Lens.Lens' BatchGetDevEndpointsResponse Prelude.Int
batchGetDevEndpointsResponse_httpStatus = Lens.lens (\BatchGetDevEndpointsResponse' {httpStatus} -> httpStatus) (\s@BatchGetDevEndpointsResponse' {} a -> s {httpStatus = a} :: BatchGetDevEndpointsResponse)

instance Prelude.NFData BatchGetDevEndpointsResponse
