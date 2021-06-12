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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchGetDevEndpoints' smart constructor.
data BatchGetDevEndpoints = BatchGetDevEndpoints'
  { -- | The list of @DevEndpoint@ names, which might be the names returned from
    -- the @ListDevEndpoint@ operation.
    devEndpointNames :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.NonEmpty Core.Text ->
  BatchGetDevEndpoints
newBatchGetDevEndpoints pDevEndpointNames_ =
  BatchGetDevEndpoints'
    { devEndpointNames =
        Lens._Coerce Lens.# pDevEndpointNames_
    }

-- | The list of @DevEndpoint@ names, which might be the names returned from
-- the @ListDevEndpoint@ operation.
batchGetDevEndpoints_devEndpointNames :: Lens.Lens' BatchGetDevEndpoints (Core.NonEmpty Core.Text)
batchGetDevEndpoints_devEndpointNames = Lens.lens (\BatchGetDevEndpoints' {devEndpointNames} -> devEndpointNames) (\s@BatchGetDevEndpoints' {} a -> s {devEndpointNames = a} :: BatchGetDevEndpoints) Core.. Lens._Coerce

instance Core.AWSRequest BatchGetDevEndpoints where
  type
    AWSResponse BatchGetDevEndpoints =
      BatchGetDevEndpointsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetDevEndpointsResponse'
            Core.<$> (x Core..?> "DevEndpoints" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "DevEndpointsNotFound")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BatchGetDevEndpoints

instance Core.NFData BatchGetDevEndpoints

instance Core.ToHeaders BatchGetDevEndpoints where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.BatchGetDevEndpoints" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON BatchGetDevEndpoints where
  toJSON BatchGetDevEndpoints' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("DevEndpointNames" Core..= devEndpointNames)
          ]
      )

instance Core.ToPath BatchGetDevEndpoints where
  toPath = Core.const "/"

instance Core.ToQuery BatchGetDevEndpoints where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newBatchGetDevEndpointsResponse' smart constructor.
data BatchGetDevEndpointsResponse = BatchGetDevEndpointsResponse'
  { -- | A list of @DevEndpoint@ definitions.
    devEndpoints :: Core.Maybe [DevEndpoint],
    -- | A list of @DevEndpoints@ not found.
    devEndpointsNotFound :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  BatchGetDevEndpointsResponse
newBatchGetDevEndpointsResponse pHttpStatus_ =
  BatchGetDevEndpointsResponse'
    { devEndpoints =
        Core.Nothing,
      devEndpointsNotFound = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @DevEndpoint@ definitions.
batchGetDevEndpointsResponse_devEndpoints :: Lens.Lens' BatchGetDevEndpointsResponse (Core.Maybe [DevEndpoint])
batchGetDevEndpointsResponse_devEndpoints = Lens.lens (\BatchGetDevEndpointsResponse' {devEndpoints} -> devEndpoints) (\s@BatchGetDevEndpointsResponse' {} a -> s {devEndpoints = a} :: BatchGetDevEndpointsResponse) Core.. Lens.mapping Lens._Coerce

-- | A list of @DevEndpoints@ not found.
batchGetDevEndpointsResponse_devEndpointsNotFound :: Lens.Lens' BatchGetDevEndpointsResponse (Core.Maybe (Core.NonEmpty Core.Text))
batchGetDevEndpointsResponse_devEndpointsNotFound = Lens.lens (\BatchGetDevEndpointsResponse' {devEndpointsNotFound} -> devEndpointsNotFound) (\s@BatchGetDevEndpointsResponse' {} a -> s {devEndpointsNotFound = a} :: BatchGetDevEndpointsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchGetDevEndpointsResponse_httpStatus :: Lens.Lens' BatchGetDevEndpointsResponse Core.Int
batchGetDevEndpointsResponse_httpStatus = Lens.lens (\BatchGetDevEndpointsResponse' {httpStatus} -> httpStatus) (\s@BatchGetDevEndpointsResponse' {} a -> s {httpStatus = a} :: BatchGetDevEndpointsResponse)

instance Core.NFData BatchGetDevEndpointsResponse
