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
-- Module      : Network.AWS.DMS.RefreshSchemas
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Populates the schema for the specified endpoint. This is an asynchronous
-- operation and can take several minutes. You can check the status of this
-- operation by calling the DescribeRefreshSchemasStatus operation.
module Network.AWS.DMS.RefreshSchemas
  ( -- * Creating a Request
    RefreshSchemas (..),
    newRefreshSchemas,

    -- * Request Lenses
    refreshSchemas_endpointArn,
    refreshSchemas_replicationInstanceArn,

    -- * Destructuring the Response
    RefreshSchemasResponse (..),
    newRefreshSchemasResponse,

    -- * Response Lenses
    refreshSchemasResponse_refreshSchemasStatus,
    refreshSchemasResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newRefreshSchemas' smart constructor.
data RefreshSchemas = RefreshSchemas'
  { -- | The Amazon Resource Name (ARN) string that uniquely identifies the
    -- endpoint.
    endpointArn :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the replication instance.
    replicationInstanceArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RefreshSchemas' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointArn', 'refreshSchemas_endpointArn' - The Amazon Resource Name (ARN) string that uniquely identifies the
-- endpoint.
--
-- 'replicationInstanceArn', 'refreshSchemas_replicationInstanceArn' - The Amazon Resource Name (ARN) of the replication instance.
newRefreshSchemas ::
  -- | 'endpointArn'
  Core.Text ->
  -- | 'replicationInstanceArn'
  Core.Text ->
  RefreshSchemas
newRefreshSchemas
  pEndpointArn_
  pReplicationInstanceArn_ =
    RefreshSchemas'
      { endpointArn = pEndpointArn_,
        replicationInstanceArn = pReplicationInstanceArn_
      }

-- | The Amazon Resource Name (ARN) string that uniquely identifies the
-- endpoint.
refreshSchemas_endpointArn :: Lens.Lens' RefreshSchemas Core.Text
refreshSchemas_endpointArn = Lens.lens (\RefreshSchemas' {endpointArn} -> endpointArn) (\s@RefreshSchemas' {} a -> s {endpointArn = a} :: RefreshSchemas)

-- | The Amazon Resource Name (ARN) of the replication instance.
refreshSchemas_replicationInstanceArn :: Lens.Lens' RefreshSchemas Core.Text
refreshSchemas_replicationInstanceArn = Lens.lens (\RefreshSchemas' {replicationInstanceArn} -> replicationInstanceArn) (\s@RefreshSchemas' {} a -> s {replicationInstanceArn = a} :: RefreshSchemas)

instance Core.AWSRequest RefreshSchemas where
  type
    AWSResponse RefreshSchemas =
      RefreshSchemasResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RefreshSchemasResponse'
            Core.<$> (x Core..?> "RefreshSchemasStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RefreshSchemas

instance Core.NFData RefreshSchemas

instance Core.ToHeaders RefreshSchemas where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.RefreshSchemas" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RefreshSchemas where
  toJSON RefreshSchemas' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("EndpointArn" Core..= endpointArn),
            Core.Just
              ( "ReplicationInstanceArn"
                  Core..= replicationInstanceArn
              )
          ]
      )

instance Core.ToPath RefreshSchemas where
  toPath = Core.const "/"

instance Core.ToQuery RefreshSchemas where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newRefreshSchemasResponse' smart constructor.
data RefreshSchemasResponse = RefreshSchemasResponse'
  { -- | The status of the refreshed schema.
    refreshSchemasStatus :: Core.Maybe RefreshSchemasStatus,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RefreshSchemasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'refreshSchemasStatus', 'refreshSchemasResponse_refreshSchemasStatus' - The status of the refreshed schema.
--
-- 'httpStatus', 'refreshSchemasResponse_httpStatus' - The response's http status code.
newRefreshSchemasResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RefreshSchemasResponse
newRefreshSchemasResponse pHttpStatus_ =
  RefreshSchemasResponse'
    { refreshSchemasStatus =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the refreshed schema.
refreshSchemasResponse_refreshSchemasStatus :: Lens.Lens' RefreshSchemasResponse (Core.Maybe RefreshSchemasStatus)
refreshSchemasResponse_refreshSchemasStatus = Lens.lens (\RefreshSchemasResponse' {refreshSchemasStatus} -> refreshSchemasStatus) (\s@RefreshSchemasResponse' {} a -> s {refreshSchemasStatus = a} :: RefreshSchemasResponse)

-- | The response's http status code.
refreshSchemasResponse_httpStatus :: Lens.Lens' RefreshSchemasResponse Core.Int
refreshSchemasResponse_httpStatus = Lens.lens (\RefreshSchemasResponse' {httpStatus} -> httpStatus) (\s@RefreshSchemasResponse' {} a -> s {httpStatus = a} :: RefreshSchemasResponse)

instance Core.NFData RefreshSchemasResponse
