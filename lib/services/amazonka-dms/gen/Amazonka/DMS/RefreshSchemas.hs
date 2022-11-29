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
-- Module      : Amazonka.DMS.RefreshSchemas
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Populates the schema for the specified endpoint. This is an asynchronous
-- operation and can take several minutes. You can check the status of this
-- operation by calling the DescribeRefreshSchemasStatus operation.
module Amazonka.DMS.RefreshSchemas
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newRefreshSchemas' smart constructor.
data RefreshSchemas = RefreshSchemas'
  { -- | The Amazon Resource Name (ARN) string that uniquely identifies the
    -- endpoint.
    endpointArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the replication instance.
    replicationInstanceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'replicationInstanceArn'
  Prelude.Text ->
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
refreshSchemas_endpointArn :: Lens.Lens' RefreshSchemas Prelude.Text
refreshSchemas_endpointArn = Lens.lens (\RefreshSchemas' {endpointArn} -> endpointArn) (\s@RefreshSchemas' {} a -> s {endpointArn = a} :: RefreshSchemas)

-- | The Amazon Resource Name (ARN) of the replication instance.
refreshSchemas_replicationInstanceArn :: Lens.Lens' RefreshSchemas Prelude.Text
refreshSchemas_replicationInstanceArn = Lens.lens (\RefreshSchemas' {replicationInstanceArn} -> replicationInstanceArn) (\s@RefreshSchemas' {} a -> s {replicationInstanceArn = a} :: RefreshSchemas)

instance Core.AWSRequest RefreshSchemas where
  type
    AWSResponse RefreshSchemas =
      RefreshSchemasResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RefreshSchemasResponse'
            Prelude.<$> (x Core..?> "RefreshSchemasStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RefreshSchemas where
  hashWithSalt _salt RefreshSchemas' {..} =
    _salt `Prelude.hashWithSalt` endpointArn
      `Prelude.hashWithSalt` replicationInstanceArn

instance Prelude.NFData RefreshSchemas where
  rnf RefreshSchemas' {..} =
    Prelude.rnf endpointArn
      `Prelude.seq` Prelude.rnf replicationInstanceArn

instance Core.ToHeaders RefreshSchemas where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.RefreshSchemas" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RefreshSchemas where
  toJSON RefreshSchemas' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("EndpointArn" Core..= endpointArn),
            Prelude.Just
              ( "ReplicationInstanceArn"
                  Core..= replicationInstanceArn
              )
          ]
      )

instance Core.ToPath RefreshSchemas where
  toPath = Prelude.const "/"

instance Core.ToQuery RefreshSchemas where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newRefreshSchemasResponse' smart constructor.
data RefreshSchemasResponse = RefreshSchemasResponse'
  { -- | The status of the refreshed schema.
    refreshSchemasStatus :: Prelude.Maybe RefreshSchemasStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  RefreshSchemasResponse
newRefreshSchemasResponse pHttpStatus_ =
  RefreshSchemasResponse'
    { refreshSchemasStatus =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the refreshed schema.
refreshSchemasResponse_refreshSchemasStatus :: Lens.Lens' RefreshSchemasResponse (Prelude.Maybe RefreshSchemasStatus)
refreshSchemasResponse_refreshSchemasStatus = Lens.lens (\RefreshSchemasResponse' {refreshSchemasStatus} -> refreshSchemasStatus) (\s@RefreshSchemasResponse' {} a -> s {refreshSchemasStatus = a} :: RefreshSchemasResponse)

-- | The response's http status code.
refreshSchemasResponse_httpStatus :: Lens.Lens' RefreshSchemasResponse Prelude.Int
refreshSchemasResponse_httpStatus = Lens.lens (\RefreshSchemasResponse' {httpStatus} -> httpStatus) (\s@RefreshSchemasResponse' {} a -> s {httpStatus = a} :: RefreshSchemasResponse)

instance Prelude.NFData RefreshSchemasResponse where
  rnf RefreshSchemasResponse' {..} =
    Prelude.rnf refreshSchemasStatus
      `Prelude.seq` Prelude.rnf httpStatus
