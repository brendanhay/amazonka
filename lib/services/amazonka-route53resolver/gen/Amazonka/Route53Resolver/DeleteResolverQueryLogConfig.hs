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
-- Module      : Amazonka.Route53Resolver.DeleteResolverQueryLogConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a query logging configuration. When you delete a configuration,
-- Resolver stops logging DNS queries for all of the Amazon VPCs that are
-- associated with the configuration. This also applies if the query
-- logging configuration is shared with other Amazon Web Services accounts,
-- and the other accounts have associated VPCs with the shared
-- configuration.
--
-- Before you can delete a query logging configuration, you must first
-- disassociate all VPCs from the configuration. See
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_DisassociateResolverQueryLogConfig.html DisassociateResolverQueryLogConfig>.
--
-- If you used Resource Access Manager (RAM) to share a query logging
-- configuration with other accounts, you must stop sharing the
-- configuration before you can delete a configuration. The accounts that
-- you shared the configuration with can first disassociate VPCs that they
-- associated with the configuration, but that\'s not necessary. If you
-- stop sharing the configuration, those VPCs are automatically
-- disassociated from the configuration.
module Amazonka.Route53Resolver.DeleteResolverQueryLogConfig
  ( -- * Creating a Request
    DeleteResolverQueryLogConfig (..),
    newDeleteResolverQueryLogConfig,

    -- * Request Lenses
    deleteResolverQueryLogConfig_resolverQueryLogConfigId,

    -- * Destructuring the Response
    DeleteResolverQueryLogConfigResponse (..),
    newDeleteResolverQueryLogConfigResponse,

    -- * Response Lenses
    deleteResolverQueryLogConfigResponse_resolverQueryLogConfig,
    deleteResolverQueryLogConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newDeleteResolverQueryLogConfig' smart constructor.
data DeleteResolverQueryLogConfig = DeleteResolverQueryLogConfig'
  { -- | The ID of the query logging configuration that you want to delete.
    resolverQueryLogConfigId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResolverQueryLogConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolverQueryLogConfigId', 'deleteResolverQueryLogConfig_resolverQueryLogConfigId' - The ID of the query logging configuration that you want to delete.
newDeleteResolverQueryLogConfig ::
  -- | 'resolverQueryLogConfigId'
  Prelude.Text ->
  DeleteResolverQueryLogConfig
newDeleteResolverQueryLogConfig
  pResolverQueryLogConfigId_ =
    DeleteResolverQueryLogConfig'
      { resolverQueryLogConfigId =
          pResolverQueryLogConfigId_
      }

-- | The ID of the query logging configuration that you want to delete.
deleteResolverQueryLogConfig_resolverQueryLogConfigId :: Lens.Lens' DeleteResolverQueryLogConfig Prelude.Text
deleteResolverQueryLogConfig_resolverQueryLogConfigId = Lens.lens (\DeleteResolverQueryLogConfig' {resolverQueryLogConfigId} -> resolverQueryLogConfigId) (\s@DeleteResolverQueryLogConfig' {} a -> s {resolverQueryLogConfigId = a} :: DeleteResolverQueryLogConfig)

instance Core.AWSRequest DeleteResolverQueryLogConfig where
  type
    AWSResponse DeleteResolverQueryLogConfig =
      DeleteResolverQueryLogConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteResolverQueryLogConfigResponse'
            Prelude.<$> (x Data..?> "ResolverQueryLogConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteResolverQueryLogConfig
  where
  hashWithSalt _salt DeleteResolverQueryLogConfig' {..} =
    _salt
      `Prelude.hashWithSalt` resolverQueryLogConfigId

instance Prelude.NFData DeleteResolverQueryLogConfig where
  rnf DeleteResolverQueryLogConfig' {..} =
    Prelude.rnf resolverQueryLogConfigId

instance Data.ToHeaders DeleteResolverQueryLogConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.DeleteResolverQueryLogConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteResolverQueryLogConfig where
  toJSON DeleteResolverQueryLogConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ResolverQueryLogConfigId"
                  Data..= resolverQueryLogConfigId
              )
          ]
      )

instance Data.ToPath DeleteResolverQueryLogConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteResolverQueryLogConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteResolverQueryLogConfigResponse' smart constructor.
data DeleteResolverQueryLogConfigResponse = DeleteResolverQueryLogConfigResponse'
  { -- | Information about the query logging configuration that you deleted,
    -- including the status of the request.
    resolverQueryLogConfig :: Prelude.Maybe ResolverQueryLogConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResolverQueryLogConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolverQueryLogConfig', 'deleteResolverQueryLogConfigResponse_resolverQueryLogConfig' - Information about the query logging configuration that you deleted,
-- including the status of the request.
--
-- 'httpStatus', 'deleteResolverQueryLogConfigResponse_httpStatus' - The response's http status code.
newDeleteResolverQueryLogConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteResolverQueryLogConfigResponse
newDeleteResolverQueryLogConfigResponse pHttpStatus_ =
  DeleteResolverQueryLogConfigResponse'
    { resolverQueryLogConfig =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the query logging configuration that you deleted,
-- including the status of the request.
deleteResolverQueryLogConfigResponse_resolverQueryLogConfig :: Lens.Lens' DeleteResolverQueryLogConfigResponse (Prelude.Maybe ResolverQueryLogConfig)
deleteResolverQueryLogConfigResponse_resolverQueryLogConfig = Lens.lens (\DeleteResolverQueryLogConfigResponse' {resolverQueryLogConfig} -> resolverQueryLogConfig) (\s@DeleteResolverQueryLogConfigResponse' {} a -> s {resolverQueryLogConfig = a} :: DeleteResolverQueryLogConfigResponse)

-- | The response's http status code.
deleteResolverQueryLogConfigResponse_httpStatus :: Lens.Lens' DeleteResolverQueryLogConfigResponse Prelude.Int
deleteResolverQueryLogConfigResponse_httpStatus = Lens.lens (\DeleteResolverQueryLogConfigResponse' {httpStatus} -> httpStatus) (\s@DeleteResolverQueryLogConfigResponse' {} a -> s {httpStatus = a} :: DeleteResolverQueryLogConfigResponse)

instance
  Prelude.NFData
    DeleteResolverQueryLogConfigResponse
  where
  rnf DeleteResolverQueryLogConfigResponse' {..} =
    Prelude.rnf resolverQueryLogConfig `Prelude.seq`
      Prelude.rnf httpStatus
