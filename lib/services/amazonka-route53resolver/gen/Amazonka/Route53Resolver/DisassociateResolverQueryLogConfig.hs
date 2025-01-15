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
-- Module      : Amazonka.Route53Resolver.DisassociateResolverQueryLogConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a VPC from a query logging configuration.
--
-- Before you can delete a query logging configuration, you must first
-- disassociate all VPCs from the configuration. If you used Resource
-- Access Manager (RAM) to share a query logging configuration with other
-- accounts, VPCs can be disassociated from the configuration in the
-- following ways:
--
-- -   The accounts that you shared the configuration with can disassociate
--     VPCs from the configuration.
--
-- -   You can stop sharing the configuration.
module Amazonka.Route53Resolver.DisassociateResolverQueryLogConfig
  ( -- * Creating a Request
    DisassociateResolverQueryLogConfig (..),
    newDisassociateResolverQueryLogConfig,

    -- * Request Lenses
    disassociateResolverQueryLogConfig_resolverQueryLogConfigId,
    disassociateResolverQueryLogConfig_resourceId,

    -- * Destructuring the Response
    DisassociateResolverQueryLogConfigResponse (..),
    newDisassociateResolverQueryLogConfigResponse,

    -- * Response Lenses
    disassociateResolverQueryLogConfigResponse_resolverQueryLogConfigAssociation,
    disassociateResolverQueryLogConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newDisassociateResolverQueryLogConfig' smart constructor.
data DisassociateResolverQueryLogConfig = DisassociateResolverQueryLogConfig'
  { -- | The ID of the query logging configuration that you want to disassociate
    -- a specified VPC from.
    resolverQueryLogConfigId :: Prelude.Text,
    -- | The ID of the Amazon VPC that you want to disassociate from a specified
    -- query logging configuration.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateResolverQueryLogConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolverQueryLogConfigId', 'disassociateResolverQueryLogConfig_resolverQueryLogConfigId' - The ID of the query logging configuration that you want to disassociate
-- a specified VPC from.
--
-- 'resourceId', 'disassociateResolverQueryLogConfig_resourceId' - The ID of the Amazon VPC that you want to disassociate from a specified
-- query logging configuration.
newDisassociateResolverQueryLogConfig ::
  -- | 'resolverQueryLogConfigId'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  DisassociateResolverQueryLogConfig
newDisassociateResolverQueryLogConfig
  pResolverQueryLogConfigId_
  pResourceId_ =
    DisassociateResolverQueryLogConfig'
      { resolverQueryLogConfigId =
          pResolverQueryLogConfigId_,
        resourceId = pResourceId_
      }

-- | The ID of the query logging configuration that you want to disassociate
-- a specified VPC from.
disassociateResolverQueryLogConfig_resolverQueryLogConfigId :: Lens.Lens' DisassociateResolverQueryLogConfig Prelude.Text
disassociateResolverQueryLogConfig_resolverQueryLogConfigId = Lens.lens (\DisassociateResolverQueryLogConfig' {resolverQueryLogConfigId} -> resolverQueryLogConfigId) (\s@DisassociateResolverQueryLogConfig' {} a -> s {resolverQueryLogConfigId = a} :: DisassociateResolverQueryLogConfig)

-- | The ID of the Amazon VPC that you want to disassociate from a specified
-- query logging configuration.
disassociateResolverQueryLogConfig_resourceId :: Lens.Lens' DisassociateResolverQueryLogConfig Prelude.Text
disassociateResolverQueryLogConfig_resourceId = Lens.lens (\DisassociateResolverQueryLogConfig' {resourceId} -> resourceId) (\s@DisassociateResolverQueryLogConfig' {} a -> s {resourceId = a} :: DisassociateResolverQueryLogConfig)

instance
  Core.AWSRequest
    DisassociateResolverQueryLogConfig
  where
  type
    AWSResponse DisassociateResolverQueryLogConfig =
      DisassociateResolverQueryLogConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateResolverQueryLogConfigResponse'
            Prelude.<$> (x Data..?> "ResolverQueryLogConfigAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateResolverQueryLogConfig
  where
  hashWithSalt
    _salt
    DisassociateResolverQueryLogConfig' {..} =
      _salt
        `Prelude.hashWithSalt` resolverQueryLogConfigId
        `Prelude.hashWithSalt` resourceId

instance
  Prelude.NFData
    DisassociateResolverQueryLogConfig
  where
  rnf DisassociateResolverQueryLogConfig' {..} =
    Prelude.rnf resolverQueryLogConfigId `Prelude.seq`
      Prelude.rnf resourceId

instance
  Data.ToHeaders
    DisassociateResolverQueryLogConfig
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.DisassociateResolverQueryLogConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DisassociateResolverQueryLogConfig
  where
  toJSON DisassociateResolverQueryLogConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ResolverQueryLogConfigId"
                  Data..= resolverQueryLogConfigId
              ),
            Prelude.Just ("ResourceId" Data..= resourceId)
          ]
      )

instance
  Data.ToPath
    DisassociateResolverQueryLogConfig
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DisassociateResolverQueryLogConfig
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateResolverQueryLogConfigResponse' smart constructor.
data DisassociateResolverQueryLogConfigResponse = DisassociateResolverQueryLogConfigResponse'
  { -- | A complex type that contains settings for the association that you
    -- deleted between an Amazon VPC and a query logging configuration.
    resolverQueryLogConfigAssociation :: Prelude.Maybe ResolverQueryLogConfigAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateResolverQueryLogConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolverQueryLogConfigAssociation', 'disassociateResolverQueryLogConfigResponse_resolverQueryLogConfigAssociation' - A complex type that contains settings for the association that you
-- deleted between an Amazon VPC and a query logging configuration.
--
-- 'httpStatus', 'disassociateResolverQueryLogConfigResponse_httpStatus' - The response's http status code.
newDisassociateResolverQueryLogConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateResolverQueryLogConfigResponse
newDisassociateResolverQueryLogConfigResponse
  pHttpStatus_ =
    DisassociateResolverQueryLogConfigResponse'
      { resolverQueryLogConfigAssociation =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A complex type that contains settings for the association that you
-- deleted between an Amazon VPC and a query logging configuration.
disassociateResolverQueryLogConfigResponse_resolverQueryLogConfigAssociation :: Lens.Lens' DisassociateResolverQueryLogConfigResponse (Prelude.Maybe ResolverQueryLogConfigAssociation)
disassociateResolverQueryLogConfigResponse_resolverQueryLogConfigAssociation = Lens.lens (\DisassociateResolverQueryLogConfigResponse' {resolverQueryLogConfigAssociation} -> resolverQueryLogConfigAssociation) (\s@DisassociateResolverQueryLogConfigResponse' {} a -> s {resolverQueryLogConfigAssociation = a} :: DisassociateResolverQueryLogConfigResponse)

-- | The response's http status code.
disassociateResolverQueryLogConfigResponse_httpStatus :: Lens.Lens' DisassociateResolverQueryLogConfigResponse Prelude.Int
disassociateResolverQueryLogConfigResponse_httpStatus = Lens.lens (\DisassociateResolverQueryLogConfigResponse' {httpStatus} -> httpStatus) (\s@DisassociateResolverQueryLogConfigResponse' {} a -> s {httpStatus = a} :: DisassociateResolverQueryLogConfigResponse)

instance
  Prelude.NFData
    DisassociateResolverQueryLogConfigResponse
  where
  rnf DisassociateResolverQueryLogConfigResponse' {..} =
    Prelude.rnf resolverQueryLogConfigAssociation `Prelude.seq`
      Prelude.rnf httpStatus
