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
-- Module      : Amazonka.Route53Resolver.AssociateResolverQueryLogConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an Amazon VPC with a specified query logging configuration.
-- Route 53 Resolver logs DNS queries that originate in all of the Amazon
-- VPCs that are associated with a specified query logging configuration.
-- To associate more than one VPC with a configuration, submit one
-- @AssociateResolverQueryLogConfig@ request for each VPC.
--
-- The VPCs that you associate with a query logging configuration must be
-- in the same Region as the configuration.
--
-- To remove a VPC from a query logging configuration, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_DisassociateResolverQueryLogConfig.html DisassociateResolverQueryLogConfig>.
module Amazonka.Route53Resolver.AssociateResolverQueryLogConfig
  ( -- * Creating a Request
    AssociateResolverQueryLogConfig (..),
    newAssociateResolverQueryLogConfig,

    -- * Request Lenses
    associateResolverQueryLogConfig_resolverQueryLogConfigId,
    associateResolverQueryLogConfig_resourceId,

    -- * Destructuring the Response
    AssociateResolverQueryLogConfigResponse (..),
    newAssociateResolverQueryLogConfigResponse,

    -- * Response Lenses
    associateResolverQueryLogConfigResponse_resolverQueryLogConfigAssociation,
    associateResolverQueryLogConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newAssociateResolverQueryLogConfig' smart constructor.
data AssociateResolverQueryLogConfig = AssociateResolverQueryLogConfig'
  { -- | The ID of the query logging configuration that you want to associate a
    -- VPC with.
    resolverQueryLogConfigId :: Prelude.Text,
    -- | The ID of an Amazon VPC that you want this query logging configuration
    -- to log queries for.
    --
    -- The VPCs and the query logging configuration must be in the same Region.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateResolverQueryLogConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolverQueryLogConfigId', 'associateResolverQueryLogConfig_resolverQueryLogConfigId' - The ID of the query logging configuration that you want to associate a
-- VPC with.
--
-- 'resourceId', 'associateResolverQueryLogConfig_resourceId' - The ID of an Amazon VPC that you want this query logging configuration
-- to log queries for.
--
-- The VPCs and the query logging configuration must be in the same Region.
newAssociateResolverQueryLogConfig ::
  -- | 'resolverQueryLogConfigId'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  AssociateResolverQueryLogConfig
newAssociateResolverQueryLogConfig
  pResolverQueryLogConfigId_
  pResourceId_ =
    AssociateResolverQueryLogConfig'
      { resolverQueryLogConfigId =
          pResolverQueryLogConfigId_,
        resourceId = pResourceId_
      }

-- | The ID of the query logging configuration that you want to associate a
-- VPC with.
associateResolverQueryLogConfig_resolverQueryLogConfigId :: Lens.Lens' AssociateResolverQueryLogConfig Prelude.Text
associateResolverQueryLogConfig_resolverQueryLogConfigId = Lens.lens (\AssociateResolverQueryLogConfig' {resolverQueryLogConfigId} -> resolverQueryLogConfigId) (\s@AssociateResolverQueryLogConfig' {} a -> s {resolverQueryLogConfigId = a} :: AssociateResolverQueryLogConfig)

-- | The ID of an Amazon VPC that you want this query logging configuration
-- to log queries for.
--
-- The VPCs and the query logging configuration must be in the same Region.
associateResolverQueryLogConfig_resourceId :: Lens.Lens' AssociateResolverQueryLogConfig Prelude.Text
associateResolverQueryLogConfig_resourceId = Lens.lens (\AssociateResolverQueryLogConfig' {resourceId} -> resourceId) (\s@AssociateResolverQueryLogConfig' {} a -> s {resourceId = a} :: AssociateResolverQueryLogConfig)

instance
  Core.AWSRequest
    AssociateResolverQueryLogConfig
  where
  type
    AWSResponse AssociateResolverQueryLogConfig =
      AssociateResolverQueryLogConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateResolverQueryLogConfigResponse'
            Prelude.<$> (x Core..?> "ResolverQueryLogConfigAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateResolverQueryLogConfig
  where
  hashWithSalt
    _salt
    AssociateResolverQueryLogConfig' {..} =
      _salt
        `Prelude.hashWithSalt` resolverQueryLogConfigId
        `Prelude.hashWithSalt` resourceId

instance
  Prelude.NFData
    AssociateResolverQueryLogConfig
  where
  rnf AssociateResolverQueryLogConfig' {..} =
    Prelude.rnf resolverQueryLogConfigId
      `Prelude.seq` Prelude.rnf resourceId

instance
  Core.ToHeaders
    AssociateResolverQueryLogConfig
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53Resolver.AssociateResolverQueryLogConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AssociateResolverQueryLogConfig where
  toJSON AssociateResolverQueryLogConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ResolverQueryLogConfigId"
                  Core..= resolverQueryLogConfigId
              ),
            Prelude.Just ("ResourceId" Core..= resourceId)
          ]
      )

instance Core.ToPath AssociateResolverQueryLogConfig where
  toPath = Prelude.const "/"

instance Core.ToQuery AssociateResolverQueryLogConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateResolverQueryLogConfigResponse' smart constructor.
data AssociateResolverQueryLogConfigResponse = AssociateResolverQueryLogConfigResponse'
  { -- | A complex type that contains settings for a specified association
    -- between an Amazon VPC and a query logging configuration.
    resolverQueryLogConfigAssociation :: Prelude.Maybe ResolverQueryLogConfigAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateResolverQueryLogConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolverQueryLogConfigAssociation', 'associateResolverQueryLogConfigResponse_resolverQueryLogConfigAssociation' - A complex type that contains settings for a specified association
-- between an Amazon VPC and a query logging configuration.
--
-- 'httpStatus', 'associateResolverQueryLogConfigResponse_httpStatus' - The response's http status code.
newAssociateResolverQueryLogConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateResolverQueryLogConfigResponse
newAssociateResolverQueryLogConfigResponse
  pHttpStatus_ =
    AssociateResolverQueryLogConfigResponse'
      { resolverQueryLogConfigAssociation =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A complex type that contains settings for a specified association
-- between an Amazon VPC and a query logging configuration.
associateResolverQueryLogConfigResponse_resolverQueryLogConfigAssociation :: Lens.Lens' AssociateResolverQueryLogConfigResponse (Prelude.Maybe ResolverQueryLogConfigAssociation)
associateResolverQueryLogConfigResponse_resolverQueryLogConfigAssociation = Lens.lens (\AssociateResolverQueryLogConfigResponse' {resolverQueryLogConfigAssociation} -> resolverQueryLogConfigAssociation) (\s@AssociateResolverQueryLogConfigResponse' {} a -> s {resolverQueryLogConfigAssociation = a} :: AssociateResolverQueryLogConfigResponse)

-- | The response's http status code.
associateResolverQueryLogConfigResponse_httpStatus :: Lens.Lens' AssociateResolverQueryLogConfigResponse Prelude.Int
associateResolverQueryLogConfigResponse_httpStatus = Lens.lens (\AssociateResolverQueryLogConfigResponse' {httpStatus} -> httpStatus) (\s@AssociateResolverQueryLogConfigResponse' {} a -> s {httpStatus = a} :: AssociateResolverQueryLogConfigResponse)

instance
  Prelude.NFData
    AssociateResolverQueryLogConfigResponse
  where
  rnf AssociateResolverQueryLogConfigResponse' {..} =
    Prelude.rnf resolverQueryLogConfigAssociation
      `Prelude.seq` Prelude.rnf httpStatus
