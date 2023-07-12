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
-- Module      : Amazonka.Route53Resolver.CreateResolverEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Resolver endpoint. There are two types of Resolver endpoints,
-- inbound and outbound:
--
-- -   An /inbound Resolver endpoint/ forwards DNS queries to the DNS
--     service for a VPC from your network.
--
-- -   An /outbound Resolver endpoint/ forwards DNS queries from the DNS
--     service for a VPC to your network.
module Amazonka.Route53Resolver.CreateResolverEndpoint
  ( -- * Creating a Request
    CreateResolverEndpoint (..),
    newCreateResolverEndpoint,

    -- * Request Lenses
    createResolverEndpoint_name,
    createResolverEndpoint_tags,
    createResolverEndpoint_creatorRequestId,
    createResolverEndpoint_securityGroupIds,
    createResolverEndpoint_direction,
    createResolverEndpoint_ipAddresses,

    -- * Destructuring the Response
    CreateResolverEndpointResponse (..),
    newCreateResolverEndpointResponse,

    -- * Response Lenses
    createResolverEndpointResponse_resolverEndpoint,
    createResolverEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newCreateResolverEndpoint' smart constructor.
data CreateResolverEndpoint = CreateResolverEndpoint'
  { -- | A friendly name that lets you easily find a configuration in the
    -- Resolver dashboard in the Route 53 console.
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of the tag keys and values that you want to associate with the
    -- endpoint.
    tags :: Prelude.Maybe [Tag],
    -- | A unique string that identifies the request and that allows failed
    -- requests to be retried without the risk of running the operation twice.
    -- @CreatorRequestId@ can be any unique string, for example, a date\/time
    -- stamp.
    creatorRequestId :: Prelude.Text,
    -- | The ID of one or more security groups that you want to use to control
    -- access to this VPC. The security group that you specify must include one
    -- or more inbound rules (for inbound Resolver endpoints) or outbound rules
    -- (for outbound Resolver endpoints). Inbound and outbound rules must allow
    -- TCP and UDP access. For inbound access, open port 53. For outbound
    -- access, open the port that you\'re using for DNS queries on your
    -- network.
    securityGroupIds :: [Prelude.Text],
    -- | Specify the applicable value:
    --
    -- -   @INBOUND@: Resolver forwards DNS queries to the DNS service for a
    --     VPC from your network
    --
    -- -   @OUTBOUND@: Resolver forwards DNS queries from the DNS service for a
    --     VPC to your network
    direction :: ResolverEndpointDirection,
    -- | The subnets and IP addresses in your VPC that DNS queries originate from
    -- (for outbound endpoints) or that you forward DNS queries to (for inbound
    -- endpoints). The subnet ID uniquely identifies a VPC.
    ipAddresses :: Prelude.NonEmpty IpAddressRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateResolverEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createResolverEndpoint_name' - A friendly name that lets you easily find a configuration in the
-- Resolver dashboard in the Route 53 console.
--
-- 'tags', 'createResolverEndpoint_tags' - A list of the tag keys and values that you want to associate with the
-- endpoint.
--
-- 'creatorRequestId', 'createResolverEndpoint_creatorRequestId' - A unique string that identifies the request and that allows failed
-- requests to be retried without the risk of running the operation twice.
-- @CreatorRequestId@ can be any unique string, for example, a date\/time
-- stamp.
--
-- 'securityGroupIds', 'createResolverEndpoint_securityGroupIds' - The ID of one or more security groups that you want to use to control
-- access to this VPC. The security group that you specify must include one
-- or more inbound rules (for inbound Resolver endpoints) or outbound rules
-- (for outbound Resolver endpoints). Inbound and outbound rules must allow
-- TCP and UDP access. For inbound access, open port 53. For outbound
-- access, open the port that you\'re using for DNS queries on your
-- network.
--
-- 'direction', 'createResolverEndpoint_direction' - Specify the applicable value:
--
-- -   @INBOUND@: Resolver forwards DNS queries to the DNS service for a
--     VPC from your network
--
-- -   @OUTBOUND@: Resolver forwards DNS queries from the DNS service for a
--     VPC to your network
--
-- 'ipAddresses', 'createResolverEndpoint_ipAddresses' - The subnets and IP addresses in your VPC that DNS queries originate from
-- (for outbound endpoints) or that you forward DNS queries to (for inbound
-- endpoints). The subnet ID uniquely identifies a VPC.
newCreateResolverEndpoint ::
  -- | 'creatorRequestId'
  Prelude.Text ->
  -- | 'direction'
  ResolverEndpointDirection ->
  -- | 'ipAddresses'
  Prelude.NonEmpty IpAddressRequest ->
  CreateResolverEndpoint
newCreateResolverEndpoint
  pCreatorRequestId_
  pDirection_
  pIpAddresses_ =
    CreateResolverEndpoint'
      { name = Prelude.Nothing,
        tags = Prelude.Nothing,
        creatorRequestId = pCreatorRequestId_,
        securityGroupIds = Prelude.mempty,
        direction = pDirection_,
        ipAddresses = Lens.coerced Lens.# pIpAddresses_
      }

-- | A friendly name that lets you easily find a configuration in the
-- Resolver dashboard in the Route 53 console.
createResolverEndpoint_name :: Lens.Lens' CreateResolverEndpoint (Prelude.Maybe Prelude.Text)
createResolverEndpoint_name = Lens.lens (\CreateResolverEndpoint' {name} -> name) (\s@CreateResolverEndpoint' {} a -> s {name = a} :: CreateResolverEndpoint)

-- | A list of the tag keys and values that you want to associate with the
-- endpoint.
createResolverEndpoint_tags :: Lens.Lens' CreateResolverEndpoint (Prelude.Maybe [Tag])
createResolverEndpoint_tags = Lens.lens (\CreateResolverEndpoint' {tags} -> tags) (\s@CreateResolverEndpoint' {} a -> s {tags = a} :: CreateResolverEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | A unique string that identifies the request and that allows failed
-- requests to be retried without the risk of running the operation twice.
-- @CreatorRequestId@ can be any unique string, for example, a date\/time
-- stamp.
createResolverEndpoint_creatorRequestId :: Lens.Lens' CreateResolverEndpoint Prelude.Text
createResolverEndpoint_creatorRequestId = Lens.lens (\CreateResolverEndpoint' {creatorRequestId} -> creatorRequestId) (\s@CreateResolverEndpoint' {} a -> s {creatorRequestId = a} :: CreateResolverEndpoint)

-- | The ID of one or more security groups that you want to use to control
-- access to this VPC. The security group that you specify must include one
-- or more inbound rules (for inbound Resolver endpoints) or outbound rules
-- (for outbound Resolver endpoints). Inbound and outbound rules must allow
-- TCP and UDP access. For inbound access, open port 53. For outbound
-- access, open the port that you\'re using for DNS queries on your
-- network.
createResolverEndpoint_securityGroupIds :: Lens.Lens' CreateResolverEndpoint [Prelude.Text]
createResolverEndpoint_securityGroupIds = Lens.lens (\CreateResolverEndpoint' {securityGroupIds} -> securityGroupIds) (\s@CreateResolverEndpoint' {} a -> s {securityGroupIds = a} :: CreateResolverEndpoint) Prelude.. Lens.coerced

-- | Specify the applicable value:
--
-- -   @INBOUND@: Resolver forwards DNS queries to the DNS service for a
--     VPC from your network
--
-- -   @OUTBOUND@: Resolver forwards DNS queries from the DNS service for a
--     VPC to your network
createResolverEndpoint_direction :: Lens.Lens' CreateResolverEndpoint ResolverEndpointDirection
createResolverEndpoint_direction = Lens.lens (\CreateResolverEndpoint' {direction} -> direction) (\s@CreateResolverEndpoint' {} a -> s {direction = a} :: CreateResolverEndpoint)

-- | The subnets and IP addresses in your VPC that DNS queries originate from
-- (for outbound endpoints) or that you forward DNS queries to (for inbound
-- endpoints). The subnet ID uniquely identifies a VPC.
createResolverEndpoint_ipAddresses :: Lens.Lens' CreateResolverEndpoint (Prelude.NonEmpty IpAddressRequest)
createResolverEndpoint_ipAddresses = Lens.lens (\CreateResolverEndpoint' {ipAddresses} -> ipAddresses) (\s@CreateResolverEndpoint' {} a -> s {ipAddresses = a} :: CreateResolverEndpoint) Prelude.. Lens.coerced

instance Core.AWSRequest CreateResolverEndpoint where
  type
    AWSResponse CreateResolverEndpoint =
      CreateResolverEndpointResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateResolverEndpointResponse'
            Prelude.<$> (x Data..?> "ResolverEndpoint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateResolverEndpoint where
  hashWithSalt _salt CreateResolverEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` creatorRequestId
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` direction
      `Prelude.hashWithSalt` ipAddresses

instance Prelude.NFData CreateResolverEndpoint where
  rnf CreateResolverEndpoint' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf creatorRequestId
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf direction
      `Prelude.seq` Prelude.rnf ipAddresses

instance Data.ToHeaders CreateResolverEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.CreateResolverEndpoint" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateResolverEndpoint where
  toJSON CreateResolverEndpoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("CreatorRequestId" Data..= creatorRequestId),
            Prelude.Just
              ("SecurityGroupIds" Data..= securityGroupIds),
            Prelude.Just ("Direction" Data..= direction),
            Prelude.Just ("IpAddresses" Data..= ipAddresses)
          ]
      )

instance Data.ToPath CreateResolverEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateResolverEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateResolverEndpointResponse' smart constructor.
data CreateResolverEndpointResponse = CreateResolverEndpointResponse'
  { -- | Information about the @CreateResolverEndpoint@ request, including the
    -- status of the request.
    resolverEndpoint :: Prelude.Maybe ResolverEndpoint,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateResolverEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolverEndpoint', 'createResolverEndpointResponse_resolverEndpoint' - Information about the @CreateResolverEndpoint@ request, including the
-- status of the request.
--
-- 'httpStatus', 'createResolverEndpointResponse_httpStatus' - The response's http status code.
newCreateResolverEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateResolverEndpointResponse
newCreateResolverEndpointResponse pHttpStatus_ =
  CreateResolverEndpointResponse'
    { resolverEndpoint =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the @CreateResolverEndpoint@ request, including the
-- status of the request.
createResolverEndpointResponse_resolverEndpoint :: Lens.Lens' CreateResolverEndpointResponse (Prelude.Maybe ResolverEndpoint)
createResolverEndpointResponse_resolverEndpoint = Lens.lens (\CreateResolverEndpointResponse' {resolverEndpoint} -> resolverEndpoint) (\s@CreateResolverEndpointResponse' {} a -> s {resolverEndpoint = a} :: CreateResolverEndpointResponse)

-- | The response's http status code.
createResolverEndpointResponse_httpStatus :: Lens.Lens' CreateResolverEndpointResponse Prelude.Int
createResolverEndpointResponse_httpStatus = Lens.lens (\CreateResolverEndpointResponse' {httpStatus} -> httpStatus) (\s@CreateResolverEndpointResponse' {} a -> s {httpStatus = a} :: CreateResolverEndpointResponse)

instance
  Prelude.NFData
    CreateResolverEndpointResponse
  where
  rnf CreateResolverEndpointResponse' {..} =
    Prelude.rnf resolverEndpoint
      `Prelude.seq` Prelude.rnf httpStatus
