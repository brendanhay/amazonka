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
-- Module      : Network.AWS.RAM.ListPrincipals
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the principals that you have shared resources with or that have
-- shared resources with you.
--
-- This operation returns paginated results.
module Network.AWS.RAM.ListPrincipals
  ( -- * Creating a Request
    ListPrincipals (..),
    newListPrincipals,

    -- * Request Lenses
    listPrincipals_resourceType,
    listPrincipals_principals,
    listPrincipals_nextToken,
    listPrincipals_resourceArn,
    listPrincipals_maxResults,
    listPrincipals_resourceShareArns,
    listPrincipals_resourceOwner,

    -- * Destructuring the Response
    ListPrincipalsResponse (..),
    newListPrincipalsResponse,

    -- * Response Lenses
    listPrincipalsResponse_principals,
    listPrincipalsResponse_nextToken,
    listPrincipalsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RAM.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListPrincipals' smart constructor.
data ListPrincipals = ListPrincipals'
  { -- | The resource type.
    --
    -- Valid values: @acm-pca:CertificateAuthority@ | @appmesh:Mesh@ |
    -- @codebuild:Project@ | @codebuild:ReportGroup@ |
    -- @ec2:CapacityReservation@ | @ec2:DedicatedHost@ |
    -- @ec2:LocalGatewayRouteTable@ | @ec2:PrefixList@ | @ec2:Subnet@ |
    -- @ec2:TrafficMirrorTarget@ | @ec2:TransitGateway@ |
    -- @imagebuilder:Component@ | @imagebuilder:Image@ |
    -- @imagebuilder:ImageRecipe@ | @imagebuilder:ContainerRecipe@ |
    -- @glue:Catalog@ | @glue:Database@ | @glue:Table@ |
    -- @license-manager:LicenseConfiguration@ I
    -- @network-firewall:FirewallPolicy@ | @network-firewall:StatefulRuleGroup@
    -- | @network-firewall:StatelessRuleGroup@ | @outposts:Outpost@ |
    -- @resource-groups:Group@ | @rds:Cluster@ |
    -- @route53resolver:FirewallRuleGroup@
    -- |@route53resolver:ResolverQueryLogConfig@ |
    -- @route53resolver:ResolverRule@ | @s3-outposts:Outpost@ |
    -- @ssm-contacts:Contact@ | @ssm-incidents:ResponsePlan@
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The principals.
    principals :: Prelude.Maybe [Prelude.Text],
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the resource.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Names (ARN) of the resource shares.
    resourceShareArns :: Prelude.Maybe [Prelude.Text],
    -- | The type of owner.
    resourceOwner :: ResourceOwner
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPrincipals' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'listPrincipals_resourceType' - The resource type.
--
-- Valid values: @acm-pca:CertificateAuthority@ | @appmesh:Mesh@ |
-- @codebuild:Project@ | @codebuild:ReportGroup@ |
-- @ec2:CapacityReservation@ | @ec2:DedicatedHost@ |
-- @ec2:LocalGatewayRouteTable@ | @ec2:PrefixList@ | @ec2:Subnet@ |
-- @ec2:TrafficMirrorTarget@ | @ec2:TransitGateway@ |
-- @imagebuilder:Component@ | @imagebuilder:Image@ |
-- @imagebuilder:ImageRecipe@ | @imagebuilder:ContainerRecipe@ |
-- @glue:Catalog@ | @glue:Database@ | @glue:Table@ |
-- @license-manager:LicenseConfiguration@ I
-- @network-firewall:FirewallPolicy@ | @network-firewall:StatefulRuleGroup@
-- | @network-firewall:StatelessRuleGroup@ | @outposts:Outpost@ |
-- @resource-groups:Group@ | @rds:Cluster@ |
-- @route53resolver:FirewallRuleGroup@
-- |@route53resolver:ResolverQueryLogConfig@ |
-- @route53resolver:ResolverRule@ | @s3-outposts:Outpost@ |
-- @ssm-contacts:Contact@ | @ssm-incidents:ResponsePlan@
--
-- 'principals', 'listPrincipals_principals' - The principals.
--
-- 'nextToken', 'listPrincipals_nextToken' - The token for the next page of results.
--
-- 'resourceArn', 'listPrincipals_resourceArn' - The Amazon Resource Name (ARN) of the resource.
--
-- 'maxResults', 'listPrincipals_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'resourceShareArns', 'listPrincipals_resourceShareArns' - The Amazon Resource Names (ARN) of the resource shares.
--
-- 'resourceOwner', 'listPrincipals_resourceOwner' - The type of owner.
newListPrincipals ::
  -- | 'resourceOwner'
  ResourceOwner ->
  ListPrincipals
newListPrincipals pResourceOwner_ =
  ListPrincipals'
    { resourceType = Prelude.Nothing,
      principals = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      resourceShareArns = Prelude.Nothing,
      resourceOwner = pResourceOwner_
    }

-- | The resource type.
--
-- Valid values: @acm-pca:CertificateAuthority@ | @appmesh:Mesh@ |
-- @codebuild:Project@ | @codebuild:ReportGroup@ |
-- @ec2:CapacityReservation@ | @ec2:DedicatedHost@ |
-- @ec2:LocalGatewayRouteTable@ | @ec2:PrefixList@ | @ec2:Subnet@ |
-- @ec2:TrafficMirrorTarget@ | @ec2:TransitGateway@ |
-- @imagebuilder:Component@ | @imagebuilder:Image@ |
-- @imagebuilder:ImageRecipe@ | @imagebuilder:ContainerRecipe@ |
-- @glue:Catalog@ | @glue:Database@ | @glue:Table@ |
-- @license-manager:LicenseConfiguration@ I
-- @network-firewall:FirewallPolicy@ | @network-firewall:StatefulRuleGroup@
-- | @network-firewall:StatelessRuleGroup@ | @outposts:Outpost@ |
-- @resource-groups:Group@ | @rds:Cluster@ |
-- @route53resolver:FirewallRuleGroup@
-- |@route53resolver:ResolverQueryLogConfig@ |
-- @route53resolver:ResolverRule@ | @s3-outposts:Outpost@ |
-- @ssm-contacts:Contact@ | @ssm-incidents:ResponsePlan@
listPrincipals_resourceType :: Lens.Lens' ListPrincipals (Prelude.Maybe Prelude.Text)
listPrincipals_resourceType = Lens.lens (\ListPrincipals' {resourceType} -> resourceType) (\s@ListPrincipals' {} a -> s {resourceType = a} :: ListPrincipals)

-- | The principals.
listPrincipals_principals :: Lens.Lens' ListPrincipals (Prelude.Maybe [Prelude.Text])
listPrincipals_principals = Lens.lens (\ListPrincipals' {principals} -> principals) (\s@ListPrincipals' {} a -> s {principals = a} :: ListPrincipals) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next page of results.
listPrincipals_nextToken :: Lens.Lens' ListPrincipals (Prelude.Maybe Prelude.Text)
listPrincipals_nextToken = Lens.lens (\ListPrincipals' {nextToken} -> nextToken) (\s@ListPrincipals' {} a -> s {nextToken = a} :: ListPrincipals)

-- | The Amazon Resource Name (ARN) of the resource.
listPrincipals_resourceArn :: Lens.Lens' ListPrincipals (Prelude.Maybe Prelude.Text)
listPrincipals_resourceArn = Lens.lens (\ListPrincipals' {resourceArn} -> resourceArn) (\s@ListPrincipals' {} a -> s {resourceArn = a} :: ListPrincipals)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
listPrincipals_maxResults :: Lens.Lens' ListPrincipals (Prelude.Maybe Prelude.Natural)
listPrincipals_maxResults = Lens.lens (\ListPrincipals' {maxResults} -> maxResults) (\s@ListPrincipals' {} a -> s {maxResults = a} :: ListPrincipals)

-- | The Amazon Resource Names (ARN) of the resource shares.
listPrincipals_resourceShareArns :: Lens.Lens' ListPrincipals (Prelude.Maybe [Prelude.Text])
listPrincipals_resourceShareArns = Lens.lens (\ListPrincipals' {resourceShareArns} -> resourceShareArns) (\s@ListPrincipals' {} a -> s {resourceShareArns = a} :: ListPrincipals) Prelude.. Lens.mapping Lens.coerced

-- | The type of owner.
listPrincipals_resourceOwner :: Lens.Lens' ListPrincipals ResourceOwner
listPrincipals_resourceOwner = Lens.lens (\ListPrincipals' {resourceOwner} -> resourceOwner) (\s@ListPrincipals' {} a -> s {resourceOwner = a} :: ListPrincipals)

instance Core.AWSPager ListPrincipals where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPrincipalsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPrincipalsResponse_principals
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listPrincipals_nextToken
          Lens..~ rs
          Lens.^? listPrincipalsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListPrincipals where
  type
    AWSResponse ListPrincipals =
      ListPrincipalsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPrincipalsResponse'
            Prelude.<$> (x Core..?> "principals" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPrincipals

instance Prelude.NFData ListPrincipals

instance Core.ToHeaders ListPrincipals where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListPrincipals where
  toJSON ListPrincipals' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("resourceType" Core..=) Prelude.<$> resourceType,
            ("principals" Core..=) Prelude.<$> principals,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("resourceArn" Core..=) Prelude.<$> resourceArn,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("resourceShareArns" Core..=)
              Prelude.<$> resourceShareArns,
            Prelude.Just
              ("resourceOwner" Core..= resourceOwner)
          ]
      )

instance Core.ToPath ListPrincipals where
  toPath = Prelude.const "/listprincipals"

instance Core.ToQuery ListPrincipals where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPrincipalsResponse' smart constructor.
data ListPrincipalsResponse = ListPrincipalsResponse'
  { -- | The principals.
    principals :: Prelude.Maybe [Principal],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPrincipalsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principals', 'listPrincipalsResponse_principals' - The principals.
--
-- 'nextToken', 'listPrincipalsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'listPrincipalsResponse_httpStatus' - The response's http status code.
newListPrincipalsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPrincipalsResponse
newListPrincipalsResponse pHttpStatus_ =
  ListPrincipalsResponse'
    { principals =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The principals.
listPrincipalsResponse_principals :: Lens.Lens' ListPrincipalsResponse (Prelude.Maybe [Principal])
listPrincipalsResponse_principals = Lens.lens (\ListPrincipalsResponse' {principals} -> principals) (\s@ListPrincipalsResponse' {} a -> s {principals = a} :: ListPrincipalsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
listPrincipalsResponse_nextToken :: Lens.Lens' ListPrincipalsResponse (Prelude.Maybe Prelude.Text)
listPrincipalsResponse_nextToken = Lens.lens (\ListPrincipalsResponse' {nextToken} -> nextToken) (\s@ListPrincipalsResponse' {} a -> s {nextToken = a} :: ListPrincipalsResponse)

-- | The response's http status code.
listPrincipalsResponse_httpStatus :: Lens.Lens' ListPrincipalsResponse Prelude.Int
listPrincipalsResponse_httpStatus = Lens.lens (\ListPrincipalsResponse' {httpStatus} -> httpStatus) (\s@ListPrincipalsResponse' {} a -> s {httpStatus = a} :: ListPrincipalsResponse)

instance Prelude.NFData ListPrincipalsResponse
