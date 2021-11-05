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
-- Module      : Network.AWS.RAM.ListResources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the resources that you added to a resource shares or the resources
-- that are shared with you.
--
-- This operation returns paginated results.
module Network.AWS.RAM.ListResources
  ( -- * Creating a Request
    ListResources (..),
    newListResources,

    -- * Request Lenses
    listResources_resourceType,
    listResources_nextToken,
    listResources_resourceArns,
    listResources_principal,
    listResources_maxResults,
    listResources_resourceShareArns,
    listResources_resourceOwner,

    -- * Destructuring the Response
    ListResourcesResponse (..),
    newListResourcesResponse,

    -- * Response Lenses
    listResourcesResponse_resources,
    listResourcesResponse_nextToken,
    listResourcesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RAM.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListResources' smart constructor.
data ListResources = ListResources'
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
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Names (ARNs) of the resources.
    resourceArns :: Prelude.Maybe [Prelude.Text],
    -- | The principal.
    principal :: Prelude.Maybe Prelude.Text,
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
-- Create a value of 'ListResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'listResources_resourceType' - The resource type.
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
-- 'nextToken', 'listResources_nextToken' - The token for the next page of results.
--
-- 'resourceArns', 'listResources_resourceArns' - The Amazon Resource Names (ARNs) of the resources.
--
-- 'principal', 'listResources_principal' - The principal.
--
-- 'maxResults', 'listResources_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'resourceShareArns', 'listResources_resourceShareArns' - The Amazon Resource Names (ARN) of the resource shares.
--
-- 'resourceOwner', 'listResources_resourceOwner' - The type of owner.
newListResources ::
  -- | 'resourceOwner'
  ResourceOwner ->
  ListResources
newListResources pResourceOwner_ =
  ListResources'
    { resourceType = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resourceArns = Prelude.Nothing,
      principal = Prelude.Nothing,
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
listResources_resourceType :: Lens.Lens' ListResources (Prelude.Maybe Prelude.Text)
listResources_resourceType = Lens.lens (\ListResources' {resourceType} -> resourceType) (\s@ListResources' {} a -> s {resourceType = a} :: ListResources)

-- | The token for the next page of results.
listResources_nextToken :: Lens.Lens' ListResources (Prelude.Maybe Prelude.Text)
listResources_nextToken = Lens.lens (\ListResources' {nextToken} -> nextToken) (\s@ListResources' {} a -> s {nextToken = a} :: ListResources)

-- | The Amazon Resource Names (ARNs) of the resources.
listResources_resourceArns :: Lens.Lens' ListResources (Prelude.Maybe [Prelude.Text])
listResources_resourceArns = Lens.lens (\ListResources' {resourceArns} -> resourceArns) (\s@ListResources' {} a -> s {resourceArns = a} :: ListResources) Prelude.. Lens.mapping Lens.coerced

-- | The principal.
listResources_principal :: Lens.Lens' ListResources (Prelude.Maybe Prelude.Text)
listResources_principal = Lens.lens (\ListResources' {principal} -> principal) (\s@ListResources' {} a -> s {principal = a} :: ListResources)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
listResources_maxResults :: Lens.Lens' ListResources (Prelude.Maybe Prelude.Natural)
listResources_maxResults = Lens.lens (\ListResources' {maxResults} -> maxResults) (\s@ListResources' {} a -> s {maxResults = a} :: ListResources)

-- | The Amazon Resource Names (ARN) of the resource shares.
listResources_resourceShareArns :: Lens.Lens' ListResources (Prelude.Maybe [Prelude.Text])
listResources_resourceShareArns = Lens.lens (\ListResources' {resourceShareArns} -> resourceShareArns) (\s@ListResources' {} a -> s {resourceShareArns = a} :: ListResources) Prelude.. Lens.mapping Lens.coerced

-- | The type of owner.
listResources_resourceOwner :: Lens.Lens' ListResources ResourceOwner
listResources_resourceOwner = Lens.lens (\ListResources' {resourceOwner} -> resourceOwner) (\s@ListResources' {} a -> s {resourceOwner = a} :: ListResources)

instance Core.AWSPager ListResources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResourcesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listResourcesResponse_resources Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listResources_nextToken
          Lens..~ rs
          Lens.^? listResourcesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListResources where
  type
    AWSResponse ListResources =
      ListResourcesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourcesResponse'
            Prelude.<$> (x Core..?> "resources" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListResources

instance Prelude.NFData ListResources

instance Core.ToHeaders ListResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListResources where
  toJSON ListResources' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("resourceType" Core..=) Prelude.<$> resourceType,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("resourceArns" Core..=) Prelude.<$> resourceArns,
            ("principal" Core..=) Prelude.<$> principal,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("resourceShareArns" Core..=)
              Prelude.<$> resourceShareArns,
            Prelude.Just
              ("resourceOwner" Core..= resourceOwner)
          ]
      )

instance Core.ToPath ListResources where
  toPath = Prelude.const "/listresources"

instance Core.ToQuery ListResources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListResourcesResponse' smart constructor.
data ListResourcesResponse = ListResourcesResponse'
  { -- | Information about the resources.
    resources :: Prelude.Maybe [Resource],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resources', 'listResourcesResponse_resources' - Information about the resources.
--
-- 'nextToken', 'listResourcesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'listResourcesResponse_httpStatus' - The response's http status code.
newListResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResourcesResponse
newListResourcesResponse pHttpStatus_ =
  ListResourcesResponse'
    { resources = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the resources.
listResourcesResponse_resources :: Lens.Lens' ListResourcesResponse (Prelude.Maybe [Resource])
listResourcesResponse_resources = Lens.lens (\ListResourcesResponse' {resources} -> resources) (\s@ListResourcesResponse' {} a -> s {resources = a} :: ListResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
listResourcesResponse_nextToken :: Lens.Lens' ListResourcesResponse (Prelude.Maybe Prelude.Text)
listResourcesResponse_nextToken = Lens.lens (\ListResourcesResponse' {nextToken} -> nextToken) (\s@ListResourcesResponse' {} a -> s {nextToken = a} :: ListResourcesResponse)

-- | The response's http status code.
listResourcesResponse_httpStatus :: Lens.Lens' ListResourcesResponse Prelude.Int
listResourcesResponse_httpStatus = Lens.lens (\ListResourcesResponse' {httpStatus} -> httpStatus) (\s@ListResourcesResponse' {} a -> s {httpStatus = a} :: ListResourcesResponse)

instance Prelude.NFData ListResourcesResponse
