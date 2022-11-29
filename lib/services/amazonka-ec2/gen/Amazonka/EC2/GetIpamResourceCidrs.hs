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
-- Module      : Amazonka.EC2.GetIpamResourceCidrs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about the resources in a scope.
--
-- This operation returns paginated results.
module Amazonka.EC2.GetIpamResourceCidrs
  ( -- * Creating a Request
    GetIpamResourceCidrs (..),
    newGetIpamResourceCidrs,

    -- * Request Lenses
    getIpamResourceCidrs_resourceId,
    getIpamResourceCidrs_resourceType,
    getIpamResourceCidrs_nextToken,
    getIpamResourceCidrs_filters,
    getIpamResourceCidrs_resourceOwner,
    getIpamResourceCidrs_dryRun,
    getIpamResourceCidrs_maxResults,
    getIpamResourceCidrs_ipamPoolId,
    getIpamResourceCidrs_resourceTag,
    getIpamResourceCidrs_ipamScopeId,

    -- * Destructuring the Response
    GetIpamResourceCidrsResponse (..),
    newGetIpamResourceCidrsResponse,

    -- * Response Lenses
    getIpamResourceCidrsResponse_nextToken,
    getIpamResourceCidrsResponse_ipamResourceCidrs,
    getIpamResourceCidrsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetIpamResourceCidrs' smart constructor.
data GetIpamResourceCidrs = GetIpamResourceCidrs'
  { -- | The ID of the resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The resource type.
    resourceType :: Prelude.Maybe IpamResourceType,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more filters for the request. For more information about
    -- filtering, see
    -- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-filter.html Filtering CLI output>.
    filters :: Prelude.Maybe [Filter],
    -- | The ID of the Amazon Web Services account that owns the resource.
    resourceOwner :: Prelude.Maybe Prelude.Text,
    -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return in the request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the IPAM pool that the resource is in.
    ipamPoolId :: Prelude.Maybe Prelude.Text,
    -- | The resource tag.
    resourceTag :: Prelude.Maybe RequestIpamResourceTag,
    -- | The ID of the scope that the resource is in.
    ipamScopeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIpamResourceCidrs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'getIpamResourceCidrs_resourceId' - The ID of the resource.
--
-- 'resourceType', 'getIpamResourceCidrs_resourceType' - The resource type.
--
-- 'nextToken', 'getIpamResourceCidrs_nextToken' - The token for the next page of results.
--
-- 'filters', 'getIpamResourceCidrs_filters' - One or more filters for the request. For more information about
-- filtering, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-filter.html Filtering CLI output>.
--
-- 'resourceOwner', 'getIpamResourceCidrs_resourceOwner' - The ID of the Amazon Web Services account that owns the resource.
--
-- 'dryRun', 'getIpamResourceCidrs_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'getIpamResourceCidrs_maxResults' - The maximum number of results to return in the request.
--
-- 'ipamPoolId', 'getIpamResourceCidrs_ipamPoolId' - The ID of the IPAM pool that the resource is in.
--
-- 'resourceTag', 'getIpamResourceCidrs_resourceTag' - The resource tag.
--
-- 'ipamScopeId', 'getIpamResourceCidrs_ipamScopeId' - The ID of the scope that the resource is in.
newGetIpamResourceCidrs ::
  -- | 'ipamScopeId'
  Prelude.Text ->
  GetIpamResourceCidrs
newGetIpamResourceCidrs pIpamScopeId_ =
  GetIpamResourceCidrs'
    { resourceId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      filters = Prelude.Nothing,
      resourceOwner = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      ipamPoolId = Prelude.Nothing,
      resourceTag = Prelude.Nothing,
      ipamScopeId = pIpamScopeId_
    }

-- | The ID of the resource.
getIpamResourceCidrs_resourceId :: Lens.Lens' GetIpamResourceCidrs (Prelude.Maybe Prelude.Text)
getIpamResourceCidrs_resourceId = Lens.lens (\GetIpamResourceCidrs' {resourceId} -> resourceId) (\s@GetIpamResourceCidrs' {} a -> s {resourceId = a} :: GetIpamResourceCidrs)

-- | The resource type.
getIpamResourceCidrs_resourceType :: Lens.Lens' GetIpamResourceCidrs (Prelude.Maybe IpamResourceType)
getIpamResourceCidrs_resourceType = Lens.lens (\GetIpamResourceCidrs' {resourceType} -> resourceType) (\s@GetIpamResourceCidrs' {} a -> s {resourceType = a} :: GetIpamResourceCidrs)

-- | The token for the next page of results.
getIpamResourceCidrs_nextToken :: Lens.Lens' GetIpamResourceCidrs (Prelude.Maybe Prelude.Text)
getIpamResourceCidrs_nextToken = Lens.lens (\GetIpamResourceCidrs' {nextToken} -> nextToken) (\s@GetIpamResourceCidrs' {} a -> s {nextToken = a} :: GetIpamResourceCidrs)

-- | One or more filters for the request. For more information about
-- filtering, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-filter.html Filtering CLI output>.
getIpamResourceCidrs_filters :: Lens.Lens' GetIpamResourceCidrs (Prelude.Maybe [Filter])
getIpamResourceCidrs_filters = Lens.lens (\GetIpamResourceCidrs' {filters} -> filters) (\s@GetIpamResourceCidrs' {} a -> s {filters = a} :: GetIpamResourceCidrs) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Web Services account that owns the resource.
getIpamResourceCidrs_resourceOwner :: Lens.Lens' GetIpamResourceCidrs (Prelude.Maybe Prelude.Text)
getIpamResourceCidrs_resourceOwner = Lens.lens (\GetIpamResourceCidrs' {resourceOwner} -> resourceOwner) (\s@GetIpamResourceCidrs' {} a -> s {resourceOwner = a} :: GetIpamResourceCidrs)

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
getIpamResourceCidrs_dryRun :: Lens.Lens' GetIpamResourceCidrs (Prelude.Maybe Prelude.Bool)
getIpamResourceCidrs_dryRun = Lens.lens (\GetIpamResourceCidrs' {dryRun} -> dryRun) (\s@GetIpamResourceCidrs' {} a -> s {dryRun = a} :: GetIpamResourceCidrs)

-- | The maximum number of results to return in the request.
getIpamResourceCidrs_maxResults :: Lens.Lens' GetIpamResourceCidrs (Prelude.Maybe Prelude.Natural)
getIpamResourceCidrs_maxResults = Lens.lens (\GetIpamResourceCidrs' {maxResults} -> maxResults) (\s@GetIpamResourceCidrs' {} a -> s {maxResults = a} :: GetIpamResourceCidrs)

-- | The ID of the IPAM pool that the resource is in.
getIpamResourceCidrs_ipamPoolId :: Lens.Lens' GetIpamResourceCidrs (Prelude.Maybe Prelude.Text)
getIpamResourceCidrs_ipamPoolId = Lens.lens (\GetIpamResourceCidrs' {ipamPoolId} -> ipamPoolId) (\s@GetIpamResourceCidrs' {} a -> s {ipamPoolId = a} :: GetIpamResourceCidrs)

-- | The resource tag.
getIpamResourceCidrs_resourceTag :: Lens.Lens' GetIpamResourceCidrs (Prelude.Maybe RequestIpamResourceTag)
getIpamResourceCidrs_resourceTag = Lens.lens (\GetIpamResourceCidrs' {resourceTag} -> resourceTag) (\s@GetIpamResourceCidrs' {} a -> s {resourceTag = a} :: GetIpamResourceCidrs)

-- | The ID of the scope that the resource is in.
getIpamResourceCidrs_ipamScopeId :: Lens.Lens' GetIpamResourceCidrs Prelude.Text
getIpamResourceCidrs_ipamScopeId = Lens.lens (\GetIpamResourceCidrs' {ipamScopeId} -> ipamScopeId) (\s@GetIpamResourceCidrs' {} a -> s {ipamScopeId = a} :: GetIpamResourceCidrs)

instance Core.AWSPager GetIpamResourceCidrs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getIpamResourceCidrsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getIpamResourceCidrsResponse_ipamResourceCidrs
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getIpamResourceCidrs_nextToken
          Lens..~ rs
          Lens.^? getIpamResourceCidrsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetIpamResourceCidrs where
  type
    AWSResponse GetIpamResourceCidrs =
      GetIpamResourceCidrsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetIpamResourceCidrsResponse'
            Prelude.<$> (x Core..@? "nextToken")
            Prelude.<*> ( x Core..@? "ipamResourceCidrSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetIpamResourceCidrs where
  hashWithSalt _salt GetIpamResourceCidrs' {..} =
    _salt `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` resourceOwner
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` ipamPoolId
      `Prelude.hashWithSalt` resourceTag
      `Prelude.hashWithSalt` ipamScopeId

instance Prelude.NFData GetIpamResourceCidrs where
  rnf GetIpamResourceCidrs' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf resourceOwner
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf ipamPoolId
      `Prelude.seq` Prelude.rnf resourceTag
      `Prelude.seq` Prelude.rnf ipamScopeId

instance Core.ToHeaders GetIpamResourceCidrs where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetIpamResourceCidrs where
  toPath = Prelude.const "/"

instance Core.ToQuery GetIpamResourceCidrs where
  toQuery GetIpamResourceCidrs' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("GetIpamResourceCidrs" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "ResourceId" Core.=: resourceId,
        "ResourceType" Core.=: resourceType,
        "NextToken" Core.=: nextToken,
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters),
        "ResourceOwner" Core.=: resourceOwner,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        "IpamPoolId" Core.=: ipamPoolId,
        "ResourceTag" Core.=: resourceTag,
        "IpamScopeId" Core.=: ipamScopeId
      ]

-- | /See:/ 'newGetIpamResourceCidrsResponse' smart constructor.
data GetIpamResourceCidrsResponse = GetIpamResourceCidrsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The resource CIDRs.
    ipamResourceCidrs :: Prelude.Maybe [IpamResourceCidr],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIpamResourceCidrsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getIpamResourceCidrsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'ipamResourceCidrs', 'getIpamResourceCidrsResponse_ipamResourceCidrs' - The resource CIDRs.
--
-- 'httpStatus', 'getIpamResourceCidrsResponse_httpStatus' - The response's http status code.
newGetIpamResourceCidrsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetIpamResourceCidrsResponse
newGetIpamResourceCidrsResponse pHttpStatus_ =
  GetIpamResourceCidrsResponse'
    { nextToken =
        Prelude.Nothing,
      ipamResourceCidrs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
getIpamResourceCidrsResponse_nextToken :: Lens.Lens' GetIpamResourceCidrsResponse (Prelude.Maybe Prelude.Text)
getIpamResourceCidrsResponse_nextToken = Lens.lens (\GetIpamResourceCidrsResponse' {nextToken} -> nextToken) (\s@GetIpamResourceCidrsResponse' {} a -> s {nextToken = a} :: GetIpamResourceCidrsResponse)

-- | The resource CIDRs.
getIpamResourceCidrsResponse_ipamResourceCidrs :: Lens.Lens' GetIpamResourceCidrsResponse (Prelude.Maybe [IpamResourceCidr])
getIpamResourceCidrsResponse_ipamResourceCidrs = Lens.lens (\GetIpamResourceCidrsResponse' {ipamResourceCidrs} -> ipamResourceCidrs) (\s@GetIpamResourceCidrsResponse' {} a -> s {ipamResourceCidrs = a} :: GetIpamResourceCidrsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getIpamResourceCidrsResponse_httpStatus :: Lens.Lens' GetIpamResourceCidrsResponse Prelude.Int
getIpamResourceCidrsResponse_httpStatus = Lens.lens (\GetIpamResourceCidrsResponse' {httpStatus} -> httpStatus) (\s@GetIpamResourceCidrsResponse' {} a -> s {httpStatus = a} :: GetIpamResourceCidrsResponse)

instance Prelude.NFData GetIpamResourceCidrsResponse where
  rnf GetIpamResourceCidrsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf ipamResourceCidrs
      `Prelude.seq` Prelude.rnf httpStatus
