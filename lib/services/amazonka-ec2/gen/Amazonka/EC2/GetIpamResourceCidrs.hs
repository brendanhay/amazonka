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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns resource CIDRs managed by IPAM in a given scope. If an IPAM is
-- associated with more than one resource discovery, the resource CIDRs
-- across all of the resource discoveries is returned. A resource discovery
-- is an IPAM component that enables IPAM to manage and monitor resources
-- that belong to the owning account.
--
-- This operation returns paginated results.
module Amazonka.EC2.GetIpamResourceCidrs
  ( -- * Creating a Request
    GetIpamResourceCidrs (..),
    newGetIpamResourceCidrs,

    -- * Request Lenses
    getIpamResourceCidrs_dryRun,
    getIpamResourceCidrs_filters,
    getIpamResourceCidrs_ipamPoolId,
    getIpamResourceCidrs_maxResults,
    getIpamResourceCidrs_nextToken,
    getIpamResourceCidrs_resourceId,
    getIpamResourceCidrs_resourceOwner,
    getIpamResourceCidrs_resourceTag,
    getIpamResourceCidrs_resourceType,
    getIpamResourceCidrs_ipamScopeId,

    -- * Destructuring the Response
    GetIpamResourceCidrsResponse (..),
    newGetIpamResourceCidrsResponse,

    -- * Response Lenses
    getIpamResourceCidrsResponse_ipamResourceCidrs,
    getIpamResourceCidrsResponse_nextToken,
    getIpamResourceCidrsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetIpamResourceCidrs' smart constructor.
data GetIpamResourceCidrs = GetIpamResourceCidrs'
  { -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters for the request. For more information about
    -- filtering, see
    -- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-filter.html Filtering CLI output>.
    filters :: Prelude.Maybe [Filter],
    -- | The ID of the IPAM pool that the resource is in.
    ipamPoolId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in the request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the resource.
    resourceOwner :: Prelude.Maybe Prelude.Text,
    -- | The resource tag.
    resourceTag :: Prelude.Maybe RequestIpamResourceTag,
    -- | The resource type.
    resourceType :: Prelude.Maybe IpamResourceType,
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
-- 'dryRun', 'getIpamResourceCidrs_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'getIpamResourceCidrs_filters' - One or more filters for the request. For more information about
-- filtering, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-filter.html Filtering CLI output>.
--
-- 'ipamPoolId', 'getIpamResourceCidrs_ipamPoolId' - The ID of the IPAM pool that the resource is in.
--
-- 'maxResults', 'getIpamResourceCidrs_maxResults' - The maximum number of results to return in the request.
--
-- 'nextToken', 'getIpamResourceCidrs_nextToken' - The token for the next page of results.
--
-- 'resourceId', 'getIpamResourceCidrs_resourceId' - The ID of the resource.
--
-- 'resourceOwner', 'getIpamResourceCidrs_resourceOwner' - The ID of the Amazon Web Services account that owns the resource.
--
-- 'resourceTag', 'getIpamResourceCidrs_resourceTag' - The resource tag.
--
-- 'resourceType', 'getIpamResourceCidrs_resourceType' - The resource type.
--
-- 'ipamScopeId', 'getIpamResourceCidrs_ipamScopeId' - The ID of the scope that the resource is in.
newGetIpamResourceCidrs ::
  -- | 'ipamScopeId'
  Prelude.Text ->
  GetIpamResourceCidrs
newGetIpamResourceCidrs pIpamScopeId_ =
  GetIpamResourceCidrs'
    { dryRun = Prelude.Nothing,
      filters = Prelude.Nothing,
      ipamPoolId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      resourceOwner = Prelude.Nothing,
      resourceTag = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      ipamScopeId = pIpamScopeId_
    }

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
getIpamResourceCidrs_dryRun :: Lens.Lens' GetIpamResourceCidrs (Prelude.Maybe Prelude.Bool)
getIpamResourceCidrs_dryRun = Lens.lens (\GetIpamResourceCidrs' {dryRun} -> dryRun) (\s@GetIpamResourceCidrs' {} a -> s {dryRun = a} :: GetIpamResourceCidrs)

-- | One or more filters for the request. For more information about
-- filtering, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-filter.html Filtering CLI output>.
getIpamResourceCidrs_filters :: Lens.Lens' GetIpamResourceCidrs (Prelude.Maybe [Filter])
getIpamResourceCidrs_filters = Lens.lens (\GetIpamResourceCidrs' {filters} -> filters) (\s@GetIpamResourceCidrs' {} a -> s {filters = a} :: GetIpamResourceCidrs) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the IPAM pool that the resource is in.
getIpamResourceCidrs_ipamPoolId :: Lens.Lens' GetIpamResourceCidrs (Prelude.Maybe Prelude.Text)
getIpamResourceCidrs_ipamPoolId = Lens.lens (\GetIpamResourceCidrs' {ipamPoolId} -> ipamPoolId) (\s@GetIpamResourceCidrs' {} a -> s {ipamPoolId = a} :: GetIpamResourceCidrs)

-- | The maximum number of results to return in the request.
getIpamResourceCidrs_maxResults :: Lens.Lens' GetIpamResourceCidrs (Prelude.Maybe Prelude.Natural)
getIpamResourceCidrs_maxResults = Lens.lens (\GetIpamResourceCidrs' {maxResults} -> maxResults) (\s@GetIpamResourceCidrs' {} a -> s {maxResults = a} :: GetIpamResourceCidrs)

-- | The token for the next page of results.
getIpamResourceCidrs_nextToken :: Lens.Lens' GetIpamResourceCidrs (Prelude.Maybe Prelude.Text)
getIpamResourceCidrs_nextToken = Lens.lens (\GetIpamResourceCidrs' {nextToken} -> nextToken) (\s@GetIpamResourceCidrs' {} a -> s {nextToken = a} :: GetIpamResourceCidrs)

-- | The ID of the resource.
getIpamResourceCidrs_resourceId :: Lens.Lens' GetIpamResourceCidrs (Prelude.Maybe Prelude.Text)
getIpamResourceCidrs_resourceId = Lens.lens (\GetIpamResourceCidrs' {resourceId} -> resourceId) (\s@GetIpamResourceCidrs' {} a -> s {resourceId = a} :: GetIpamResourceCidrs)

-- | The ID of the Amazon Web Services account that owns the resource.
getIpamResourceCidrs_resourceOwner :: Lens.Lens' GetIpamResourceCidrs (Prelude.Maybe Prelude.Text)
getIpamResourceCidrs_resourceOwner = Lens.lens (\GetIpamResourceCidrs' {resourceOwner} -> resourceOwner) (\s@GetIpamResourceCidrs' {} a -> s {resourceOwner = a} :: GetIpamResourceCidrs)

-- | The resource tag.
getIpamResourceCidrs_resourceTag :: Lens.Lens' GetIpamResourceCidrs (Prelude.Maybe RequestIpamResourceTag)
getIpamResourceCidrs_resourceTag = Lens.lens (\GetIpamResourceCidrs' {resourceTag} -> resourceTag) (\s@GetIpamResourceCidrs' {} a -> s {resourceTag = a} :: GetIpamResourceCidrs)

-- | The resource type.
getIpamResourceCidrs_resourceType :: Lens.Lens' GetIpamResourceCidrs (Prelude.Maybe IpamResourceType)
getIpamResourceCidrs_resourceType = Lens.lens (\GetIpamResourceCidrs' {resourceType} -> resourceType) (\s@GetIpamResourceCidrs' {} a -> s {resourceType = a} :: GetIpamResourceCidrs)

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
        Prelude.Just
          Prelude.$ rq
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
            Prelude.<$> ( x
                            Data..@? "ipamResourceCidrSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetIpamResourceCidrs where
  hashWithSalt _salt GetIpamResourceCidrs' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` ipamPoolId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceOwner
      `Prelude.hashWithSalt` resourceTag
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` ipamScopeId

instance Prelude.NFData GetIpamResourceCidrs where
  rnf GetIpamResourceCidrs' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf ipamPoolId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceOwner
      `Prelude.seq` Prelude.rnf resourceTag
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf ipamScopeId

instance Data.ToHeaders GetIpamResourceCidrs where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetIpamResourceCidrs where
  toPath = Prelude.const "/"

instance Data.ToQuery GetIpamResourceCidrs where
  toQuery GetIpamResourceCidrs' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("GetIpamResourceCidrs" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "IpamPoolId" Data.=: ipamPoolId,
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "ResourceId" Data.=: resourceId,
        "ResourceOwner" Data.=: resourceOwner,
        "ResourceTag" Data.=: resourceTag,
        "ResourceType" Data.=: resourceType,
        "IpamScopeId" Data.=: ipamScopeId
      ]

-- | /See:/ 'newGetIpamResourceCidrsResponse' smart constructor.
data GetIpamResourceCidrsResponse = GetIpamResourceCidrsResponse'
  { -- | The resource CIDRs.
    ipamResourceCidrs :: Prelude.Maybe [IpamResourceCidr],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'ipamResourceCidrs', 'getIpamResourceCidrsResponse_ipamResourceCidrs' - The resource CIDRs.
--
-- 'nextToken', 'getIpamResourceCidrsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'getIpamResourceCidrsResponse_httpStatus' - The response's http status code.
newGetIpamResourceCidrsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetIpamResourceCidrsResponse
newGetIpamResourceCidrsResponse pHttpStatus_ =
  GetIpamResourceCidrsResponse'
    { ipamResourceCidrs =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The resource CIDRs.
getIpamResourceCidrsResponse_ipamResourceCidrs :: Lens.Lens' GetIpamResourceCidrsResponse (Prelude.Maybe [IpamResourceCidr])
getIpamResourceCidrsResponse_ipamResourceCidrs = Lens.lens (\GetIpamResourceCidrsResponse' {ipamResourceCidrs} -> ipamResourceCidrs) (\s@GetIpamResourceCidrsResponse' {} a -> s {ipamResourceCidrs = a} :: GetIpamResourceCidrsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
getIpamResourceCidrsResponse_nextToken :: Lens.Lens' GetIpamResourceCidrsResponse (Prelude.Maybe Prelude.Text)
getIpamResourceCidrsResponse_nextToken = Lens.lens (\GetIpamResourceCidrsResponse' {nextToken} -> nextToken) (\s@GetIpamResourceCidrsResponse' {} a -> s {nextToken = a} :: GetIpamResourceCidrsResponse)

-- | The response's http status code.
getIpamResourceCidrsResponse_httpStatus :: Lens.Lens' GetIpamResourceCidrsResponse Prelude.Int
getIpamResourceCidrsResponse_httpStatus = Lens.lens (\GetIpamResourceCidrsResponse' {httpStatus} -> httpStatus) (\s@GetIpamResourceCidrsResponse' {} a -> s {httpStatus = a} :: GetIpamResourceCidrsResponse)

instance Prelude.NFData GetIpamResourceCidrsResponse where
  rnf GetIpamResourceCidrsResponse' {..} =
    Prelude.rnf ipamResourceCidrs
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
