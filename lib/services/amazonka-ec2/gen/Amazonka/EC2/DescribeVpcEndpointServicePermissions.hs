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
-- Module      : Amazonka.EC2.DescribeVpcEndpointServicePermissions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the principals (service consumers) that are permitted to
-- discover your VPC endpoint service.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeVpcEndpointServicePermissions
  ( -- * Creating a Request
    DescribeVpcEndpointServicePermissions (..),
    newDescribeVpcEndpointServicePermissions,

    -- * Request Lenses
    describeVpcEndpointServicePermissions_dryRun,
    describeVpcEndpointServicePermissions_filters,
    describeVpcEndpointServicePermissions_maxResults,
    describeVpcEndpointServicePermissions_nextToken,
    describeVpcEndpointServicePermissions_serviceId,

    -- * Destructuring the Response
    DescribeVpcEndpointServicePermissionsResponse (..),
    newDescribeVpcEndpointServicePermissionsResponse,

    -- * Response Lenses
    describeVpcEndpointServicePermissionsResponse_allowedPrincipals,
    describeVpcEndpointServicePermissionsResponse_nextToken,
    describeVpcEndpointServicePermissionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeVpcEndpointServicePermissions' smart constructor.
data DescribeVpcEndpointServicePermissions = DescribeVpcEndpointServicePermissions'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters.
    --
    -- -   @principal@ - The ARN of the principal.
    --
    -- -   @principal-type@ - The principal type (@All@ | @Service@ |
    --     @OrganizationUnit@ | @Account@ | @User@ | @Role@).
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return for the request in a single
    -- page. The remaining results of the initial request can be seen by
    -- sending another request with the returned @NextToken@ value. This value
    -- can be between 5 and 1,000; if @MaxResults@ is given a value larger than
    -- 1,000, only 1,000 results are returned.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The token to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the service.
    serviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVpcEndpointServicePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeVpcEndpointServicePermissions_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeVpcEndpointServicePermissions_filters' - One or more filters.
--
-- -   @principal@ - The ARN of the principal.
--
-- -   @principal-type@ - The principal type (@All@ | @Service@ |
--     @OrganizationUnit@ | @Account@ | @User@ | @Role@).
--
-- 'maxResults', 'describeVpcEndpointServicePermissions_maxResults' - The maximum number of results to return for the request in a single
-- page. The remaining results of the initial request can be seen by
-- sending another request with the returned @NextToken@ value. This value
-- can be between 5 and 1,000; if @MaxResults@ is given a value larger than
-- 1,000, only 1,000 results are returned.
--
-- 'nextToken', 'describeVpcEndpointServicePermissions_nextToken' - The token to retrieve the next page of results.
--
-- 'serviceId', 'describeVpcEndpointServicePermissions_serviceId' - The ID of the service.
newDescribeVpcEndpointServicePermissions ::
  -- | 'serviceId'
  Prelude.Text ->
  DescribeVpcEndpointServicePermissions
newDescribeVpcEndpointServicePermissions pServiceId_ =
  DescribeVpcEndpointServicePermissions'
    { dryRun =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      serviceId = pServiceId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeVpcEndpointServicePermissions_dryRun :: Lens.Lens' DescribeVpcEndpointServicePermissions (Prelude.Maybe Prelude.Bool)
describeVpcEndpointServicePermissions_dryRun = Lens.lens (\DescribeVpcEndpointServicePermissions' {dryRun} -> dryRun) (\s@DescribeVpcEndpointServicePermissions' {} a -> s {dryRun = a} :: DescribeVpcEndpointServicePermissions)

-- | One or more filters.
--
-- -   @principal@ - The ARN of the principal.
--
-- -   @principal-type@ - The principal type (@All@ | @Service@ |
--     @OrganizationUnit@ | @Account@ | @User@ | @Role@).
describeVpcEndpointServicePermissions_filters :: Lens.Lens' DescribeVpcEndpointServicePermissions (Prelude.Maybe [Filter])
describeVpcEndpointServicePermissions_filters = Lens.lens (\DescribeVpcEndpointServicePermissions' {filters} -> filters) (\s@DescribeVpcEndpointServicePermissions' {} a -> s {filters = a} :: DescribeVpcEndpointServicePermissions) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return for the request in a single
-- page. The remaining results of the initial request can be seen by
-- sending another request with the returned @NextToken@ value. This value
-- can be between 5 and 1,000; if @MaxResults@ is given a value larger than
-- 1,000, only 1,000 results are returned.
describeVpcEndpointServicePermissions_maxResults :: Lens.Lens' DescribeVpcEndpointServicePermissions (Prelude.Maybe Prelude.Int)
describeVpcEndpointServicePermissions_maxResults = Lens.lens (\DescribeVpcEndpointServicePermissions' {maxResults} -> maxResults) (\s@DescribeVpcEndpointServicePermissions' {} a -> s {maxResults = a} :: DescribeVpcEndpointServicePermissions)

-- | The token to retrieve the next page of results.
describeVpcEndpointServicePermissions_nextToken :: Lens.Lens' DescribeVpcEndpointServicePermissions (Prelude.Maybe Prelude.Text)
describeVpcEndpointServicePermissions_nextToken = Lens.lens (\DescribeVpcEndpointServicePermissions' {nextToken} -> nextToken) (\s@DescribeVpcEndpointServicePermissions' {} a -> s {nextToken = a} :: DescribeVpcEndpointServicePermissions)

-- | The ID of the service.
describeVpcEndpointServicePermissions_serviceId :: Lens.Lens' DescribeVpcEndpointServicePermissions Prelude.Text
describeVpcEndpointServicePermissions_serviceId = Lens.lens (\DescribeVpcEndpointServicePermissions' {serviceId} -> serviceId) (\s@DescribeVpcEndpointServicePermissions' {} a -> s {serviceId = a} :: DescribeVpcEndpointServicePermissions)

instance
  Core.AWSPager
    DescribeVpcEndpointServicePermissions
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeVpcEndpointServicePermissionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeVpcEndpointServicePermissionsResponse_allowedPrincipals
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeVpcEndpointServicePermissions_nextToken
              Lens..~ rs
              Lens.^? describeVpcEndpointServicePermissionsResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeVpcEndpointServicePermissions
  where
  type
    AWSResponse
      DescribeVpcEndpointServicePermissions =
      DescribeVpcEndpointServicePermissionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVpcEndpointServicePermissionsResponse'
            Prelude.<$> ( x
                            Data..@? "allowedPrincipals"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeVpcEndpointServicePermissions
  where
  hashWithSalt
    _salt
    DescribeVpcEndpointServicePermissions' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` serviceId

instance
  Prelude.NFData
    DescribeVpcEndpointServicePermissions
  where
  rnf DescribeVpcEndpointServicePermissions' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf filters `Prelude.seq`
        Prelude.rnf maxResults `Prelude.seq`
          Prelude.rnf nextToken `Prelude.seq`
            Prelude.rnf serviceId

instance
  Data.ToHeaders
    DescribeVpcEndpointServicePermissions
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeVpcEndpointServicePermissions
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeVpcEndpointServicePermissions
  where
  toQuery DescribeVpcEndpointServicePermissions' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeVpcEndpointServicePermissions" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "ServiceId" Data.=: serviceId
      ]

-- | /See:/ 'newDescribeVpcEndpointServicePermissionsResponse' smart constructor.
data DescribeVpcEndpointServicePermissionsResponse = DescribeVpcEndpointServicePermissionsResponse'
  { -- | Information about one or more allowed principals.
    allowedPrincipals :: Prelude.Maybe [AllowedPrincipal],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVpcEndpointServicePermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedPrincipals', 'describeVpcEndpointServicePermissionsResponse_allowedPrincipals' - Information about one or more allowed principals.
--
-- 'nextToken', 'describeVpcEndpointServicePermissionsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeVpcEndpointServicePermissionsResponse_httpStatus' - The response's http status code.
newDescribeVpcEndpointServicePermissionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeVpcEndpointServicePermissionsResponse
newDescribeVpcEndpointServicePermissionsResponse
  pHttpStatus_ =
    DescribeVpcEndpointServicePermissionsResponse'
      { allowedPrincipals =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about one or more allowed principals.
describeVpcEndpointServicePermissionsResponse_allowedPrincipals :: Lens.Lens' DescribeVpcEndpointServicePermissionsResponse (Prelude.Maybe [AllowedPrincipal])
describeVpcEndpointServicePermissionsResponse_allowedPrincipals = Lens.lens (\DescribeVpcEndpointServicePermissionsResponse' {allowedPrincipals} -> allowedPrincipals) (\s@DescribeVpcEndpointServicePermissionsResponse' {} a -> s {allowedPrincipals = a} :: DescribeVpcEndpointServicePermissionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeVpcEndpointServicePermissionsResponse_nextToken :: Lens.Lens' DescribeVpcEndpointServicePermissionsResponse (Prelude.Maybe Prelude.Text)
describeVpcEndpointServicePermissionsResponse_nextToken = Lens.lens (\DescribeVpcEndpointServicePermissionsResponse' {nextToken} -> nextToken) (\s@DescribeVpcEndpointServicePermissionsResponse' {} a -> s {nextToken = a} :: DescribeVpcEndpointServicePermissionsResponse)

-- | The response's http status code.
describeVpcEndpointServicePermissionsResponse_httpStatus :: Lens.Lens' DescribeVpcEndpointServicePermissionsResponse Prelude.Int
describeVpcEndpointServicePermissionsResponse_httpStatus = Lens.lens (\DescribeVpcEndpointServicePermissionsResponse' {httpStatus} -> httpStatus) (\s@DescribeVpcEndpointServicePermissionsResponse' {} a -> s {httpStatus = a} :: DescribeVpcEndpointServicePermissionsResponse)

instance
  Prelude.NFData
    DescribeVpcEndpointServicePermissionsResponse
  where
  rnf
    DescribeVpcEndpointServicePermissionsResponse' {..} =
      Prelude.rnf allowedPrincipals `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf httpStatus
