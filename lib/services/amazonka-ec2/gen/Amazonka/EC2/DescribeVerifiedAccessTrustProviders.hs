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
-- Module      : Amazonka.EC2.DescribeVerifiedAccessTrustProviders
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe details of existing Verified Access trust providers.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeVerifiedAccessTrustProviders
  ( -- * Creating a Request
    DescribeVerifiedAccessTrustProviders (..),
    newDescribeVerifiedAccessTrustProviders,

    -- * Request Lenses
    describeVerifiedAccessTrustProviders_dryRun,
    describeVerifiedAccessTrustProviders_filters,
    describeVerifiedAccessTrustProviders_maxResults,
    describeVerifiedAccessTrustProviders_nextToken,
    describeVerifiedAccessTrustProviders_verifiedAccessTrustProviderIds,

    -- * Destructuring the Response
    DescribeVerifiedAccessTrustProvidersResponse (..),
    newDescribeVerifiedAccessTrustProvidersResponse,

    -- * Response Lenses
    describeVerifiedAccessTrustProvidersResponse_nextToken,
    describeVerifiedAccessTrustProvidersResponse_verifiedAccessTrustProviders,
    describeVerifiedAccessTrustProvidersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeVerifiedAccessTrustProviders' smart constructor.
data DescribeVerifiedAccessTrustProviders = DescribeVerifiedAccessTrustProviders'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters. Filter names and values are case-sensitive.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the Amazon Web Services Verified Access trust providers.
    verifiedAccessTrustProviderIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVerifiedAccessTrustProviders' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeVerifiedAccessTrustProviders_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeVerifiedAccessTrustProviders_filters' - One or more filters. Filter names and values are case-sensitive.
--
-- 'maxResults', 'describeVerifiedAccessTrustProviders_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'describeVerifiedAccessTrustProviders_nextToken' - The token for the next page of results.
--
-- 'verifiedAccessTrustProviderIds', 'describeVerifiedAccessTrustProviders_verifiedAccessTrustProviderIds' - The IDs of the Amazon Web Services Verified Access trust providers.
newDescribeVerifiedAccessTrustProviders ::
  DescribeVerifiedAccessTrustProviders
newDescribeVerifiedAccessTrustProviders =
  DescribeVerifiedAccessTrustProviders'
    { dryRun =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      verifiedAccessTrustProviderIds =
        Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeVerifiedAccessTrustProviders_dryRun :: Lens.Lens' DescribeVerifiedAccessTrustProviders (Prelude.Maybe Prelude.Bool)
describeVerifiedAccessTrustProviders_dryRun = Lens.lens (\DescribeVerifiedAccessTrustProviders' {dryRun} -> dryRun) (\s@DescribeVerifiedAccessTrustProviders' {} a -> s {dryRun = a} :: DescribeVerifiedAccessTrustProviders)

-- | One or more filters. Filter names and values are case-sensitive.
describeVerifiedAccessTrustProviders_filters :: Lens.Lens' DescribeVerifiedAccessTrustProviders (Prelude.Maybe [Filter])
describeVerifiedAccessTrustProviders_filters = Lens.lens (\DescribeVerifiedAccessTrustProviders' {filters} -> filters) (\s@DescribeVerifiedAccessTrustProviders' {} a -> s {filters = a} :: DescribeVerifiedAccessTrustProviders) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeVerifiedAccessTrustProviders_maxResults :: Lens.Lens' DescribeVerifiedAccessTrustProviders (Prelude.Maybe Prelude.Natural)
describeVerifiedAccessTrustProviders_maxResults = Lens.lens (\DescribeVerifiedAccessTrustProviders' {maxResults} -> maxResults) (\s@DescribeVerifiedAccessTrustProviders' {} a -> s {maxResults = a} :: DescribeVerifiedAccessTrustProviders)

-- | The token for the next page of results.
describeVerifiedAccessTrustProviders_nextToken :: Lens.Lens' DescribeVerifiedAccessTrustProviders (Prelude.Maybe Prelude.Text)
describeVerifiedAccessTrustProviders_nextToken = Lens.lens (\DescribeVerifiedAccessTrustProviders' {nextToken} -> nextToken) (\s@DescribeVerifiedAccessTrustProviders' {} a -> s {nextToken = a} :: DescribeVerifiedAccessTrustProviders)

-- | The IDs of the Amazon Web Services Verified Access trust providers.
describeVerifiedAccessTrustProviders_verifiedAccessTrustProviderIds :: Lens.Lens' DescribeVerifiedAccessTrustProviders (Prelude.Maybe [Prelude.Text])
describeVerifiedAccessTrustProviders_verifiedAccessTrustProviderIds = Lens.lens (\DescribeVerifiedAccessTrustProviders' {verifiedAccessTrustProviderIds} -> verifiedAccessTrustProviderIds) (\s@DescribeVerifiedAccessTrustProviders' {} a -> s {verifiedAccessTrustProviderIds = a} :: DescribeVerifiedAccessTrustProviders) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSPager
    DescribeVerifiedAccessTrustProviders
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeVerifiedAccessTrustProvidersResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeVerifiedAccessTrustProvidersResponse_verifiedAccessTrustProviders
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeVerifiedAccessTrustProviders_nextToken
          Lens..~ rs
          Lens.^? describeVerifiedAccessTrustProvidersResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeVerifiedAccessTrustProviders
  where
  type
    AWSResponse DescribeVerifiedAccessTrustProviders =
      DescribeVerifiedAccessTrustProvidersResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVerifiedAccessTrustProvidersResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x
                            Data..@? "verifiedAccessTrustProviderSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeVerifiedAccessTrustProviders
  where
  hashWithSalt
    _salt
    DescribeVerifiedAccessTrustProviders' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` verifiedAccessTrustProviderIds

instance
  Prelude.NFData
    DescribeVerifiedAccessTrustProviders
  where
  rnf DescribeVerifiedAccessTrustProviders' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf verifiedAccessTrustProviderIds

instance
  Data.ToHeaders
    DescribeVerifiedAccessTrustProviders
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeVerifiedAccessTrustProviders
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeVerifiedAccessTrustProviders
  where
  toQuery DescribeVerifiedAccessTrustProviders' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeVerifiedAccessTrustProviders" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        Data.toQuery
          ( Data.toQueryList "VerifiedAccessTrustProviderId"
              Prelude.<$> verifiedAccessTrustProviderIds
          )
      ]

-- | /See:/ 'newDescribeVerifiedAccessTrustProvidersResponse' smart constructor.
data DescribeVerifiedAccessTrustProvidersResponse = DescribeVerifiedAccessTrustProvidersResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the Amazon Web Services Verified Access trust providers.
    verifiedAccessTrustProviders :: Prelude.Maybe [VerifiedAccessTrustProvider],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVerifiedAccessTrustProvidersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeVerifiedAccessTrustProvidersResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'verifiedAccessTrustProviders', 'describeVerifiedAccessTrustProvidersResponse_verifiedAccessTrustProviders' - The IDs of the Amazon Web Services Verified Access trust providers.
--
-- 'httpStatus', 'describeVerifiedAccessTrustProvidersResponse_httpStatus' - The response's http status code.
newDescribeVerifiedAccessTrustProvidersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeVerifiedAccessTrustProvidersResponse
newDescribeVerifiedAccessTrustProvidersResponse
  pHttpStatus_ =
    DescribeVerifiedAccessTrustProvidersResponse'
      { nextToken =
          Prelude.Nothing,
        verifiedAccessTrustProviders =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeVerifiedAccessTrustProvidersResponse_nextToken :: Lens.Lens' DescribeVerifiedAccessTrustProvidersResponse (Prelude.Maybe Prelude.Text)
describeVerifiedAccessTrustProvidersResponse_nextToken = Lens.lens (\DescribeVerifiedAccessTrustProvidersResponse' {nextToken} -> nextToken) (\s@DescribeVerifiedAccessTrustProvidersResponse' {} a -> s {nextToken = a} :: DescribeVerifiedAccessTrustProvidersResponse)

-- | The IDs of the Amazon Web Services Verified Access trust providers.
describeVerifiedAccessTrustProvidersResponse_verifiedAccessTrustProviders :: Lens.Lens' DescribeVerifiedAccessTrustProvidersResponse (Prelude.Maybe [VerifiedAccessTrustProvider])
describeVerifiedAccessTrustProvidersResponse_verifiedAccessTrustProviders = Lens.lens (\DescribeVerifiedAccessTrustProvidersResponse' {verifiedAccessTrustProviders} -> verifiedAccessTrustProviders) (\s@DescribeVerifiedAccessTrustProvidersResponse' {} a -> s {verifiedAccessTrustProviders = a} :: DescribeVerifiedAccessTrustProvidersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeVerifiedAccessTrustProvidersResponse_httpStatus :: Lens.Lens' DescribeVerifiedAccessTrustProvidersResponse Prelude.Int
describeVerifiedAccessTrustProvidersResponse_httpStatus = Lens.lens (\DescribeVerifiedAccessTrustProvidersResponse' {httpStatus} -> httpStatus) (\s@DescribeVerifiedAccessTrustProvidersResponse' {} a -> s {httpStatus = a} :: DescribeVerifiedAccessTrustProvidersResponse)

instance
  Prelude.NFData
    DescribeVerifiedAccessTrustProvidersResponse
  where
  rnf DescribeVerifiedAccessTrustProvidersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf verifiedAccessTrustProviders
      `Prelude.seq` Prelude.rnf httpStatus
