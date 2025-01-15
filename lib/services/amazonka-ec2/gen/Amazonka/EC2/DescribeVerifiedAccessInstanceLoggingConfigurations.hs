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
-- Module      : Amazonka.EC2.DescribeVerifiedAccessInstanceLoggingConfigurations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the current logging configuration for the Amazon Web Services
-- Verified Access instances.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeVerifiedAccessInstanceLoggingConfigurations
  ( -- * Creating a Request
    DescribeVerifiedAccessInstanceLoggingConfigurations (..),
    newDescribeVerifiedAccessInstanceLoggingConfigurations,

    -- * Request Lenses
    describeVerifiedAccessInstanceLoggingConfigurations_dryRun,
    describeVerifiedAccessInstanceLoggingConfigurations_filters,
    describeVerifiedAccessInstanceLoggingConfigurations_maxResults,
    describeVerifiedAccessInstanceLoggingConfigurations_nextToken,
    describeVerifiedAccessInstanceLoggingConfigurations_verifiedAccessInstanceIds,

    -- * Destructuring the Response
    DescribeVerifiedAccessInstanceLoggingConfigurationsResponse (..),
    newDescribeVerifiedAccessInstanceLoggingConfigurationsResponse,

    -- * Response Lenses
    describeVerifiedAccessInstanceLoggingConfigurationsResponse_loggingConfigurations,
    describeVerifiedAccessInstanceLoggingConfigurationsResponse_nextToken,
    describeVerifiedAccessInstanceLoggingConfigurationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeVerifiedAccessInstanceLoggingConfigurations' smart constructor.
data DescribeVerifiedAccessInstanceLoggingConfigurations = DescribeVerifiedAccessInstanceLoggingConfigurations'
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
    -- | The IDs of the Amazon Web Services Verified Access instances.
    verifiedAccessInstanceIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVerifiedAccessInstanceLoggingConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeVerifiedAccessInstanceLoggingConfigurations_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeVerifiedAccessInstanceLoggingConfigurations_filters' - One or more filters. Filter names and values are case-sensitive.
--
-- 'maxResults', 'describeVerifiedAccessInstanceLoggingConfigurations_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'describeVerifiedAccessInstanceLoggingConfigurations_nextToken' - The token for the next page of results.
--
-- 'verifiedAccessInstanceIds', 'describeVerifiedAccessInstanceLoggingConfigurations_verifiedAccessInstanceIds' - The IDs of the Amazon Web Services Verified Access instances.
newDescribeVerifiedAccessInstanceLoggingConfigurations ::
  DescribeVerifiedAccessInstanceLoggingConfigurations
newDescribeVerifiedAccessInstanceLoggingConfigurations =
  DescribeVerifiedAccessInstanceLoggingConfigurations'
    { dryRun =
        Prelude.Nothing,
      filters =
        Prelude.Nothing,
      maxResults =
        Prelude.Nothing,
      nextToken =
        Prelude.Nothing,
      verifiedAccessInstanceIds =
        Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeVerifiedAccessInstanceLoggingConfigurations_dryRun :: Lens.Lens' DescribeVerifiedAccessInstanceLoggingConfigurations (Prelude.Maybe Prelude.Bool)
describeVerifiedAccessInstanceLoggingConfigurations_dryRun = Lens.lens (\DescribeVerifiedAccessInstanceLoggingConfigurations' {dryRun} -> dryRun) (\s@DescribeVerifiedAccessInstanceLoggingConfigurations' {} a -> s {dryRun = a} :: DescribeVerifiedAccessInstanceLoggingConfigurations)

-- | One or more filters. Filter names and values are case-sensitive.
describeVerifiedAccessInstanceLoggingConfigurations_filters :: Lens.Lens' DescribeVerifiedAccessInstanceLoggingConfigurations (Prelude.Maybe [Filter])
describeVerifiedAccessInstanceLoggingConfigurations_filters = Lens.lens (\DescribeVerifiedAccessInstanceLoggingConfigurations' {filters} -> filters) (\s@DescribeVerifiedAccessInstanceLoggingConfigurations' {} a -> s {filters = a} :: DescribeVerifiedAccessInstanceLoggingConfigurations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeVerifiedAccessInstanceLoggingConfigurations_maxResults :: Lens.Lens' DescribeVerifiedAccessInstanceLoggingConfigurations (Prelude.Maybe Prelude.Natural)
describeVerifiedAccessInstanceLoggingConfigurations_maxResults = Lens.lens (\DescribeVerifiedAccessInstanceLoggingConfigurations' {maxResults} -> maxResults) (\s@DescribeVerifiedAccessInstanceLoggingConfigurations' {} a -> s {maxResults = a} :: DescribeVerifiedAccessInstanceLoggingConfigurations)

-- | The token for the next page of results.
describeVerifiedAccessInstanceLoggingConfigurations_nextToken :: Lens.Lens' DescribeVerifiedAccessInstanceLoggingConfigurations (Prelude.Maybe Prelude.Text)
describeVerifiedAccessInstanceLoggingConfigurations_nextToken = Lens.lens (\DescribeVerifiedAccessInstanceLoggingConfigurations' {nextToken} -> nextToken) (\s@DescribeVerifiedAccessInstanceLoggingConfigurations' {} a -> s {nextToken = a} :: DescribeVerifiedAccessInstanceLoggingConfigurations)

-- | The IDs of the Amazon Web Services Verified Access instances.
describeVerifiedAccessInstanceLoggingConfigurations_verifiedAccessInstanceIds :: Lens.Lens' DescribeVerifiedAccessInstanceLoggingConfigurations (Prelude.Maybe [Prelude.Text])
describeVerifiedAccessInstanceLoggingConfigurations_verifiedAccessInstanceIds = Lens.lens (\DescribeVerifiedAccessInstanceLoggingConfigurations' {verifiedAccessInstanceIds} -> verifiedAccessInstanceIds) (\s@DescribeVerifiedAccessInstanceLoggingConfigurations' {} a -> s {verifiedAccessInstanceIds = a} :: DescribeVerifiedAccessInstanceLoggingConfigurations) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSPager
    DescribeVerifiedAccessInstanceLoggingConfigurations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeVerifiedAccessInstanceLoggingConfigurationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeVerifiedAccessInstanceLoggingConfigurationsResponse_loggingConfigurations
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeVerifiedAccessInstanceLoggingConfigurations_nextToken
              Lens..~ rs
              Lens.^? describeVerifiedAccessInstanceLoggingConfigurationsResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeVerifiedAccessInstanceLoggingConfigurations
  where
  type
    AWSResponse
      DescribeVerifiedAccessInstanceLoggingConfigurations =
      DescribeVerifiedAccessInstanceLoggingConfigurationsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVerifiedAccessInstanceLoggingConfigurationsResponse'
            Prelude.<$> ( x
                            Data..@? "loggingConfigurationSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeVerifiedAccessInstanceLoggingConfigurations
  where
  hashWithSalt
    _salt
    DescribeVerifiedAccessInstanceLoggingConfigurations' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` verifiedAccessInstanceIds

instance
  Prelude.NFData
    DescribeVerifiedAccessInstanceLoggingConfigurations
  where
  rnf
    DescribeVerifiedAccessInstanceLoggingConfigurations' {..} =
      Prelude.rnf dryRun `Prelude.seq`
        Prelude.rnf filters `Prelude.seq`
          Prelude.rnf maxResults `Prelude.seq`
            Prelude.rnf nextToken `Prelude.seq`
              Prelude.rnf verifiedAccessInstanceIds

instance
  Data.ToHeaders
    DescribeVerifiedAccessInstanceLoggingConfigurations
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeVerifiedAccessInstanceLoggingConfigurations
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeVerifiedAccessInstanceLoggingConfigurations
  where
  toQuery
    DescribeVerifiedAccessInstanceLoggingConfigurations' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "DescribeVerifiedAccessInstanceLoggingConfigurations" ::
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
            ( Data.toQueryList "VerifiedAccessInstanceId"
                Prelude.<$> verifiedAccessInstanceIds
            )
        ]

-- | /See:/ 'newDescribeVerifiedAccessInstanceLoggingConfigurationsResponse' smart constructor.
data DescribeVerifiedAccessInstanceLoggingConfigurationsResponse = DescribeVerifiedAccessInstanceLoggingConfigurationsResponse'
  { -- | The current logging configuration for the Amazon Web Services Verified
    -- Access instances.
    loggingConfigurations :: Prelude.Maybe [VerifiedAccessInstanceLoggingConfiguration],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVerifiedAccessInstanceLoggingConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loggingConfigurations', 'describeVerifiedAccessInstanceLoggingConfigurationsResponse_loggingConfigurations' - The current logging configuration for the Amazon Web Services Verified
-- Access instances.
--
-- 'nextToken', 'describeVerifiedAccessInstanceLoggingConfigurationsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeVerifiedAccessInstanceLoggingConfigurationsResponse_httpStatus' - The response's http status code.
newDescribeVerifiedAccessInstanceLoggingConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeVerifiedAccessInstanceLoggingConfigurationsResponse
newDescribeVerifiedAccessInstanceLoggingConfigurationsResponse
  pHttpStatus_ =
    DescribeVerifiedAccessInstanceLoggingConfigurationsResponse'
      { loggingConfigurations =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The current logging configuration for the Amazon Web Services Verified
-- Access instances.
describeVerifiedAccessInstanceLoggingConfigurationsResponse_loggingConfigurations :: Lens.Lens' DescribeVerifiedAccessInstanceLoggingConfigurationsResponse (Prelude.Maybe [VerifiedAccessInstanceLoggingConfiguration])
describeVerifiedAccessInstanceLoggingConfigurationsResponse_loggingConfigurations = Lens.lens (\DescribeVerifiedAccessInstanceLoggingConfigurationsResponse' {loggingConfigurations} -> loggingConfigurations) (\s@DescribeVerifiedAccessInstanceLoggingConfigurationsResponse' {} a -> s {loggingConfigurations = a} :: DescribeVerifiedAccessInstanceLoggingConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeVerifiedAccessInstanceLoggingConfigurationsResponse_nextToken :: Lens.Lens' DescribeVerifiedAccessInstanceLoggingConfigurationsResponse (Prelude.Maybe Prelude.Text)
describeVerifiedAccessInstanceLoggingConfigurationsResponse_nextToken = Lens.lens (\DescribeVerifiedAccessInstanceLoggingConfigurationsResponse' {nextToken} -> nextToken) (\s@DescribeVerifiedAccessInstanceLoggingConfigurationsResponse' {} a -> s {nextToken = a} :: DescribeVerifiedAccessInstanceLoggingConfigurationsResponse)

-- | The response's http status code.
describeVerifiedAccessInstanceLoggingConfigurationsResponse_httpStatus :: Lens.Lens' DescribeVerifiedAccessInstanceLoggingConfigurationsResponse Prelude.Int
describeVerifiedAccessInstanceLoggingConfigurationsResponse_httpStatus = Lens.lens (\DescribeVerifiedAccessInstanceLoggingConfigurationsResponse' {httpStatus} -> httpStatus) (\s@DescribeVerifiedAccessInstanceLoggingConfigurationsResponse' {} a -> s {httpStatus = a} :: DescribeVerifiedAccessInstanceLoggingConfigurationsResponse)

instance
  Prelude.NFData
    DescribeVerifiedAccessInstanceLoggingConfigurationsResponse
  where
  rnf
    DescribeVerifiedAccessInstanceLoggingConfigurationsResponse' {..} =
      Prelude.rnf loggingConfigurations `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf httpStatus
