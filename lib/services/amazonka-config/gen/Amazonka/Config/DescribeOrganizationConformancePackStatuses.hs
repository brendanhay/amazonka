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
-- Module      : Amazonka.Config.DescribeOrganizationConformancePackStatuses
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides organization conformance pack deployment status for an
-- organization.
--
-- The status is not considered successful until organization conformance
-- pack is successfully deployed in all the member accounts with an
-- exception of excluded accounts.
--
-- When you specify the limit and the next token, you receive a paginated
-- response. Limit and next token are not applicable if you specify
-- organization conformance pack names. They are only applicable, when you
-- request all the organization conformance packs.
--
-- This operation returns paginated results.
module Amazonka.Config.DescribeOrganizationConformancePackStatuses
  ( -- * Creating a Request
    DescribeOrganizationConformancePackStatuses (..),
    newDescribeOrganizationConformancePackStatuses,

    -- * Request Lenses
    describeOrganizationConformancePackStatuses_limit,
    describeOrganizationConformancePackStatuses_nextToken,
    describeOrganizationConformancePackStatuses_organizationConformancePackNames,

    -- * Destructuring the Response
    DescribeOrganizationConformancePackStatusesResponse (..),
    newDescribeOrganizationConformancePackStatusesResponse,

    -- * Response Lenses
    describeOrganizationConformancePackStatusesResponse_nextToken,
    describeOrganizationConformancePackStatusesResponse_organizationConformancePackStatuses,
    describeOrganizationConformancePackStatusesResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeOrganizationConformancePackStatuses' smart constructor.
data DescribeOrganizationConformancePackStatuses = DescribeOrganizationConformancePackStatuses'
  { -- | The maximum number of OrganizationConformancePackStatuses returned on
    -- each page. If you do no specify a number, Config uses the default. The
    -- default is 100.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The nextToken string returned on a previous page that you use to get the
    -- next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The names of organization conformance packs for which you want status
    -- details. If you do not specify any names, Config returns details for all
    -- your organization conformance packs.
    organizationConformancePackNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOrganizationConformancePackStatuses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'describeOrganizationConformancePackStatuses_limit' - The maximum number of OrganizationConformancePackStatuses returned on
-- each page. If you do no specify a number, Config uses the default. The
-- default is 100.
--
-- 'nextToken', 'describeOrganizationConformancePackStatuses_nextToken' - The nextToken string returned on a previous page that you use to get the
-- next page of results in a paginated response.
--
-- 'organizationConformancePackNames', 'describeOrganizationConformancePackStatuses_organizationConformancePackNames' - The names of organization conformance packs for which you want status
-- details. If you do not specify any names, Config returns details for all
-- your organization conformance packs.
newDescribeOrganizationConformancePackStatuses ::
  DescribeOrganizationConformancePackStatuses
newDescribeOrganizationConformancePackStatuses =
  DescribeOrganizationConformancePackStatuses'
    { limit =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      organizationConformancePackNames =
        Prelude.Nothing
    }

-- | The maximum number of OrganizationConformancePackStatuses returned on
-- each page. If you do no specify a number, Config uses the default. The
-- default is 100.
describeOrganizationConformancePackStatuses_limit :: Lens.Lens' DescribeOrganizationConformancePackStatuses (Prelude.Maybe Prelude.Natural)
describeOrganizationConformancePackStatuses_limit = Lens.lens (\DescribeOrganizationConformancePackStatuses' {limit} -> limit) (\s@DescribeOrganizationConformancePackStatuses' {} a -> s {limit = a} :: DescribeOrganizationConformancePackStatuses)

-- | The nextToken string returned on a previous page that you use to get the
-- next page of results in a paginated response.
describeOrganizationConformancePackStatuses_nextToken :: Lens.Lens' DescribeOrganizationConformancePackStatuses (Prelude.Maybe Prelude.Text)
describeOrganizationConformancePackStatuses_nextToken = Lens.lens (\DescribeOrganizationConformancePackStatuses' {nextToken} -> nextToken) (\s@DescribeOrganizationConformancePackStatuses' {} a -> s {nextToken = a} :: DescribeOrganizationConformancePackStatuses)

-- | The names of organization conformance packs for which you want status
-- details. If you do not specify any names, Config returns details for all
-- your organization conformance packs.
describeOrganizationConformancePackStatuses_organizationConformancePackNames :: Lens.Lens' DescribeOrganizationConformancePackStatuses (Prelude.Maybe [Prelude.Text])
describeOrganizationConformancePackStatuses_organizationConformancePackNames = Lens.lens (\DescribeOrganizationConformancePackStatuses' {organizationConformancePackNames} -> organizationConformancePackNames) (\s@DescribeOrganizationConformancePackStatuses' {} a -> s {organizationConformancePackNames = a} :: DescribeOrganizationConformancePackStatuses) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSPager
    DescribeOrganizationConformancePackStatuses
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeOrganizationConformancePackStatusesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeOrganizationConformancePackStatusesResponse_organizationConformancePackStatuses
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeOrganizationConformancePackStatuses_nextToken
              Lens..~ rs
              Lens.^? describeOrganizationConformancePackStatusesResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeOrganizationConformancePackStatuses
  where
  type
    AWSResponse
      DescribeOrganizationConformancePackStatuses =
      DescribeOrganizationConformancePackStatusesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOrganizationConformancePackStatusesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "OrganizationConformancePackStatuses"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeOrganizationConformancePackStatuses
  where
  hashWithSalt
    _salt
    DescribeOrganizationConformancePackStatuses' {..} =
      _salt
        `Prelude.hashWithSalt` limit
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` organizationConformancePackNames

instance
  Prelude.NFData
    DescribeOrganizationConformancePackStatuses
  where
  rnf DescribeOrganizationConformancePackStatuses' {..} =
    Prelude.rnf limit `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf organizationConformancePackNames

instance
  Data.ToHeaders
    DescribeOrganizationConformancePackStatuses
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.DescribeOrganizationConformancePackStatuses" ::
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
    DescribeOrganizationConformancePackStatuses
  where
  toJSON
    DescribeOrganizationConformancePackStatuses' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Limit" Data..=) Prelude.<$> limit,
              ("NextToken" Data..=) Prelude.<$> nextToken,
              ("OrganizationConformancePackNames" Data..=)
                Prelude.<$> organizationConformancePackNames
            ]
        )

instance
  Data.ToPath
    DescribeOrganizationConformancePackStatuses
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeOrganizationConformancePackStatuses
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeOrganizationConformancePackStatusesResponse' smart constructor.
data DescribeOrganizationConformancePackStatusesResponse = DescribeOrganizationConformancePackStatusesResponse'
  { -- | The nextToken string returned on a previous page that you use to get the
    -- next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of @OrganizationConformancePackStatus@ objects.
    organizationConformancePackStatuses :: Prelude.Maybe [OrganizationConformancePackStatus],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOrganizationConformancePackStatusesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeOrganizationConformancePackStatusesResponse_nextToken' - The nextToken string returned on a previous page that you use to get the
-- next page of results in a paginated response.
--
-- 'organizationConformancePackStatuses', 'describeOrganizationConformancePackStatusesResponse_organizationConformancePackStatuses' - A list of @OrganizationConformancePackStatus@ objects.
--
-- 'httpStatus', 'describeOrganizationConformancePackStatusesResponse_httpStatus' - The response's http status code.
newDescribeOrganizationConformancePackStatusesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeOrganizationConformancePackStatusesResponse
newDescribeOrganizationConformancePackStatusesResponse
  pHttpStatus_ =
    DescribeOrganizationConformancePackStatusesResponse'
      { nextToken =
          Prelude.Nothing,
        organizationConformancePackStatuses =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The nextToken string returned on a previous page that you use to get the
-- next page of results in a paginated response.
describeOrganizationConformancePackStatusesResponse_nextToken :: Lens.Lens' DescribeOrganizationConformancePackStatusesResponse (Prelude.Maybe Prelude.Text)
describeOrganizationConformancePackStatusesResponse_nextToken = Lens.lens (\DescribeOrganizationConformancePackStatusesResponse' {nextToken} -> nextToken) (\s@DescribeOrganizationConformancePackStatusesResponse' {} a -> s {nextToken = a} :: DescribeOrganizationConformancePackStatusesResponse)

-- | A list of @OrganizationConformancePackStatus@ objects.
describeOrganizationConformancePackStatusesResponse_organizationConformancePackStatuses :: Lens.Lens' DescribeOrganizationConformancePackStatusesResponse (Prelude.Maybe [OrganizationConformancePackStatus])
describeOrganizationConformancePackStatusesResponse_organizationConformancePackStatuses = Lens.lens (\DescribeOrganizationConformancePackStatusesResponse' {organizationConformancePackStatuses} -> organizationConformancePackStatuses) (\s@DescribeOrganizationConformancePackStatusesResponse' {} a -> s {organizationConformancePackStatuses = a} :: DescribeOrganizationConformancePackStatusesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeOrganizationConformancePackStatusesResponse_httpStatus :: Lens.Lens' DescribeOrganizationConformancePackStatusesResponse Prelude.Int
describeOrganizationConformancePackStatusesResponse_httpStatus = Lens.lens (\DescribeOrganizationConformancePackStatusesResponse' {httpStatus} -> httpStatus) (\s@DescribeOrganizationConformancePackStatusesResponse' {} a -> s {httpStatus = a} :: DescribeOrganizationConformancePackStatusesResponse)

instance
  Prelude.NFData
    DescribeOrganizationConformancePackStatusesResponse
  where
  rnf
    DescribeOrganizationConformancePackStatusesResponse' {..} =
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf organizationConformancePackStatuses `Prelude.seq`
          Prelude.rnf httpStatus
