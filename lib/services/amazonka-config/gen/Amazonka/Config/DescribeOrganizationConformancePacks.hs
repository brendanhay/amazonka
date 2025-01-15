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
-- Module      : Amazonka.Config.DescribeOrganizationConformancePacks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of organization conformance packs.
--
-- When you specify the limit and the next token, you receive a paginated
-- response.
--
-- Limit and next token are not applicable if you specify organization
-- conformance packs names. They are only applicable, when you request all
-- the organization conformance packs.
--
-- /For accounts within an organzation/
--
-- If you deploy an organizational rule or conformance pack in an
-- organization administrator account, and then establish a delegated
-- administrator and deploy an organizational rule or conformance pack in
-- the delegated administrator account, you won\'t be able to see the
-- organizational rule or conformance pack in the organization
-- administrator account from the delegated administrator account or see
-- the organizational rule or conformance pack in the delegated
-- administrator account from organization administrator account. The
-- @DescribeOrganizationConfigRules@ and
-- @DescribeOrganizationConformancePacks@ APIs can only see and interact
-- with the organization-related resource that were deployed from within
-- the account calling those APIs.
--
-- This operation returns paginated results.
module Amazonka.Config.DescribeOrganizationConformancePacks
  ( -- * Creating a Request
    DescribeOrganizationConformancePacks (..),
    newDescribeOrganizationConformancePacks,

    -- * Request Lenses
    describeOrganizationConformancePacks_limit,
    describeOrganizationConformancePacks_nextToken,
    describeOrganizationConformancePacks_organizationConformancePackNames,

    -- * Destructuring the Response
    DescribeOrganizationConformancePacksResponse (..),
    newDescribeOrganizationConformancePacksResponse,

    -- * Response Lenses
    describeOrganizationConformancePacksResponse_nextToken,
    describeOrganizationConformancePacksResponse_organizationConformancePacks,
    describeOrganizationConformancePacksResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeOrganizationConformancePacks' smart constructor.
data DescribeOrganizationConformancePacks = DescribeOrganizationConformancePacks'
  { -- | The maximum number of organization config packs returned on each page.
    -- If you do no specify a number, Config uses the default. The default is
    -- 100.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The nextToken string returned on a previous page that you use to get the
    -- next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name that you assign to an organization conformance pack.
    organizationConformancePackNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOrganizationConformancePacks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'describeOrganizationConformancePacks_limit' - The maximum number of organization config packs returned on each page.
-- If you do no specify a number, Config uses the default. The default is
-- 100.
--
-- 'nextToken', 'describeOrganizationConformancePacks_nextToken' - The nextToken string returned on a previous page that you use to get the
-- next page of results in a paginated response.
--
-- 'organizationConformancePackNames', 'describeOrganizationConformancePacks_organizationConformancePackNames' - The name that you assign to an organization conformance pack.
newDescribeOrganizationConformancePacks ::
  DescribeOrganizationConformancePacks
newDescribeOrganizationConformancePacks =
  DescribeOrganizationConformancePacks'
    { limit =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      organizationConformancePackNames =
        Prelude.Nothing
    }

-- | The maximum number of organization config packs returned on each page.
-- If you do no specify a number, Config uses the default. The default is
-- 100.
describeOrganizationConformancePacks_limit :: Lens.Lens' DescribeOrganizationConformancePacks (Prelude.Maybe Prelude.Natural)
describeOrganizationConformancePacks_limit = Lens.lens (\DescribeOrganizationConformancePacks' {limit} -> limit) (\s@DescribeOrganizationConformancePacks' {} a -> s {limit = a} :: DescribeOrganizationConformancePacks)

-- | The nextToken string returned on a previous page that you use to get the
-- next page of results in a paginated response.
describeOrganizationConformancePacks_nextToken :: Lens.Lens' DescribeOrganizationConformancePacks (Prelude.Maybe Prelude.Text)
describeOrganizationConformancePacks_nextToken = Lens.lens (\DescribeOrganizationConformancePacks' {nextToken} -> nextToken) (\s@DescribeOrganizationConformancePacks' {} a -> s {nextToken = a} :: DescribeOrganizationConformancePacks)

-- | The name that you assign to an organization conformance pack.
describeOrganizationConformancePacks_organizationConformancePackNames :: Lens.Lens' DescribeOrganizationConformancePacks (Prelude.Maybe [Prelude.Text])
describeOrganizationConformancePacks_organizationConformancePackNames = Lens.lens (\DescribeOrganizationConformancePacks' {organizationConformancePackNames} -> organizationConformancePackNames) (\s@DescribeOrganizationConformancePacks' {} a -> s {organizationConformancePackNames = a} :: DescribeOrganizationConformancePacks) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSPager
    DescribeOrganizationConformancePacks
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeOrganizationConformancePacksResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeOrganizationConformancePacksResponse_organizationConformancePacks
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeOrganizationConformancePacks_nextToken
              Lens..~ rs
              Lens.^? describeOrganizationConformancePacksResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeOrganizationConformancePacks
  where
  type
    AWSResponse DescribeOrganizationConformancePacks =
      DescribeOrganizationConformancePacksResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOrganizationConformancePacksResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "OrganizationConformancePacks"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeOrganizationConformancePacks
  where
  hashWithSalt
    _salt
    DescribeOrganizationConformancePacks' {..} =
      _salt
        `Prelude.hashWithSalt` limit
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` organizationConformancePackNames

instance
  Prelude.NFData
    DescribeOrganizationConformancePacks
  where
  rnf DescribeOrganizationConformancePacks' {..} =
    Prelude.rnf limit `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf organizationConformancePackNames

instance
  Data.ToHeaders
    DescribeOrganizationConformancePacks
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.DescribeOrganizationConformancePacks" ::
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
    DescribeOrganizationConformancePacks
  where
  toJSON DescribeOrganizationConformancePacks' {..} =
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
    DescribeOrganizationConformancePacks
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeOrganizationConformancePacks
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeOrganizationConformancePacksResponse' smart constructor.
data DescribeOrganizationConformancePacksResponse = DescribeOrganizationConformancePacksResponse'
  { -- | The nextToken string returned on a previous page that you use to get the
    -- next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns a list of OrganizationConformancePacks objects.
    organizationConformancePacks :: Prelude.Maybe [OrganizationConformancePack],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOrganizationConformancePacksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeOrganizationConformancePacksResponse_nextToken' - The nextToken string returned on a previous page that you use to get the
-- next page of results in a paginated response.
--
-- 'organizationConformancePacks', 'describeOrganizationConformancePacksResponse_organizationConformancePacks' - Returns a list of OrganizationConformancePacks objects.
--
-- 'httpStatus', 'describeOrganizationConformancePacksResponse_httpStatus' - The response's http status code.
newDescribeOrganizationConformancePacksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeOrganizationConformancePacksResponse
newDescribeOrganizationConformancePacksResponse
  pHttpStatus_ =
    DescribeOrganizationConformancePacksResponse'
      { nextToken =
          Prelude.Nothing,
        organizationConformancePacks =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The nextToken string returned on a previous page that you use to get the
-- next page of results in a paginated response.
describeOrganizationConformancePacksResponse_nextToken :: Lens.Lens' DescribeOrganizationConformancePacksResponse (Prelude.Maybe Prelude.Text)
describeOrganizationConformancePacksResponse_nextToken = Lens.lens (\DescribeOrganizationConformancePacksResponse' {nextToken} -> nextToken) (\s@DescribeOrganizationConformancePacksResponse' {} a -> s {nextToken = a} :: DescribeOrganizationConformancePacksResponse)

-- | Returns a list of OrganizationConformancePacks objects.
describeOrganizationConformancePacksResponse_organizationConformancePacks :: Lens.Lens' DescribeOrganizationConformancePacksResponse (Prelude.Maybe [OrganizationConformancePack])
describeOrganizationConformancePacksResponse_organizationConformancePacks = Lens.lens (\DescribeOrganizationConformancePacksResponse' {organizationConformancePacks} -> organizationConformancePacks) (\s@DescribeOrganizationConformancePacksResponse' {} a -> s {organizationConformancePacks = a} :: DescribeOrganizationConformancePacksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeOrganizationConformancePacksResponse_httpStatus :: Lens.Lens' DescribeOrganizationConformancePacksResponse Prelude.Int
describeOrganizationConformancePacksResponse_httpStatus = Lens.lens (\DescribeOrganizationConformancePacksResponse' {httpStatus} -> httpStatus) (\s@DescribeOrganizationConformancePacksResponse' {} a -> s {httpStatus = a} :: DescribeOrganizationConformancePacksResponse)

instance
  Prelude.NFData
    DescribeOrganizationConformancePacksResponse
  where
  rnf DescribeOrganizationConformancePacksResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf organizationConformancePacks `Prelude.seq`
        Prelude.rnf httpStatus
