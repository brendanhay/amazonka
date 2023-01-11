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
-- Module      : Amazonka.Config.DescribeOrganizationConfigRules
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of organization Config rules.
--
-- When you specify the limit and the next token, you receive a paginated
-- response.
--
-- Limit and next token are not applicable if you specify organization
-- Config rule names. It is only applicable, when you request all the
-- organization Config rules.
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
module Amazonka.Config.DescribeOrganizationConfigRules
  ( -- * Creating a Request
    DescribeOrganizationConfigRules (..),
    newDescribeOrganizationConfigRules,

    -- * Request Lenses
    describeOrganizationConfigRules_limit,
    describeOrganizationConfigRules_nextToken,
    describeOrganizationConfigRules_organizationConfigRuleNames,

    -- * Destructuring the Response
    DescribeOrganizationConfigRulesResponse (..),
    newDescribeOrganizationConfigRulesResponse,

    -- * Response Lenses
    describeOrganizationConfigRulesResponse_nextToken,
    describeOrganizationConfigRulesResponse_organizationConfigRules,
    describeOrganizationConfigRulesResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeOrganizationConfigRules' smart constructor.
data DescribeOrganizationConfigRules = DescribeOrganizationConfigRules'
  { -- | The maximum number of organization Config rules returned on each page.
    -- If you do no specify a number, Config uses the default. The default is
    -- 100.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The names of organization Config rules for which you want details. If
    -- you do not specify any names, Config returns details for all your
    -- organization Config rules.
    organizationConfigRuleNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOrganizationConfigRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'describeOrganizationConfigRules_limit' - The maximum number of organization Config rules returned on each page.
-- If you do no specify a number, Config uses the default. The default is
-- 100.
--
-- 'nextToken', 'describeOrganizationConfigRules_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'organizationConfigRuleNames', 'describeOrganizationConfigRules_organizationConfigRuleNames' - The names of organization Config rules for which you want details. If
-- you do not specify any names, Config returns details for all your
-- organization Config rules.
newDescribeOrganizationConfigRules ::
  DescribeOrganizationConfigRules
newDescribeOrganizationConfigRules =
  DescribeOrganizationConfigRules'
    { limit =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      organizationConfigRuleNames =
        Prelude.Nothing
    }

-- | The maximum number of organization Config rules returned on each page.
-- If you do no specify a number, Config uses the default. The default is
-- 100.
describeOrganizationConfigRules_limit :: Lens.Lens' DescribeOrganizationConfigRules (Prelude.Maybe Prelude.Natural)
describeOrganizationConfigRules_limit = Lens.lens (\DescribeOrganizationConfigRules' {limit} -> limit) (\s@DescribeOrganizationConfigRules' {} a -> s {limit = a} :: DescribeOrganizationConfigRules)

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeOrganizationConfigRules_nextToken :: Lens.Lens' DescribeOrganizationConfigRules (Prelude.Maybe Prelude.Text)
describeOrganizationConfigRules_nextToken = Lens.lens (\DescribeOrganizationConfigRules' {nextToken} -> nextToken) (\s@DescribeOrganizationConfigRules' {} a -> s {nextToken = a} :: DescribeOrganizationConfigRules)

-- | The names of organization Config rules for which you want details. If
-- you do not specify any names, Config returns details for all your
-- organization Config rules.
describeOrganizationConfigRules_organizationConfigRuleNames :: Lens.Lens' DescribeOrganizationConfigRules (Prelude.Maybe [Prelude.Text])
describeOrganizationConfigRules_organizationConfigRuleNames = Lens.lens (\DescribeOrganizationConfigRules' {organizationConfigRuleNames} -> organizationConfigRuleNames) (\s@DescribeOrganizationConfigRules' {} a -> s {organizationConfigRuleNames = a} :: DescribeOrganizationConfigRules) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSPager
    DescribeOrganizationConfigRules
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeOrganizationConfigRulesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeOrganizationConfigRulesResponse_organizationConfigRules
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeOrganizationConfigRules_nextToken
          Lens..~ rs
          Lens.^? describeOrganizationConfigRulesResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeOrganizationConfigRules
  where
  type
    AWSResponse DescribeOrganizationConfigRules =
      DescribeOrganizationConfigRulesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOrganizationConfigRulesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "OrganizationConfigRules"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeOrganizationConfigRules
  where
  hashWithSalt
    _salt
    DescribeOrganizationConfigRules' {..} =
      _salt `Prelude.hashWithSalt` limit
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` organizationConfigRuleNames

instance
  Prelude.NFData
    DescribeOrganizationConfigRules
  where
  rnf DescribeOrganizationConfigRules' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf organizationConfigRuleNames

instance
  Data.ToHeaders
    DescribeOrganizationConfigRules
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.DescribeOrganizationConfigRules" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeOrganizationConfigRules where
  toJSON DescribeOrganizationConfigRules' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("OrganizationConfigRuleNames" Data..=)
              Prelude.<$> organizationConfigRuleNames
          ]
      )

instance Data.ToPath DescribeOrganizationConfigRules where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeOrganizationConfigRules where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeOrganizationConfigRulesResponse' smart constructor.
data DescribeOrganizationConfigRulesResponse = DescribeOrganizationConfigRulesResponse'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns a list of @OrganizationConfigRule@ objects.
    organizationConfigRules :: Prelude.Maybe [OrganizationConfigRule],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOrganizationConfigRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeOrganizationConfigRulesResponse_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'organizationConfigRules', 'describeOrganizationConfigRulesResponse_organizationConfigRules' - Returns a list of @OrganizationConfigRule@ objects.
--
-- 'httpStatus', 'describeOrganizationConfigRulesResponse_httpStatus' - The response's http status code.
newDescribeOrganizationConfigRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeOrganizationConfigRulesResponse
newDescribeOrganizationConfigRulesResponse
  pHttpStatus_ =
    DescribeOrganizationConfigRulesResponse'
      { nextToken =
          Prelude.Nothing,
        organizationConfigRules =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeOrganizationConfigRulesResponse_nextToken :: Lens.Lens' DescribeOrganizationConfigRulesResponse (Prelude.Maybe Prelude.Text)
describeOrganizationConfigRulesResponse_nextToken = Lens.lens (\DescribeOrganizationConfigRulesResponse' {nextToken} -> nextToken) (\s@DescribeOrganizationConfigRulesResponse' {} a -> s {nextToken = a} :: DescribeOrganizationConfigRulesResponse)

-- | Returns a list of @OrganizationConfigRule@ objects.
describeOrganizationConfigRulesResponse_organizationConfigRules :: Lens.Lens' DescribeOrganizationConfigRulesResponse (Prelude.Maybe [OrganizationConfigRule])
describeOrganizationConfigRulesResponse_organizationConfigRules = Lens.lens (\DescribeOrganizationConfigRulesResponse' {organizationConfigRules} -> organizationConfigRules) (\s@DescribeOrganizationConfigRulesResponse' {} a -> s {organizationConfigRules = a} :: DescribeOrganizationConfigRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeOrganizationConfigRulesResponse_httpStatus :: Lens.Lens' DescribeOrganizationConfigRulesResponse Prelude.Int
describeOrganizationConfigRulesResponse_httpStatus = Lens.lens (\DescribeOrganizationConfigRulesResponse' {httpStatus} -> httpStatus) (\s@DescribeOrganizationConfigRulesResponse' {} a -> s {httpStatus = a} :: DescribeOrganizationConfigRulesResponse)

instance
  Prelude.NFData
    DescribeOrganizationConfigRulesResponse
  where
  rnf DescribeOrganizationConfigRulesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf organizationConfigRules
      `Prelude.seq` Prelude.rnf httpStatus
