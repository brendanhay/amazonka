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
-- Module      : Network.AWS.Config.GetOrganizationConfigRuleDetailedStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed status for each member account within an organization
-- for a given organization config rule.
module Network.AWS.Config.GetOrganizationConfigRuleDetailedStatus
  ( -- * Creating a Request
    GetOrganizationConfigRuleDetailedStatus (..),
    newGetOrganizationConfigRuleDetailedStatus,

    -- * Request Lenses
    getOrganizationConfigRuleDetailedStatus_nextToken,
    getOrganizationConfigRuleDetailedStatus_filters,
    getOrganizationConfigRuleDetailedStatus_limit,
    getOrganizationConfigRuleDetailedStatus_organizationConfigRuleName,

    -- * Destructuring the Response
    GetOrganizationConfigRuleDetailedStatusResponse (..),
    newGetOrganizationConfigRuleDetailedStatusResponse,

    -- * Response Lenses
    getOrganizationConfigRuleDetailedStatusResponse_nextToken,
    getOrganizationConfigRuleDetailedStatusResponse_organizationConfigRuleDetailedStatus,
    getOrganizationConfigRuleDetailedStatusResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetOrganizationConfigRuleDetailedStatus' smart constructor.
data GetOrganizationConfigRuleDetailedStatus = GetOrganizationConfigRuleDetailedStatus'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A @StatusDetailFilters@ object.
    filters :: Prelude.Maybe StatusDetailFilters,
    -- | The maximum number of @OrganizationConfigRuleDetailedStatus@ returned on
    -- each page. If you do not specify a number, AWS Config uses the default.
    -- The default is 100.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The name of organization config rule for which you want status details
    -- for member accounts.
    organizationConfigRuleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOrganizationConfigRuleDetailedStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getOrganizationConfigRuleDetailedStatus_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'filters', 'getOrganizationConfigRuleDetailedStatus_filters' - A @StatusDetailFilters@ object.
--
-- 'limit', 'getOrganizationConfigRuleDetailedStatus_limit' - The maximum number of @OrganizationConfigRuleDetailedStatus@ returned on
-- each page. If you do not specify a number, AWS Config uses the default.
-- The default is 100.
--
-- 'organizationConfigRuleName', 'getOrganizationConfigRuleDetailedStatus_organizationConfigRuleName' - The name of organization config rule for which you want status details
-- for member accounts.
newGetOrganizationConfigRuleDetailedStatus ::
  -- | 'organizationConfigRuleName'
  Prelude.Text ->
  GetOrganizationConfigRuleDetailedStatus
newGetOrganizationConfigRuleDetailedStatus
  pOrganizationConfigRuleName_ =
    GetOrganizationConfigRuleDetailedStatus'
      { nextToken =
          Prelude.Nothing,
        filters = Prelude.Nothing,
        limit = Prelude.Nothing,
        organizationConfigRuleName =
          pOrganizationConfigRuleName_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
getOrganizationConfigRuleDetailedStatus_nextToken :: Lens.Lens' GetOrganizationConfigRuleDetailedStatus (Prelude.Maybe Prelude.Text)
getOrganizationConfigRuleDetailedStatus_nextToken = Lens.lens (\GetOrganizationConfigRuleDetailedStatus' {nextToken} -> nextToken) (\s@GetOrganizationConfigRuleDetailedStatus' {} a -> s {nextToken = a} :: GetOrganizationConfigRuleDetailedStatus)

-- | A @StatusDetailFilters@ object.
getOrganizationConfigRuleDetailedStatus_filters :: Lens.Lens' GetOrganizationConfigRuleDetailedStatus (Prelude.Maybe StatusDetailFilters)
getOrganizationConfigRuleDetailedStatus_filters = Lens.lens (\GetOrganizationConfigRuleDetailedStatus' {filters} -> filters) (\s@GetOrganizationConfigRuleDetailedStatus' {} a -> s {filters = a} :: GetOrganizationConfigRuleDetailedStatus)

-- | The maximum number of @OrganizationConfigRuleDetailedStatus@ returned on
-- each page. If you do not specify a number, AWS Config uses the default.
-- The default is 100.
getOrganizationConfigRuleDetailedStatus_limit :: Lens.Lens' GetOrganizationConfigRuleDetailedStatus (Prelude.Maybe Prelude.Natural)
getOrganizationConfigRuleDetailedStatus_limit = Lens.lens (\GetOrganizationConfigRuleDetailedStatus' {limit} -> limit) (\s@GetOrganizationConfigRuleDetailedStatus' {} a -> s {limit = a} :: GetOrganizationConfigRuleDetailedStatus)

-- | The name of organization config rule for which you want status details
-- for member accounts.
getOrganizationConfigRuleDetailedStatus_organizationConfigRuleName :: Lens.Lens' GetOrganizationConfigRuleDetailedStatus Prelude.Text
getOrganizationConfigRuleDetailedStatus_organizationConfigRuleName = Lens.lens (\GetOrganizationConfigRuleDetailedStatus' {organizationConfigRuleName} -> organizationConfigRuleName) (\s@GetOrganizationConfigRuleDetailedStatus' {} a -> s {organizationConfigRuleName = a} :: GetOrganizationConfigRuleDetailedStatus)

instance
  Core.AWSRequest
    GetOrganizationConfigRuleDetailedStatus
  where
  type
    AWSResponse
      GetOrganizationConfigRuleDetailedStatus =
      GetOrganizationConfigRuleDetailedStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOrganizationConfigRuleDetailedStatusResponse'
            Prelude.<$> (x Core..?> "NextToken")
              Prelude.<*> ( x Core..?> "OrganizationConfigRuleDetailedStatus"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetOrganizationConfigRuleDetailedStatus

instance
  Prelude.NFData
    GetOrganizationConfigRuleDetailedStatus

instance
  Core.ToHeaders
    GetOrganizationConfigRuleDetailedStatus
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.GetOrganizationConfigRuleDetailedStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    GetOrganizationConfigRuleDetailedStatus
  where
  toJSON GetOrganizationConfigRuleDetailedStatus' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Filters" Core..=) Prelude.<$> filters,
            ("Limit" Core..=) Prelude.<$> limit,
            Prelude.Just
              ( "OrganizationConfigRuleName"
                  Core..= organizationConfigRuleName
              )
          ]
      )

instance
  Core.ToPath
    GetOrganizationConfigRuleDetailedStatus
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    GetOrganizationConfigRuleDetailedStatus
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetOrganizationConfigRuleDetailedStatusResponse' smart constructor.
data GetOrganizationConfigRuleDetailedStatusResponse = GetOrganizationConfigRuleDetailedStatusResponse'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of @MemberAccountStatus@ objects.
    organizationConfigRuleDetailedStatus :: Prelude.Maybe [MemberAccountStatus],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOrganizationConfigRuleDetailedStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getOrganizationConfigRuleDetailedStatusResponse_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'organizationConfigRuleDetailedStatus', 'getOrganizationConfigRuleDetailedStatusResponse_organizationConfigRuleDetailedStatus' - A list of @MemberAccountStatus@ objects.
--
-- 'httpStatus', 'getOrganizationConfigRuleDetailedStatusResponse_httpStatus' - The response's http status code.
newGetOrganizationConfigRuleDetailedStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetOrganizationConfigRuleDetailedStatusResponse
newGetOrganizationConfigRuleDetailedStatusResponse
  pHttpStatus_ =
    GetOrganizationConfigRuleDetailedStatusResponse'
      { nextToken =
          Prelude.Nothing,
        organizationConfigRuleDetailedStatus =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
getOrganizationConfigRuleDetailedStatusResponse_nextToken :: Lens.Lens' GetOrganizationConfigRuleDetailedStatusResponse (Prelude.Maybe Prelude.Text)
getOrganizationConfigRuleDetailedStatusResponse_nextToken = Lens.lens (\GetOrganizationConfigRuleDetailedStatusResponse' {nextToken} -> nextToken) (\s@GetOrganizationConfigRuleDetailedStatusResponse' {} a -> s {nextToken = a} :: GetOrganizationConfigRuleDetailedStatusResponse)

-- | A list of @MemberAccountStatus@ objects.
getOrganizationConfigRuleDetailedStatusResponse_organizationConfigRuleDetailedStatus :: Lens.Lens' GetOrganizationConfigRuleDetailedStatusResponse (Prelude.Maybe [MemberAccountStatus])
getOrganizationConfigRuleDetailedStatusResponse_organizationConfigRuleDetailedStatus = Lens.lens (\GetOrganizationConfigRuleDetailedStatusResponse' {organizationConfigRuleDetailedStatus} -> organizationConfigRuleDetailedStatus) (\s@GetOrganizationConfigRuleDetailedStatusResponse' {} a -> s {organizationConfigRuleDetailedStatus = a} :: GetOrganizationConfigRuleDetailedStatusResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getOrganizationConfigRuleDetailedStatusResponse_httpStatus :: Lens.Lens' GetOrganizationConfigRuleDetailedStatusResponse Prelude.Int
getOrganizationConfigRuleDetailedStatusResponse_httpStatus = Lens.lens (\GetOrganizationConfigRuleDetailedStatusResponse' {httpStatus} -> httpStatus) (\s@GetOrganizationConfigRuleDetailedStatusResponse' {} a -> s {httpStatus = a} :: GetOrganizationConfigRuleDetailedStatusResponse)

instance
  Prelude.NFData
    GetOrganizationConfigRuleDetailedStatusResponse
