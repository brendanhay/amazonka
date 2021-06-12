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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetOrganizationConfigRuleDetailedStatus' smart constructor.
data GetOrganizationConfigRuleDetailedStatus = GetOrganizationConfigRuleDetailedStatus'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | A @StatusDetailFilters@ object.
    filters :: Core.Maybe StatusDetailFilters,
    -- | The maximum number of @OrganizationConfigRuleDetailedStatus@ returned on
    -- each page. If you do not specify a number, AWS Config uses the default.
    -- The default is 100.
    limit :: Core.Maybe Core.Natural,
    -- | The name of organization config rule for which you want status details
    -- for member accounts.
    organizationConfigRuleName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetOrganizationConfigRuleDetailedStatus
newGetOrganizationConfigRuleDetailedStatus
  pOrganizationConfigRuleName_ =
    GetOrganizationConfigRuleDetailedStatus'
      { nextToken =
          Core.Nothing,
        filters = Core.Nothing,
        limit = Core.Nothing,
        organizationConfigRuleName =
          pOrganizationConfigRuleName_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
getOrganizationConfigRuleDetailedStatus_nextToken :: Lens.Lens' GetOrganizationConfigRuleDetailedStatus (Core.Maybe Core.Text)
getOrganizationConfigRuleDetailedStatus_nextToken = Lens.lens (\GetOrganizationConfigRuleDetailedStatus' {nextToken} -> nextToken) (\s@GetOrganizationConfigRuleDetailedStatus' {} a -> s {nextToken = a} :: GetOrganizationConfigRuleDetailedStatus)

-- | A @StatusDetailFilters@ object.
getOrganizationConfigRuleDetailedStatus_filters :: Lens.Lens' GetOrganizationConfigRuleDetailedStatus (Core.Maybe StatusDetailFilters)
getOrganizationConfigRuleDetailedStatus_filters = Lens.lens (\GetOrganizationConfigRuleDetailedStatus' {filters} -> filters) (\s@GetOrganizationConfigRuleDetailedStatus' {} a -> s {filters = a} :: GetOrganizationConfigRuleDetailedStatus)

-- | The maximum number of @OrganizationConfigRuleDetailedStatus@ returned on
-- each page. If you do not specify a number, AWS Config uses the default.
-- The default is 100.
getOrganizationConfigRuleDetailedStatus_limit :: Lens.Lens' GetOrganizationConfigRuleDetailedStatus (Core.Maybe Core.Natural)
getOrganizationConfigRuleDetailedStatus_limit = Lens.lens (\GetOrganizationConfigRuleDetailedStatus' {limit} -> limit) (\s@GetOrganizationConfigRuleDetailedStatus' {} a -> s {limit = a} :: GetOrganizationConfigRuleDetailedStatus)

-- | The name of organization config rule for which you want status details
-- for member accounts.
getOrganizationConfigRuleDetailedStatus_organizationConfigRuleName :: Lens.Lens' GetOrganizationConfigRuleDetailedStatus Core.Text
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
            Core.<$> (x Core..?> "NextToken")
              Core.<*> ( x Core..?> "OrganizationConfigRuleDetailedStatus"
                           Core..!@ Core.mempty
                       )
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    GetOrganizationConfigRuleDetailedStatus

instance
  Core.NFData
    GetOrganizationConfigRuleDetailedStatus

instance
  Core.ToHeaders
    GetOrganizationConfigRuleDetailedStatus
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.GetOrganizationConfigRuleDetailedStatus" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    GetOrganizationConfigRuleDetailedStatus
  where
  toJSON GetOrganizationConfigRuleDetailedStatus' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("Filters" Core..=) Core.<$> filters,
            ("Limit" Core..=) Core.<$> limit,
            Core.Just
              ( "OrganizationConfigRuleName"
                  Core..= organizationConfigRuleName
              )
          ]
      )

instance
  Core.ToPath
    GetOrganizationConfigRuleDetailedStatus
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    GetOrganizationConfigRuleDetailedStatus
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetOrganizationConfigRuleDetailedStatusResponse' smart constructor.
data GetOrganizationConfigRuleDetailedStatusResponse = GetOrganizationConfigRuleDetailedStatusResponse'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of @MemberAccountStatus@ objects.
    organizationConfigRuleDetailedStatus :: Core.Maybe [MemberAccountStatus],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetOrganizationConfigRuleDetailedStatusResponse
newGetOrganizationConfigRuleDetailedStatusResponse
  pHttpStatus_ =
    GetOrganizationConfigRuleDetailedStatusResponse'
      { nextToken =
          Core.Nothing,
        organizationConfigRuleDetailedStatus =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
getOrganizationConfigRuleDetailedStatusResponse_nextToken :: Lens.Lens' GetOrganizationConfigRuleDetailedStatusResponse (Core.Maybe Core.Text)
getOrganizationConfigRuleDetailedStatusResponse_nextToken = Lens.lens (\GetOrganizationConfigRuleDetailedStatusResponse' {nextToken} -> nextToken) (\s@GetOrganizationConfigRuleDetailedStatusResponse' {} a -> s {nextToken = a} :: GetOrganizationConfigRuleDetailedStatusResponse)

-- | A list of @MemberAccountStatus@ objects.
getOrganizationConfigRuleDetailedStatusResponse_organizationConfigRuleDetailedStatus :: Lens.Lens' GetOrganizationConfigRuleDetailedStatusResponse (Core.Maybe [MemberAccountStatus])
getOrganizationConfigRuleDetailedStatusResponse_organizationConfigRuleDetailedStatus = Lens.lens (\GetOrganizationConfigRuleDetailedStatusResponse' {organizationConfigRuleDetailedStatus} -> organizationConfigRuleDetailedStatus) (\s@GetOrganizationConfigRuleDetailedStatusResponse' {} a -> s {organizationConfigRuleDetailedStatus = a} :: GetOrganizationConfigRuleDetailedStatusResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getOrganizationConfigRuleDetailedStatusResponse_httpStatus :: Lens.Lens' GetOrganizationConfigRuleDetailedStatusResponse Core.Int
getOrganizationConfigRuleDetailedStatusResponse_httpStatus = Lens.lens (\GetOrganizationConfigRuleDetailedStatusResponse' {httpStatus} -> httpStatus) (\s@GetOrganizationConfigRuleDetailedStatusResponse' {} a -> s {httpStatus = a} :: GetOrganizationConfigRuleDetailedStatusResponse)

instance
  Core.NFData
    GetOrganizationConfigRuleDetailedStatusResponse
