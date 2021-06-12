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
-- Module      : Network.AWS.Config.GetOrganizationConformancePackDetailedStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed status for each member account within an organization
-- for a given organization conformance pack.
module Network.AWS.Config.GetOrganizationConformancePackDetailedStatus
  ( -- * Creating a Request
    GetOrganizationConformancePackDetailedStatus (..),
    newGetOrganizationConformancePackDetailedStatus,

    -- * Request Lenses
    getOrganizationConformancePackDetailedStatus_nextToken,
    getOrganizationConformancePackDetailedStatus_filters,
    getOrganizationConformancePackDetailedStatus_limit,
    getOrganizationConformancePackDetailedStatus_organizationConformancePackName,

    -- * Destructuring the Response
    GetOrganizationConformancePackDetailedStatusResponse (..),
    newGetOrganizationConformancePackDetailedStatusResponse,

    -- * Response Lenses
    getOrganizationConformancePackDetailedStatusResponse_nextToken,
    getOrganizationConformancePackDetailedStatusResponse_organizationConformancePackDetailedStatuses,
    getOrganizationConformancePackDetailedStatusResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetOrganizationConformancePackDetailedStatus' smart constructor.
data GetOrganizationConformancePackDetailedStatus = GetOrganizationConformancePackDetailedStatus'
  { -- | The nextToken string returned on a previous page that you use to get the
    -- next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | An @OrganizationResourceDetailedStatusFilters@ object.
    filters :: Core.Maybe OrganizationResourceDetailedStatusFilters,
    -- | The maximum number of @OrganizationConformancePackDetailedStatuses@
    -- returned on each page. If you do not specify a number, AWS Config uses
    -- the default. The default is 100.
    limit :: Core.Maybe Core.Natural,
    -- | The name of organization conformance pack for which you want status
    -- details for member accounts.
    organizationConformancePackName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetOrganizationConformancePackDetailedStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getOrganizationConformancePackDetailedStatus_nextToken' - The nextToken string returned on a previous page that you use to get the
-- next page of results in a paginated response.
--
-- 'filters', 'getOrganizationConformancePackDetailedStatus_filters' - An @OrganizationResourceDetailedStatusFilters@ object.
--
-- 'limit', 'getOrganizationConformancePackDetailedStatus_limit' - The maximum number of @OrganizationConformancePackDetailedStatuses@
-- returned on each page. If you do not specify a number, AWS Config uses
-- the default. The default is 100.
--
-- 'organizationConformancePackName', 'getOrganizationConformancePackDetailedStatus_organizationConformancePackName' - The name of organization conformance pack for which you want status
-- details for member accounts.
newGetOrganizationConformancePackDetailedStatus ::
  -- | 'organizationConformancePackName'
  Core.Text ->
  GetOrganizationConformancePackDetailedStatus
newGetOrganizationConformancePackDetailedStatus
  pOrganizationConformancePackName_ =
    GetOrganizationConformancePackDetailedStatus'
      { nextToken =
          Core.Nothing,
        filters = Core.Nothing,
        limit = Core.Nothing,
        organizationConformancePackName =
          pOrganizationConformancePackName_
      }

-- | The nextToken string returned on a previous page that you use to get the
-- next page of results in a paginated response.
getOrganizationConformancePackDetailedStatus_nextToken :: Lens.Lens' GetOrganizationConformancePackDetailedStatus (Core.Maybe Core.Text)
getOrganizationConformancePackDetailedStatus_nextToken = Lens.lens (\GetOrganizationConformancePackDetailedStatus' {nextToken} -> nextToken) (\s@GetOrganizationConformancePackDetailedStatus' {} a -> s {nextToken = a} :: GetOrganizationConformancePackDetailedStatus)

-- | An @OrganizationResourceDetailedStatusFilters@ object.
getOrganizationConformancePackDetailedStatus_filters :: Lens.Lens' GetOrganizationConformancePackDetailedStatus (Core.Maybe OrganizationResourceDetailedStatusFilters)
getOrganizationConformancePackDetailedStatus_filters = Lens.lens (\GetOrganizationConformancePackDetailedStatus' {filters} -> filters) (\s@GetOrganizationConformancePackDetailedStatus' {} a -> s {filters = a} :: GetOrganizationConformancePackDetailedStatus)

-- | The maximum number of @OrganizationConformancePackDetailedStatuses@
-- returned on each page. If you do not specify a number, AWS Config uses
-- the default. The default is 100.
getOrganizationConformancePackDetailedStatus_limit :: Lens.Lens' GetOrganizationConformancePackDetailedStatus (Core.Maybe Core.Natural)
getOrganizationConformancePackDetailedStatus_limit = Lens.lens (\GetOrganizationConformancePackDetailedStatus' {limit} -> limit) (\s@GetOrganizationConformancePackDetailedStatus' {} a -> s {limit = a} :: GetOrganizationConformancePackDetailedStatus)

-- | The name of organization conformance pack for which you want status
-- details for member accounts.
getOrganizationConformancePackDetailedStatus_organizationConformancePackName :: Lens.Lens' GetOrganizationConformancePackDetailedStatus Core.Text
getOrganizationConformancePackDetailedStatus_organizationConformancePackName = Lens.lens (\GetOrganizationConformancePackDetailedStatus' {organizationConformancePackName} -> organizationConformancePackName) (\s@GetOrganizationConformancePackDetailedStatus' {} a -> s {organizationConformancePackName = a} :: GetOrganizationConformancePackDetailedStatus)

instance
  Core.AWSRequest
    GetOrganizationConformancePackDetailedStatus
  where
  type
    AWSResponse
      GetOrganizationConformancePackDetailedStatus =
      GetOrganizationConformancePackDetailedStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOrganizationConformancePackDetailedStatusResponse'
            Core.<$> (x Core..?> "NextToken")
              Core.<*> ( x
                           Core..?> "OrganizationConformancePackDetailedStatuses"
                           Core..!@ Core.mempty
                       )
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    GetOrganizationConformancePackDetailedStatus

instance
  Core.NFData
    GetOrganizationConformancePackDetailedStatus

instance
  Core.ToHeaders
    GetOrganizationConformancePackDetailedStatus
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.GetOrganizationConformancePackDetailedStatus" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    GetOrganizationConformancePackDetailedStatus
  where
  toJSON
    GetOrganizationConformancePackDetailedStatus' {..} =
      Core.object
        ( Core.catMaybes
            [ ("NextToken" Core..=) Core.<$> nextToken,
              ("Filters" Core..=) Core.<$> filters,
              ("Limit" Core..=) Core.<$> limit,
              Core.Just
                ( "OrganizationConformancePackName"
                    Core..= organizationConformancePackName
                )
            ]
        )

instance
  Core.ToPath
    GetOrganizationConformancePackDetailedStatus
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    GetOrganizationConformancePackDetailedStatus
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetOrganizationConformancePackDetailedStatusResponse' smart constructor.
data GetOrganizationConformancePackDetailedStatusResponse = GetOrganizationConformancePackDetailedStatusResponse'
  { -- | The nextToken string returned on a previous page that you use to get the
    -- next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of @OrganizationConformancePackDetailedStatus@ objects.
    organizationConformancePackDetailedStatuses :: Core.Maybe [OrganizationConformancePackDetailedStatus],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetOrganizationConformancePackDetailedStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getOrganizationConformancePackDetailedStatusResponse_nextToken' - The nextToken string returned on a previous page that you use to get the
-- next page of results in a paginated response.
--
-- 'organizationConformancePackDetailedStatuses', 'getOrganizationConformancePackDetailedStatusResponse_organizationConformancePackDetailedStatuses' - A list of @OrganizationConformancePackDetailedStatus@ objects.
--
-- 'httpStatus', 'getOrganizationConformancePackDetailedStatusResponse_httpStatus' - The response's http status code.
newGetOrganizationConformancePackDetailedStatusResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetOrganizationConformancePackDetailedStatusResponse
newGetOrganizationConformancePackDetailedStatusResponse
  pHttpStatus_ =
    GetOrganizationConformancePackDetailedStatusResponse'
      { nextToken =
          Core.Nothing,
        organizationConformancePackDetailedStatuses =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The nextToken string returned on a previous page that you use to get the
-- next page of results in a paginated response.
getOrganizationConformancePackDetailedStatusResponse_nextToken :: Lens.Lens' GetOrganizationConformancePackDetailedStatusResponse (Core.Maybe Core.Text)
getOrganizationConformancePackDetailedStatusResponse_nextToken = Lens.lens (\GetOrganizationConformancePackDetailedStatusResponse' {nextToken} -> nextToken) (\s@GetOrganizationConformancePackDetailedStatusResponse' {} a -> s {nextToken = a} :: GetOrganizationConformancePackDetailedStatusResponse)

-- | A list of @OrganizationConformancePackDetailedStatus@ objects.
getOrganizationConformancePackDetailedStatusResponse_organizationConformancePackDetailedStatuses :: Lens.Lens' GetOrganizationConformancePackDetailedStatusResponse (Core.Maybe [OrganizationConformancePackDetailedStatus])
getOrganizationConformancePackDetailedStatusResponse_organizationConformancePackDetailedStatuses = Lens.lens (\GetOrganizationConformancePackDetailedStatusResponse' {organizationConformancePackDetailedStatuses} -> organizationConformancePackDetailedStatuses) (\s@GetOrganizationConformancePackDetailedStatusResponse' {} a -> s {organizationConformancePackDetailedStatuses = a} :: GetOrganizationConformancePackDetailedStatusResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getOrganizationConformancePackDetailedStatusResponse_httpStatus :: Lens.Lens' GetOrganizationConformancePackDetailedStatusResponse Core.Int
getOrganizationConformancePackDetailedStatusResponse_httpStatus = Lens.lens (\GetOrganizationConformancePackDetailedStatusResponse' {httpStatus} -> httpStatus) (\s@GetOrganizationConformancePackDetailedStatusResponse' {} a -> s {httpStatus = a} :: GetOrganizationConformancePackDetailedStatusResponse)

instance
  Core.NFData
    GetOrganizationConformancePackDetailedStatusResponse
