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
-- Module      : Amazonka.Config.GetOrganizationConformancePackDetailedStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed status for each member account within an organization
-- for a given organization conformance pack.
--
-- This operation returns paginated results.
module Amazonka.Config.GetOrganizationConformancePackDetailedStatus
  ( -- * Creating a Request
    GetOrganizationConformancePackDetailedStatus (..),
    newGetOrganizationConformancePackDetailedStatus,

    -- * Request Lenses
    getOrganizationConformancePackDetailedStatus_filters,
    getOrganizationConformancePackDetailedStatus_limit,
    getOrganizationConformancePackDetailedStatus_nextToken,
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

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetOrganizationConformancePackDetailedStatus' smart constructor.
data GetOrganizationConformancePackDetailedStatus = GetOrganizationConformancePackDetailedStatus'
  { -- | An @OrganizationResourceDetailedStatusFilters@ object.
    filters :: Prelude.Maybe OrganizationResourceDetailedStatusFilters,
    -- | The maximum number of @OrganizationConformancePackDetailedStatuses@
    -- returned on each page. If you do not specify a number, Config uses the
    -- default. The default is 100.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The nextToken string returned on a previous page that you use to get the
    -- next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of organization conformance pack for which you want status
    -- details for member accounts.
    organizationConformancePackName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOrganizationConformancePackDetailedStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'getOrganizationConformancePackDetailedStatus_filters' - An @OrganizationResourceDetailedStatusFilters@ object.
--
-- 'limit', 'getOrganizationConformancePackDetailedStatus_limit' - The maximum number of @OrganizationConformancePackDetailedStatuses@
-- returned on each page. If you do not specify a number, Config uses the
-- default. The default is 100.
--
-- 'nextToken', 'getOrganizationConformancePackDetailedStatus_nextToken' - The nextToken string returned on a previous page that you use to get the
-- next page of results in a paginated response.
--
-- 'organizationConformancePackName', 'getOrganizationConformancePackDetailedStatus_organizationConformancePackName' - The name of organization conformance pack for which you want status
-- details for member accounts.
newGetOrganizationConformancePackDetailedStatus ::
  -- | 'organizationConformancePackName'
  Prelude.Text ->
  GetOrganizationConformancePackDetailedStatus
newGetOrganizationConformancePackDetailedStatus
  pOrganizationConformancePackName_ =
    GetOrganizationConformancePackDetailedStatus'
      { filters =
          Prelude.Nothing,
        limit = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        organizationConformancePackName =
          pOrganizationConformancePackName_
      }

-- | An @OrganizationResourceDetailedStatusFilters@ object.
getOrganizationConformancePackDetailedStatus_filters :: Lens.Lens' GetOrganizationConformancePackDetailedStatus (Prelude.Maybe OrganizationResourceDetailedStatusFilters)
getOrganizationConformancePackDetailedStatus_filters = Lens.lens (\GetOrganizationConformancePackDetailedStatus' {filters} -> filters) (\s@GetOrganizationConformancePackDetailedStatus' {} a -> s {filters = a} :: GetOrganizationConformancePackDetailedStatus)

-- | The maximum number of @OrganizationConformancePackDetailedStatuses@
-- returned on each page. If you do not specify a number, Config uses the
-- default. The default is 100.
getOrganizationConformancePackDetailedStatus_limit :: Lens.Lens' GetOrganizationConformancePackDetailedStatus (Prelude.Maybe Prelude.Natural)
getOrganizationConformancePackDetailedStatus_limit = Lens.lens (\GetOrganizationConformancePackDetailedStatus' {limit} -> limit) (\s@GetOrganizationConformancePackDetailedStatus' {} a -> s {limit = a} :: GetOrganizationConformancePackDetailedStatus)

-- | The nextToken string returned on a previous page that you use to get the
-- next page of results in a paginated response.
getOrganizationConformancePackDetailedStatus_nextToken :: Lens.Lens' GetOrganizationConformancePackDetailedStatus (Prelude.Maybe Prelude.Text)
getOrganizationConformancePackDetailedStatus_nextToken = Lens.lens (\GetOrganizationConformancePackDetailedStatus' {nextToken} -> nextToken) (\s@GetOrganizationConformancePackDetailedStatus' {} a -> s {nextToken = a} :: GetOrganizationConformancePackDetailedStatus)

-- | The name of organization conformance pack for which you want status
-- details for member accounts.
getOrganizationConformancePackDetailedStatus_organizationConformancePackName :: Lens.Lens' GetOrganizationConformancePackDetailedStatus Prelude.Text
getOrganizationConformancePackDetailedStatus_organizationConformancePackName = Lens.lens (\GetOrganizationConformancePackDetailedStatus' {organizationConformancePackName} -> organizationConformancePackName) (\s@GetOrganizationConformancePackDetailedStatus' {} a -> s {organizationConformancePackName = a} :: GetOrganizationConformancePackDetailedStatus)

instance
  Core.AWSPager
    GetOrganizationConformancePackDetailedStatus
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getOrganizationConformancePackDetailedStatusResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getOrganizationConformancePackDetailedStatusResponse_organizationConformancePackDetailedStatuses
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getOrganizationConformancePackDetailedStatus_nextToken
          Lens..~ rs
            Lens.^? getOrganizationConformancePackDetailedStatusResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    GetOrganizationConformancePackDetailedStatus
  where
  type
    AWSResponse
      GetOrganizationConformancePackDetailedStatus =
      GetOrganizationConformancePackDetailedStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOrganizationConformancePackDetailedStatusResponse'
            Prelude.<$> (x Data..?> "NextToken")
              Prelude.<*> ( x
                              Data..?> "OrganizationConformancePackDetailedStatuses"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetOrganizationConformancePackDetailedStatus
  where
  hashWithSalt
    _salt
    GetOrganizationConformancePackDetailedStatus' {..} =
      _salt `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` limit
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` organizationConformancePackName

instance
  Prelude.NFData
    GetOrganizationConformancePackDetailedStatus
  where
  rnf GetOrganizationConformancePackDetailedStatus' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf organizationConformancePackName

instance
  Data.ToHeaders
    GetOrganizationConformancePackDetailedStatus
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.GetOrganizationConformancePackDetailedStatus" ::
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
    GetOrganizationConformancePackDetailedStatus
  where
  toJSON
    GetOrganizationConformancePackDetailedStatus' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Filters" Data..=) Prelude.<$> filters,
              ("Limit" Data..=) Prelude.<$> limit,
              ("NextToken" Data..=) Prelude.<$> nextToken,
              Prelude.Just
                ( "OrganizationConformancePackName"
                    Data..= organizationConformancePackName
                )
            ]
        )

instance
  Data.ToPath
    GetOrganizationConformancePackDetailedStatus
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetOrganizationConformancePackDetailedStatus
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetOrganizationConformancePackDetailedStatusResponse' smart constructor.
data GetOrganizationConformancePackDetailedStatusResponse = GetOrganizationConformancePackDetailedStatusResponse'
  { -- | The nextToken string returned on a previous page that you use to get the
    -- next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of @OrganizationConformancePackDetailedStatus@ objects.
    organizationConformancePackDetailedStatuses :: Prelude.Maybe [OrganizationConformancePackDetailedStatus],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetOrganizationConformancePackDetailedStatusResponse
newGetOrganizationConformancePackDetailedStatusResponse
  pHttpStatus_ =
    GetOrganizationConformancePackDetailedStatusResponse'
      { nextToken =
          Prelude.Nothing,
        organizationConformancePackDetailedStatuses =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The nextToken string returned on a previous page that you use to get the
-- next page of results in a paginated response.
getOrganizationConformancePackDetailedStatusResponse_nextToken :: Lens.Lens' GetOrganizationConformancePackDetailedStatusResponse (Prelude.Maybe Prelude.Text)
getOrganizationConformancePackDetailedStatusResponse_nextToken = Lens.lens (\GetOrganizationConformancePackDetailedStatusResponse' {nextToken} -> nextToken) (\s@GetOrganizationConformancePackDetailedStatusResponse' {} a -> s {nextToken = a} :: GetOrganizationConformancePackDetailedStatusResponse)

-- | A list of @OrganizationConformancePackDetailedStatus@ objects.
getOrganizationConformancePackDetailedStatusResponse_organizationConformancePackDetailedStatuses :: Lens.Lens' GetOrganizationConformancePackDetailedStatusResponse (Prelude.Maybe [OrganizationConformancePackDetailedStatus])
getOrganizationConformancePackDetailedStatusResponse_organizationConformancePackDetailedStatuses = Lens.lens (\GetOrganizationConformancePackDetailedStatusResponse' {organizationConformancePackDetailedStatuses} -> organizationConformancePackDetailedStatuses) (\s@GetOrganizationConformancePackDetailedStatusResponse' {} a -> s {organizationConformancePackDetailedStatuses = a} :: GetOrganizationConformancePackDetailedStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getOrganizationConformancePackDetailedStatusResponse_httpStatus :: Lens.Lens' GetOrganizationConformancePackDetailedStatusResponse Prelude.Int
getOrganizationConformancePackDetailedStatusResponse_httpStatus = Lens.lens (\GetOrganizationConformancePackDetailedStatusResponse' {httpStatus} -> httpStatus) (\s@GetOrganizationConformancePackDetailedStatusResponse' {} a -> s {httpStatus = a} :: GetOrganizationConformancePackDetailedStatusResponse)

instance
  Prelude.NFData
    GetOrganizationConformancePackDetailedStatusResponse
  where
  rnf
    GetOrganizationConformancePackDetailedStatusResponse' {..} =
      Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf
          organizationConformancePackDetailedStatuses
        `Prelude.seq` Prelude.rnf httpStatus
