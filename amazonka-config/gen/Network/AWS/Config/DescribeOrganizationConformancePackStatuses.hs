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
-- Module      : Network.AWS.Config.DescribeOrganizationConformancePackStatuses
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.Config.DescribeOrganizationConformancePackStatuses
  ( -- * Creating a Request
    DescribeOrganizationConformancePackStatuses (..),
    newDescribeOrganizationConformancePackStatuses,

    -- * Request Lenses
    describeOrganizationConformancePackStatuses_nextToken,
    describeOrganizationConformancePackStatuses_organizationConformancePackNames,
    describeOrganizationConformancePackStatuses_limit,

    -- * Destructuring the Response
    DescribeOrganizationConformancePackStatusesResponse (..),
    newDescribeOrganizationConformancePackStatusesResponse,

    -- * Response Lenses
    describeOrganizationConformancePackStatusesResponse_nextToken,
    describeOrganizationConformancePackStatusesResponse_organizationConformancePackStatuses,
    describeOrganizationConformancePackStatusesResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeOrganizationConformancePackStatuses' smart constructor.
data DescribeOrganizationConformancePackStatuses = DescribeOrganizationConformancePackStatuses'
  { -- | The nextToken string returned on a previous page that you use to get the
    -- next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The names of organization conformance packs for which you want status
    -- details. If you do not specify any names, AWS Config returns details for
    -- all your organization conformance packs.
    organizationConformancePackNames :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of OrganizationConformancePackStatuses returned on
    -- each page. If you do no specify a number, AWS Config uses the default.
    -- The default is 100.
    limit :: Prelude.Maybe Prelude.Natural
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
-- 'nextToken', 'describeOrganizationConformancePackStatuses_nextToken' - The nextToken string returned on a previous page that you use to get the
-- next page of results in a paginated response.
--
-- 'organizationConformancePackNames', 'describeOrganizationConformancePackStatuses_organizationConformancePackNames' - The names of organization conformance packs for which you want status
-- details. If you do not specify any names, AWS Config returns details for
-- all your organization conformance packs.
--
-- 'limit', 'describeOrganizationConformancePackStatuses_limit' - The maximum number of OrganizationConformancePackStatuses returned on
-- each page. If you do no specify a number, AWS Config uses the default.
-- The default is 100.
newDescribeOrganizationConformancePackStatuses ::
  DescribeOrganizationConformancePackStatuses
newDescribeOrganizationConformancePackStatuses =
  DescribeOrganizationConformancePackStatuses'
    { nextToken =
        Prelude.Nothing,
      organizationConformancePackNames =
        Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | The nextToken string returned on a previous page that you use to get the
-- next page of results in a paginated response.
describeOrganizationConformancePackStatuses_nextToken :: Lens.Lens' DescribeOrganizationConformancePackStatuses (Prelude.Maybe Prelude.Text)
describeOrganizationConformancePackStatuses_nextToken = Lens.lens (\DescribeOrganizationConformancePackStatuses' {nextToken} -> nextToken) (\s@DescribeOrganizationConformancePackStatuses' {} a -> s {nextToken = a} :: DescribeOrganizationConformancePackStatuses)

-- | The names of organization conformance packs for which you want status
-- details. If you do not specify any names, AWS Config returns details for
-- all your organization conformance packs.
describeOrganizationConformancePackStatuses_organizationConformancePackNames :: Lens.Lens' DescribeOrganizationConformancePackStatuses (Prelude.Maybe [Prelude.Text])
describeOrganizationConformancePackStatuses_organizationConformancePackNames = Lens.lens (\DescribeOrganizationConformancePackStatuses' {organizationConformancePackNames} -> organizationConformancePackNames) (\s@DescribeOrganizationConformancePackStatuses' {} a -> s {organizationConformancePackNames = a} :: DescribeOrganizationConformancePackStatuses) Prelude.. Lens.mapping Lens._Coerce

-- | The maximum number of OrganizationConformancePackStatuses returned on
-- each page. If you do no specify a number, AWS Config uses the default.
-- The default is 100.
describeOrganizationConformancePackStatuses_limit :: Lens.Lens' DescribeOrganizationConformancePackStatuses (Prelude.Maybe Prelude.Natural)
describeOrganizationConformancePackStatuses_limit = Lens.lens (\DescribeOrganizationConformancePackStatuses' {limit} -> limit) (\s@DescribeOrganizationConformancePackStatuses' {} a -> s {limit = a} :: DescribeOrganizationConformancePackStatuses)

instance
  Core.AWSRequest
    DescribeOrganizationConformancePackStatuses
  where
  type
    AWSResponse
      DescribeOrganizationConformancePackStatuses =
      DescribeOrganizationConformancePackStatusesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOrganizationConformancePackStatusesResponse'
            Prelude.<$> (x Core..?> "NextToken")
              Prelude.<*> ( x Core..?> "OrganizationConformancePackStatuses"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeOrganizationConformancePackStatuses

instance
  Prelude.NFData
    DescribeOrganizationConformancePackStatuses

instance
  Core.ToHeaders
    DescribeOrganizationConformancePackStatuses
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DescribeOrganizationConformancePackStatuses" ::
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
    DescribeOrganizationConformancePackStatuses
  where
  toJSON
    DescribeOrganizationConformancePackStatuses' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("NextToken" Core..=) Prelude.<$> nextToken,
              ("OrganizationConformancePackNames" Core..=)
                Prelude.<$> organizationConformancePackNames,
              ("Limit" Core..=) Prelude.<$> limit
            ]
        )

instance
  Core.ToPath
    DescribeOrganizationConformancePackStatuses
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
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
describeOrganizationConformancePackStatusesResponse_organizationConformancePackStatuses = Lens.lens (\DescribeOrganizationConformancePackStatusesResponse' {organizationConformancePackStatuses} -> organizationConformancePackStatuses) (\s@DescribeOrganizationConformancePackStatusesResponse' {} a -> s {organizationConformancePackStatuses = a} :: DescribeOrganizationConformancePackStatusesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeOrganizationConformancePackStatusesResponse_httpStatus :: Lens.Lens' DescribeOrganizationConformancePackStatusesResponse Prelude.Int
describeOrganizationConformancePackStatusesResponse_httpStatus = Lens.lens (\DescribeOrganizationConformancePackStatusesResponse' {httpStatus} -> httpStatus) (\s@DescribeOrganizationConformancePackStatusesResponse' {} a -> s {httpStatus = a} :: DescribeOrganizationConformancePackStatusesResponse)

instance
  Prelude.NFData
    DescribeOrganizationConformancePackStatusesResponse
