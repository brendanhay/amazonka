{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Config.DescribeOrganizationConfigRuleStatuses
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides organization config rule deployment status for an organization.
--
-- The status is not considered successful until organization config rule
-- is successfully deployed in all the member accounts with an exception of
-- excluded accounts.
--
-- When you specify the limit and the next token, you receive a paginated
-- response. Limit and next token are not applicable if you specify
-- organization config rule names. It is only applicable, when you request
-- all the organization config rules.
module Network.AWS.Config.DescribeOrganizationConfigRuleStatuses
  ( -- * Creating a Request
    DescribeOrganizationConfigRuleStatuses (..),
    newDescribeOrganizationConfigRuleStatuses,

    -- * Request Lenses
    describeOrganizationConfigRuleStatuses_nextToken,
    describeOrganizationConfigRuleStatuses_organizationConfigRuleNames,
    describeOrganizationConfigRuleStatuses_limit,

    -- * Destructuring the Response
    DescribeOrganizationConfigRuleStatusesResponse (..),
    newDescribeOrganizationConfigRuleStatusesResponse,

    -- * Response Lenses
    describeOrganizationConfigRuleStatusesResponse_nextToken,
    describeOrganizationConfigRuleStatusesResponse_organizationConfigRuleStatuses,
    describeOrganizationConfigRuleStatusesResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeOrganizationConfigRuleStatuses' smart constructor.
data DescribeOrganizationConfigRuleStatuses = DescribeOrganizationConfigRuleStatuses'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The names of organization config rules for which you want status
    -- details. If you do not specify any names, AWS Config returns details for
    -- all your organization AWS Confg rules.
    organizationConfigRuleNames :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of @OrganizationConfigRuleStatuses@ returned on each
    -- page. If you do no specify a number, AWS Config uses the default. The
    -- default is 100.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeOrganizationConfigRuleStatuses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeOrganizationConfigRuleStatuses_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'organizationConfigRuleNames', 'describeOrganizationConfigRuleStatuses_organizationConfigRuleNames' - The names of organization config rules for which you want status
-- details. If you do not specify any names, AWS Config returns details for
-- all your organization AWS Confg rules.
--
-- 'limit', 'describeOrganizationConfigRuleStatuses_limit' - The maximum number of @OrganizationConfigRuleStatuses@ returned on each
-- page. If you do no specify a number, AWS Config uses the default. The
-- default is 100.
newDescribeOrganizationConfigRuleStatuses ::
  DescribeOrganizationConfigRuleStatuses
newDescribeOrganizationConfigRuleStatuses =
  DescribeOrganizationConfigRuleStatuses'
    { nextToken =
        Prelude.Nothing,
      organizationConfigRuleNames =
        Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeOrganizationConfigRuleStatuses_nextToken :: Lens.Lens' DescribeOrganizationConfigRuleStatuses (Prelude.Maybe Prelude.Text)
describeOrganizationConfigRuleStatuses_nextToken = Lens.lens (\DescribeOrganizationConfigRuleStatuses' {nextToken} -> nextToken) (\s@DescribeOrganizationConfigRuleStatuses' {} a -> s {nextToken = a} :: DescribeOrganizationConfigRuleStatuses)

-- | The names of organization config rules for which you want status
-- details. If you do not specify any names, AWS Config returns details for
-- all your organization AWS Confg rules.
describeOrganizationConfigRuleStatuses_organizationConfigRuleNames :: Lens.Lens' DescribeOrganizationConfigRuleStatuses (Prelude.Maybe [Prelude.Text])
describeOrganizationConfigRuleStatuses_organizationConfigRuleNames = Lens.lens (\DescribeOrganizationConfigRuleStatuses' {organizationConfigRuleNames} -> organizationConfigRuleNames) (\s@DescribeOrganizationConfigRuleStatuses' {} a -> s {organizationConfigRuleNames = a} :: DescribeOrganizationConfigRuleStatuses) Prelude.. Lens.mapping Prelude._Coerce

-- | The maximum number of @OrganizationConfigRuleStatuses@ returned on each
-- page. If you do no specify a number, AWS Config uses the default. The
-- default is 100.
describeOrganizationConfigRuleStatuses_limit :: Lens.Lens' DescribeOrganizationConfigRuleStatuses (Prelude.Maybe Prelude.Natural)
describeOrganizationConfigRuleStatuses_limit = Lens.lens (\DescribeOrganizationConfigRuleStatuses' {limit} -> limit) (\s@DescribeOrganizationConfigRuleStatuses' {} a -> s {limit = a} :: DescribeOrganizationConfigRuleStatuses)

instance
  Prelude.AWSRequest
    DescribeOrganizationConfigRuleStatuses
  where
  type
    Rs DescribeOrganizationConfigRuleStatuses =
      DescribeOrganizationConfigRuleStatusesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOrganizationConfigRuleStatusesResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
              Prelude.<*> ( x Prelude..?> "OrganizationConfigRuleStatuses"
                              Prelude..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeOrganizationConfigRuleStatuses

instance
  Prelude.NFData
    DescribeOrganizationConfigRuleStatuses

instance
  Prelude.ToHeaders
    DescribeOrganizationConfigRuleStatuses
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StarlingDoveService.DescribeOrganizationConfigRuleStatuses" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    DescribeOrganizationConfigRuleStatuses
  where
  toJSON DescribeOrganizationConfigRuleStatuses' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("OrganizationConfigRuleNames" Prelude..=)
              Prelude.<$> organizationConfigRuleNames,
            ("Limit" Prelude..=) Prelude.<$> limit
          ]
      )

instance
  Prelude.ToPath
    DescribeOrganizationConfigRuleStatuses
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DescribeOrganizationConfigRuleStatuses
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeOrganizationConfigRuleStatusesResponse' smart constructor.
data DescribeOrganizationConfigRuleStatusesResponse = DescribeOrganizationConfigRuleStatusesResponse'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of @OrganizationConfigRuleStatus@ objects.
    organizationConfigRuleStatuses :: Prelude.Maybe [OrganizationConfigRuleStatus],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeOrganizationConfigRuleStatusesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeOrganizationConfigRuleStatusesResponse_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'organizationConfigRuleStatuses', 'describeOrganizationConfigRuleStatusesResponse_organizationConfigRuleStatuses' - A list of @OrganizationConfigRuleStatus@ objects.
--
-- 'httpStatus', 'describeOrganizationConfigRuleStatusesResponse_httpStatus' - The response's http status code.
newDescribeOrganizationConfigRuleStatusesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeOrganizationConfigRuleStatusesResponse
newDescribeOrganizationConfigRuleStatusesResponse
  pHttpStatus_ =
    DescribeOrganizationConfigRuleStatusesResponse'
      { nextToken =
          Prelude.Nothing,
        organizationConfigRuleStatuses =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeOrganizationConfigRuleStatusesResponse_nextToken :: Lens.Lens' DescribeOrganizationConfigRuleStatusesResponse (Prelude.Maybe Prelude.Text)
describeOrganizationConfigRuleStatusesResponse_nextToken = Lens.lens (\DescribeOrganizationConfigRuleStatusesResponse' {nextToken} -> nextToken) (\s@DescribeOrganizationConfigRuleStatusesResponse' {} a -> s {nextToken = a} :: DescribeOrganizationConfigRuleStatusesResponse)

-- | A list of @OrganizationConfigRuleStatus@ objects.
describeOrganizationConfigRuleStatusesResponse_organizationConfigRuleStatuses :: Lens.Lens' DescribeOrganizationConfigRuleStatusesResponse (Prelude.Maybe [OrganizationConfigRuleStatus])
describeOrganizationConfigRuleStatusesResponse_organizationConfigRuleStatuses = Lens.lens (\DescribeOrganizationConfigRuleStatusesResponse' {organizationConfigRuleStatuses} -> organizationConfigRuleStatuses) (\s@DescribeOrganizationConfigRuleStatusesResponse' {} a -> s {organizationConfigRuleStatuses = a} :: DescribeOrganizationConfigRuleStatusesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeOrganizationConfigRuleStatusesResponse_httpStatus :: Lens.Lens' DescribeOrganizationConfigRuleStatusesResponse Prelude.Int
describeOrganizationConfigRuleStatusesResponse_httpStatus = Lens.lens (\DescribeOrganizationConfigRuleStatusesResponse' {httpStatus} -> httpStatus) (\s@DescribeOrganizationConfigRuleStatusesResponse' {} a -> s {httpStatus = a} :: DescribeOrganizationConfigRuleStatusesResponse)

instance
  Prelude.NFData
    DescribeOrganizationConfigRuleStatusesResponse
