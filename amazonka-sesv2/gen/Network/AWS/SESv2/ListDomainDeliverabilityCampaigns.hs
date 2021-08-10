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
-- Module      : Network.AWS.SESv2.ListDomainDeliverabilityCampaigns
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve deliverability data for all the campaigns that used a specific
-- domain to send email during a specified time range. This data is
-- available for a domain only if you enabled the Deliverability dashboard
-- for the domain.
module Network.AWS.SESv2.ListDomainDeliverabilityCampaigns
  ( -- * Creating a Request
    ListDomainDeliverabilityCampaigns (..),
    newListDomainDeliverabilityCampaigns,

    -- * Request Lenses
    listDomainDeliverabilityCampaigns_nextToken,
    listDomainDeliverabilityCampaigns_pageSize,
    listDomainDeliverabilityCampaigns_startDate,
    listDomainDeliverabilityCampaigns_endDate,
    listDomainDeliverabilityCampaigns_subscribedDomain,

    -- * Destructuring the Response
    ListDomainDeliverabilityCampaignsResponse (..),
    newListDomainDeliverabilityCampaignsResponse,

    -- * Response Lenses
    listDomainDeliverabilityCampaignsResponse_nextToken,
    listDomainDeliverabilityCampaignsResponse_httpStatus,
    listDomainDeliverabilityCampaignsResponse_domainDeliverabilityCampaigns,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

-- | Retrieve deliverability data for all the campaigns that used a specific
-- domain to send email during a specified time range. This data is
-- available for a domain only if you enabled the Deliverability dashboard.
--
-- /See:/ 'newListDomainDeliverabilityCampaigns' smart constructor.
data ListDomainDeliverabilityCampaigns = ListDomainDeliverabilityCampaigns'
  { -- | A token that’s returned from a previous call to the
    -- @ListDomainDeliverabilityCampaigns@ operation. This token indicates the
    -- position of a campaign in the list of campaigns.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to include in response to a single call to
    -- the @ListDomainDeliverabilityCampaigns@ operation. If the number of
    -- results is larger than the number that you specify in this parameter,
    -- the response includes a @NextToken@ element, which you can use to obtain
    -- additional results.
    pageSize :: Prelude.Maybe Prelude.Int,
    -- | The first day, in Unix time format, that you want to obtain
    -- deliverability data for.
    startDate :: Core.POSIX,
    -- | The last day, in Unix time format, that you want to obtain
    -- deliverability data for. This value has to be less than or equal to 30
    -- days after the value of the @StartDate@ parameter.
    endDate :: Core.POSIX,
    -- | The domain to obtain deliverability data for.
    subscribedDomain :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDomainDeliverabilityCampaigns' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDomainDeliverabilityCampaigns_nextToken' - A token that’s returned from a previous call to the
-- @ListDomainDeliverabilityCampaigns@ operation. This token indicates the
-- position of a campaign in the list of campaigns.
--
-- 'pageSize', 'listDomainDeliverabilityCampaigns_pageSize' - The maximum number of results to include in response to a single call to
-- the @ListDomainDeliverabilityCampaigns@ operation. If the number of
-- results is larger than the number that you specify in this parameter,
-- the response includes a @NextToken@ element, which you can use to obtain
-- additional results.
--
-- 'startDate', 'listDomainDeliverabilityCampaigns_startDate' - The first day, in Unix time format, that you want to obtain
-- deliverability data for.
--
-- 'endDate', 'listDomainDeliverabilityCampaigns_endDate' - The last day, in Unix time format, that you want to obtain
-- deliverability data for. This value has to be less than or equal to 30
-- days after the value of the @StartDate@ parameter.
--
-- 'subscribedDomain', 'listDomainDeliverabilityCampaigns_subscribedDomain' - The domain to obtain deliverability data for.
newListDomainDeliverabilityCampaigns ::
  -- | 'startDate'
  Prelude.UTCTime ->
  -- | 'endDate'
  Prelude.UTCTime ->
  -- | 'subscribedDomain'
  Prelude.Text ->
  ListDomainDeliverabilityCampaigns
newListDomainDeliverabilityCampaigns
  pStartDate_
  pEndDate_
  pSubscribedDomain_ =
    ListDomainDeliverabilityCampaigns'
      { nextToken =
          Prelude.Nothing,
        pageSize = Prelude.Nothing,
        startDate =
          Core._Time Lens.# pStartDate_,
        endDate = Core._Time Lens.# pEndDate_,
        subscribedDomain = pSubscribedDomain_
      }

-- | A token that’s returned from a previous call to the
-- @ListDomainDeliverabilityCampaigns@ operation. This token indicates the
-- position of a campaign in the list of campaigns.
listDomainDeliverabilityCampaigns_nextToken :: Lens.Lens' ListDomainDeliverabilityCampaigns (Prelude.Maybe Prelude.Text)
listDomainDeliverabilityCampaigns_nextToken = Lens.lens (\ListDomainDeliverabilityCampaigns' {nextToken} -> nextToken) (\s@ListDomainDeliverabilityCampaigns' {} a -> s {nextToken = a} :: ListDomainDeliverabilityCampaigns)

-- | The maximum number of results to include in response to a single call to
-- the @ListDomainDeliverabilityCampaigns@ operation. If the number of
-- results is larger than the number that you specify in this parameter,
-- the response includes a @NextToken@ element, which you can use to obtain
-- additional results.
listDomainDeliverabilityCampaigns_pageSize :: Lens.Lens' ListDomainDeliverabilityCampaigns (Prelude.Maybe Prelude.Int)
listDomainDeliverabilityCampaigns_pageSize = Lens.lens (\ListDomainDeliverabilityCampaigns' {pageSize} -> pageSize) (\s@ListDomainDeliverabilityCampaigns' {} a -> s {pageSize = a} :: ListDomainDeliverabilityCampaigns)

-- | The first day, in Unix time format, that you want to obtain
-- deliverability data for.
listDomainDeliverabilityCampaigns_startDate :: Lens.Lens' ListDomainDeliverabilityCampaigns Prelude.UTCTime
listDomainDeliverabilityCampaigns_startDate = Lens.lens (\ListDomainDeliverabilityCampaigns' {startDate} -> startDate) (\s@ListDomainDeliverabilityCampaigns' {} a -> s {startDate = a} :: ListDomainDeliverabilityCampaigns) Prelude.. Core._Time

-- | The last day, in Unix time format, that you want to obtain
-- deliverability data for. This value has to be less than or equal to 30
-- days after the value of the @StartDate@ parameter.
listDomainDeliverabilityCampaigns_endDate :: Lens.Lens' ListDomainDeliverabilityCampaigns Prelude.UTCTime
listDomainDeliverabilityCampaigns_endDate = Lens.lens (\ListDomainDeliverabilityCampaigns' {endDate} -> endDate) (\s@ListDomainDeliverabilityCampaigns' {} a -> s {endDate = a} :: ListDomainDeliverabilityCampaigns) Prelude.. Core._Time

-- | The domain to obtain deliverability data for.
listDomainDeliverabilityCampaigns_subscribedDomain :: Lens.Lens' ListDomainDeliverabilityCampaigns Prelude.Text
listDomainDeliverabilityCampaigns_subscribedDomain = Lens.lens (\ListDomainDeliverabilityCampaigns' {subscribedDomain} -> subscribedDomain) (\s@ListDomainDeliverabilityCampaigns' {} a -> s {subscribedDomain = a} :: ListDomainDeliverabilityCampaigns)

instance
  Core.AWSRequest
    ListDomainDeliverabilityCampaigns
  where
  type
    AWSResponse ListDomainDeliverabilityCampaigns =
      ListDomainDeliverabilityCampaignsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDomainDeliverabilityCampaignsResponse'
            Prelude.<$> (x Core..?> "NextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> ( x Core..?> "DomainDeliverabilityCampaigns"
                              Core..!@ Prelude.mempty
                          )
      )

instance
  Prelude.Hashable
    ListDomainDeliverabilityCampaigns

instance
  Prelude.NFData
    ListDomainDeliverabilityCampaigns

instance
  Core.ToHeaders
    ListDomainDeliverabilityCampaigns
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToPath
    ListDomainDeliverabilityCampaigns
  where
  toPath ListDomainDeliverabilityCampaigns' {..} =
    Prelude.mconcat
      [ "/v2/email/deliverability-dashboard/domains/",
        Core.toBS subscribedDomain,
        "/campaigns"
      ]

instance
  Core.ToQuery
    ListDomainDeliverabilityCampaigns
  where
  toQuery ListDomainDeliverabilityCampaigns' {..} =
    Prelude.mconcat
      [ "NextToken" Core.=: nextToken,
        "PageSize" Core.=: pageSize,
        "StartDate" Core.=: startDate,
        "EndDate" Core.=: endDate
      ]

-- | An array of objects that provide deliverability data for all the
-- campaigns that used a specific domain to send email during a specified
-- time range. This data is available for a domain only if you enabled the
-- Deliverability dashboard for the domain.
--
-- /See:/ 'newListDomainDeliverabilityCampaignsResponse' smart constructor.
data ListDomainDeliverabilityCampaignsResponse = ListDomainDeliverabilityCampaignsResponse'
  { -- | A token that’s returned from a previous call to the
    -- @ListDomainDeliverabilityCampaigns@ operation. This token indicates the
    -- position of the campaign in the list of campaigns.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of responses, one for each campaign that used the domain to
    -- send email during the specified time range.
    domainDeliverabilityCampaigns :: [DomainDeliverabilityCampaign]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDomainDeliverabilityCampaignsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDomainDeliverabilityCampaignsResponse_nextToken' - A token that’s returned from a previous call to the
-- @ListDomainDeliverabilityCampaigns@ operation. This token indicates the
-- position of the campaign in the list of campaigns.
--
-- 'httpStatus', 'listDomainDeliverabilityCampaignsResponse_httpStatus' - The response's http status code.
--
-- 'domainDeliverabilityCampaigns', 'listDomainDeliverabilityCampaignsResponse_domainDeliverabilityCampaigns' - An array of responses, one for each campaign that used the domain to
-- send email during the specified time range.
newListDomainDeliverabilityCampaignsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDomainDeliverabilityCampaignsResponse
newListDomainDeliverabilityCampaignsResponse
  pHttpStatus_ =
    ListDomainDeliverabilityCampaignsResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        domainDeliverabilityCampaigns =
          Prelude.mempty
      }

-- | A token that’s returned from a previous call to the
-- @ListDomainDeliverabilityCampaigns@ operation. This token indicates the
-- position of the campaign in the list of campaigns.
listDomainDeliverabilityCampaignsResponse_nextToken :: Lens.Lens' ListDomainDeliverabilityCampaignsResponse (Prelude.Maybe Prelude.Text)
listDomainDeliverabilityCampaignsResponse_nextToken = Lens.lens (\ListDomainDeliverabilityCampaignsResponse' {nextToken} -> nextToken) (\s@ListDomainDeliverabilityCampaignsResponse' {} a -> s {nextToken = a} :: ListDomainDeliverabilityCampaignsResponse)

-- | The response's http status code.
listDomainDeliverabilityCampaignsResponse_httpStatus :: Lens.Lens' ListDomainDeliverabilityCampaignsResponse Prelude.Int
listDomainDeliverabilityCampaignsResponse_httpStatus = Lens.lens (\ListDomainDeliverabilityCampaignsResponse' {httpStatus} -> httpStatus) (\s@ListDomainDeliverabilityCampaignsResponse' {} a -> s {httpStatus = a} :: ListDomainDeliverabilityCampaignsResponse)

-- | An array of responses, one for each campaign that used the domain to
-- send email during the specified time range.
listDomainDeliverabilityCampaignsResponse_domainDeliverabilityCampaigns :: Lens.Lens' ListDomainDeliverabilityCampaignsResponse [DomainDeliverabilityCampaign]
listDomainDeliverabilityCampaignsResponse_domainDeliverabilityCampaigns = Lens.lens (\ListDomainDeliverabilityCampaignsResponse' {domainDeliverabilityCampaigns} -> domainDeliverabilityCampaigns) (\s@ListDomainDeliverabilityCampaignsResponse' {} a -> s {domainDeliverabilityCampaigns = a} :: ListDomainDeliverabilityCampaignsResponse) Prelude.. Lens._Coerce

instance
  Prelude.NFData
    ListDomainDeliverabilityCampaignsResponse
