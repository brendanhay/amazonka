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
-- Module      : Network.AWS.CostExplorer.GetReservationPurchaseRecommendation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets recommendations for which reservations to purchase. These
-- recommendations could help you reduce your costs. Reservations provide a
-- discounted hourly rate (up to 75%) compared to On-Demand pricing.
--
-- Amazon Web Services generates your recommendations by identifying your
-- On-Demand usage during a specific time period and collecting your usage
-- into categories that are eligible for a reservation. After Amazon Web
-- Services has these categories, it simulates every combination of
-- reservations in each category of usage to identify the best number of
-- each type of RI to purchase to maximize your estimated savings.
--
-- For example, Amazon Web Services automatically aggregates your Amazon
-- EC2 Linux, shared tenancy, and c4 family usage in the US West (Oregon)
-- Region and recommends that you buy size-flexible regional reservations
-- to apply to the c4 family usage. Amazon Web Services recommends the
-- smallest size instance in an instance family. This makes it easier to
-- purchase a size-flexible RI. Amazon Web Services also shows the equal
-- number of normalized units so that you can purchase any instance size
-- that you want. For this example, your RI recommendation would be for
-- @c4.large@ because that is the smallest size instance in the c4 instance
-- family.
module Network.AWS.CostExplorer.GetReservationPurchaseRecommendation
  ( -- * Creating a Request
    GetReservationPurchaseRecommendation (..),
    newGetReservationPurchaseRecommendation,

    -- * Request Lenses
    getReservationPurchaseRecommendation_accountId,
    getReservationPurchaseRecommendation_paymentOption,
    getReservationPurchaseRecommendation_pageSize,
    getReservationPurchaseRecommendation_accountScope,
    getReservationPurchaseRecommendation_serviceSpecification,
    getReservationPurchaseRecommendation_termInYears,
    getReservationPurchaseRecommendation_nextPageToken,
    getReservationPurchaseRecommendation_lookbackPeriodInDays,
    getReservationPurchaseRecommendation_filter,
    getReservationPurchaseRecommendation_service,

    -- * Destructuring the Response
    GetReservationPurchaseRecommendationResponse (..),
    newGetReservationPurchaseRecommendationResponse,

    -- * Response Lenses
    getReservationPurchaseRecommendationResponse_metadata,
    getReservationPurchaseRecommendationResponse_recommendations,
    getReservationPurchaseRecommendationResponse_nextPageToken,
    getReservationPurchaseRecommendationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetReservationPurchaseRecommendation' smart constructor.
data GetReservationPurchaseRecommendation = GetReservationPurchaseRecommendation'
  { -- | The account ID that is associated with the recommendation.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The reservation purchase option that you want recommendations for.
    paymentOption :: Prelude.Maybe PaymentOption,
    -- | The number of recommendations that you want returned in a single
    -- response object.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The account scope that you want your recommendations for. Amazon Web
    -- Services calculates recommendations including the management account and
    -- member accounts if the value is set to @PAYER@. If the value is
    -- @LINKED@, recommendations are calculated for individual member accounts
    -- only.
    accountScope :: Prelude.Maybe AccountScope,
    -- | The hardware specifications for the service instances that you want
    -- recommendations for, such as standard or convertible Amazon EC2
    -- instances.
    serviceSpecification :: Prelude.Maybe ServiceSpecification,
    -- | The reservation term that you want recommendations for.
    termInYears :: Prelude.Maybe TermInYears,
    -- | The pagination token that indicates the next set of results that you
    -- want to retrieve.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The number of previous days that you want Amazon Web Services to
    -- consider when it calculates your recommendations.
    lookbackPeriodInDays :: Prelude.Maybe LookbackPeriodInDays,
    filter' :: Prelude.Maybe Expression,
    -- | The specific service that you want recommendations for.
    service :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReservationPurchaseRecommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'getReservationPurchaseRecommendation_accountId' - The account ID that is associated with the recommendation.
--
-- 'paymentOption', 'getReservationPurchaseRecommendation_paymentOption' - The reservation purchase option that you want recommendations for.
--
-- 'pageSize', 'getReservationPurchaseRecommendation_pageSize' - The number of recommendations that you want returned in a single
-- response object.
--
-- 'accountScope', 'getReservationPurchaseRecommendation_accountScope' - The account scope that you want your recommendations for. Amazon Web
-- Services calculates recommendations including the management account and
-- member accounts if the value is set to @PAYER@. If the value is
-- @LINKED@, recommendations are calculated for individual member accounts
-- only.
--
-- 'serviceSpecification', 'getReservationPurchaseRecommendation_serviceSpecification' - The hardware specifications for the service instances that you want
-- recommendations for, such as standard or convertible Amazon EC2
-- instances.
--
-- 'termInYears', 'getReservationPurchaseRecommendation_termInYears' - The reservation term that you want recommendations for.
--
-- 'nextPageToken', 'getReservationPurchaseRecommendation_nextPageToken' - The pagination token that indicates the next set of results that you
-- want to retrieve.
--
-- 'lookbackPeriodInDays', 'getReservationPurchaseRecommendation_lookbackPeriodInDays' - The number of previous days that you want Amazon Web Services to
-- consider when it calculates your recommendations.
--
-- 'filter'', 'getReservationPurchaseRecommendation_filter' - Undocumented member.
--
-- 'service', 'getReservationPurchaseRecommendation_service' - The specific service that you want recommendations for.
newGetReservationPurchaseRecommendation ::
  -- | 'service'
  Prelude.Text ->
  GetReservationPurchaseRecommendation
newGetReservationPurchaseRecommendation pService_ =
  GetReservationPurchaseRecommendation'
    { accountId =
        Prelude.Nothing,
      paymentOption = Prelude.Nothing,
      pageSize = Prelude.Nothing,
      accountScope = Prelude.Nothing,
      serviceSpecification =
        Prelude.Nothing,
      termInYears = Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      lookbackPeriodInDays =
        Prelude.Nothing,
      filter' = Prelude.Nothing,
      service = pService_
    }

-- | The account ID that is associated with the recommendation.
getReservationPurchaseRecommendation_accountId :: Lens.Lens' GetReservationPurchaseRecommendation (Prelude.Maybe Prelude.Text)
getReservationPurchaseRecommendation_accountId = Lens.lens (\GetReservationPurchaseRecommendation' {accountId} -> accountId) (\s@GetReservationPurchaseRecommendation' {} a -> s {accountId = a} :: GetReservationPurchaseRecommendation)

-- | The reservation purchase option that you want recommendations for.
getReservationPurchaseRecommendation_paymentOption :: Lens.Lens' GetReservationPurchaseRecommendation (Prelude.Maybe PaymentOption)
getReservationPurchaseRecommendation_paymentOption = Lens.lens (\GetReservationPurchaseRecommendation' {paymentOption} -> paymentOption) (\s@GetReservationPurchaseRecommendation' {} a -> s {paymentOption = a} :: GetReservationPurchaseRecommendation)

-- | The number of recommendations that you want returned in a single
-- response object.
getReservationPurchaseRecommendation_pageSize :: Lens.Lens' GetReservationPurchaseRecommendation (Prelude.Maybe Prelude.Natural)
getReservationPurchaseRecommendation_pageSize = Lens.lens (\GetReservationPurchaseRecommendation' {pageSize} -> pageSize) (\s@GetReservationPurchaseRecommendation' {} a -> s {pageSize = a} :: GetReservationPurchaseRecommendation)

-- | The account scope that you want your recommendations for. Amazon Web
-- Services calculates recommendations including the management account and
-- member accounts if the value is set to @PAYER@. If the value is
-- @LINKED@, recommendations are calculated for individual member accounts
-- only.
getReservationPurchaseRecommendation_accountScope :: Lens.Lens' GetReservationPurchaseRecommendation (Prelude.Maybe AccountScope)
getReservationPurchaseRecommendation_accountScope = Lens.lens (\GetReservationPurchaseRecommendation' {accountScope} -> accountScope) (\s@GetReservationPurchaseRecommendation' {} a -> s {accountScope = a} :: GetReservationPurchaseRecommendation)

-- | The hardware specifications for the service instances that you want
-- recommendations for, such as standard or convertible Amazon EC2
-- instances.
getReservationPurchaseRecommendation_serviceSpecification :: Lens.Lens' GetReservationPurchaseRecommendation (Prelude.Maybe ServiceSpecification)
getReservationPurchaseRecommendation_serviceSpecification = Lens.lens (\GetReservationPurchaseRecommendation' {serviceSpecification} -> serviceSpecification) (\s@GetReservationPurchaseRecommendation' {} a -> s {serviceSpecification = a} :: GetReservationPurchaseRecommendation)

-- | The reservation term that you want recommendations for.
getReservationPurchaseRecommendation_termInYears :: Lens.Lens' GetReservationPurchaseRecommendation (Prelude.Maybe TermInYears)
getReservationPurchaseRecommendation_termInYears = Lens.lens (\GetReservationPurchaseRecommendation' {termInYears} -> termInYears) (\s@GetReservationPurchaseRecommendation' {} a -> s {termInYears = a} :: GetReservationPurchaseRecommendation)

-- | The pagination token that indicates the next set of results that you
-- want to retrieve.
getReservationPurchaseRecommendation_nextPageToken :: Lens.Lens' GetReservationPurchaseRecommendation (Prelude.Maybe Prelude.Text)
getReservationPurchaseRecommendation_nextPageToken = Lens.lens (\GetReservationPurchaseRecommendation' {nextPageToken} -> nextPageToken) (\s@GetReservationPurchaseRecommendation' {} a -> s {nextPageToken = a} :: GetReservationPurchaseRecommendation)

-- | The number of previous days that you want Amazon Web Services to
-- consider when it calculates your recommendations.
getReservationPurchaseRecommendation_lookbackPeriodInDays :: Lens.Lens' GetReservationPurchaseRecommendation (Prelude.Maybe LookbackPeriodInDays)
getReservationPurchaseRecommendation_lookbackPeriodInDays = Lens.lens (\GetReservationPurchaseRecommendation' {lookbackPeriodInDays} -> lookbackPeriodInDays) (\s@GetReservationPurchaseRecommendation' {} a -> s {lookbackPeriodInDays = a} :: GetReservationPurchaseRecommendation)

-- | Undocumented member.
getReservationPurchaseRecommendation_filter :: Lens.Lens' GetReservationPurchaseRecommendation (Prelude.Maybe Expression)
getReservationPurchaseRecommendation_filter = Lens.lens (\GetReservationPurchaseRecommendation' {filter'} -> filter') (\s@GetReservationPurchaseRecommendation' {} a -> s {filter' = a} :: GetReservationPurchaseRecommendation)

-- | The specific service that you want recommendations for.
getReservationPurchaseRecommendation_service :: Lens.Lens' GetReservationPurchaseRecommendation Prelude.Text
getReservationPurchaseRecommendation_service = Lens.lens (\GetReservationPurchaseRecommendation' {service} -> service) (\s@GetReservationPurchaseRecommendation' {} a -> s {service = a} :: GetReservationPurchaseRecommendation)

instance
  Core.AWSRequest
    GetReservationPurchaseRecommendation
  where
  type
    AWSResponse GetReservationPurchaseRecommendation =
      GetReservationPurchaseRecommendationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetReservationPurchaseRecommendationResponse'
            Prelude.<$> (x Core..?> "Metadata")
              Prelude.<*> ( x Core..?> "Recommendations"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (x Core..?> "NextPageToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetReservationPurchaseRecommendation

instance
  Prelude.NFData
    GetReservationPurchaseRecommendation

instance
  Core.ToHeaders
    GetReservationPurchaseRecommendation
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSInsightsIndexService.GetReservationPurchaseRecommendation" ::
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
    GetReservationPurchaseRecommendation
  where
  toJSON GetReservationPurchaseRecommendation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AccountId" Core..=) Prelude.<$> accountId,
            ("PaymentOption" Core..=) Prelude.<$> paymentOption,
            ("PageSize" Core..=) Prelude.<$> pageSize,
            ("AccountScope" Core..=) Prelude.<$> accountScope,
            ("ServiceSpecification" Core..=)
              Prelude.<$> serviceSpecification,
            ("TermInYears" Core..=) Prelude.<$> termInYears,
            ("NextPageToken" Core..=) Prelude.<$> nextPageToken,
            ("LookbackPeriodInDays" Core..=)
              Prelude.<$> lookbackPeriodInDays,
            ("Filter" Core..=) Prelude.<$> filter',
            Prelude.Just ("Service" Core..= service)
          ]
      )

instance
  Core.ToPath
    GetReservationPurchaseRecommendation
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    GetReservationPurchaseRecommendation
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetReservationPurchaseRecommendationResponse' smart constructor.
data GetReservationPurchaseRecommendationResponse = GetReservationPurchaseRecommendationResponse'
  { -- | Information about this specific recommendation call, such as the time
    -- stamp for when Cost Explorer generated this recommendation.
    metadata :: Prelude.Maybe ReservationPurchaseRecommendationMetadata,
    -- | Recommendations for reservations to purchase.
    recommendations :: Prelude.Maybe [ReservationPurchaseRecommendation],
    -- | The pagination token for the next set of retrievable results.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReservationPurchaseRecommendationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadata', 'getReservationPurchaseRecommendationResponse_metadata' - Information about this specific recommendation call, such as the time
-- stamp for when Cost Explorer generated this recommendation.
--
-- 'recommendations', 'getReservationPurchaseRecommendationResponse_recommendations' - Recommendations for reservations to purchase.
--
-- 'nextPageToken', 'getReservationPurchaseRecommendationResponse_nextPageToken' - The pagination token for the next set of retrievable results.
--
-- 'httpStatus', 'getReservationPurchaseRecommendationResponse_httpStatus' - The response's http status code.
newGetReservationPurchaseRecommendationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetReservationPurchaseRecommendationResponse
newGetReservationPurchaseRecommendationResponse
  pHttpStatus_ =
    GetReservationPurchaseRecommendationResponse'
      { metadata =
          Prelude.Nothing,
        recommendations =
          Prelude.Nothing,
        nextPageToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about this specific recommendation call, such as the time
-- stamp for when Cost Explorer generated this recommendation.
getReservationPurchaseRecommendationResponse_metadata :: Lens.Lens' GetReservationPurchaseRecommendationResponse (Prelude.Maybe ReservationPurchaseRecommendationMetadata)
getReservationPurchaseRecommendationResponse_metadata = Lens.lens (\GetReservationPurchaseRecommendationResponse' {metadata} -> metadata) (\s@GetReservationPurchaseRecommendationResponse' {} a -> s {metadata = a} :: GetReservationPurchaseRecommendationResponse)

-- | Recommendations for reservations to purchase.
getReservationPurchaseRecommendationResponse_recommendations :: Lens.Lens' GetReservationPurchaseRecommendationResponse (Prelude.Maybe [ReservationPurchaseRecommendation])
getReservationPurchaseRecommendationResponse_recommendations = Lens.lens (\GetReservationPurchaseRecommendationResponse' {recommendations} -> recommendations) (\s@GetReservationPurchaseRecommendationResponse' {} a -> s {recommendations = a} :: GetReservationPurchaseRecommendationResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The pagination token for the next set of retrievable results.
getReservationPurchaseRecommendationResponse_nextPageToken :: Lens.Lens' GetReservationPurchaseRecommendationResponse (Prelude.Maybe Prelude.Text)
getReservationPurchaseRecommendationResponse_nextPageToken = Lens.lens (\GetReservationPurchaseRecommendationResponse' {nextPageToken} -> nextPageToken) (\s@GetReservationPurchaseRecommendationResponse' {} a -> s {nextPageToken = a} :: GetReservationPurchaseRecommendationResponse)

-- | The response's http status code.
getReservationPurchaseRecommendationResponse_httpStatus :: Lens.Lens' GetReservationPurchaseRecommendationResponse Prelude.Int
getReservationPurchaseRecommendationResponse_httpStatus = Lens.lens (\GetReservationPurchaseRecommendationResponse' {httpStatus} -> httpStatus) (\s@GetReservationPurchaseRecommendationResponse' {} a -> s {httpStatus = a} :: GetReservationPurchaseRecommendationResponse)

instance
  Prelude.NFData
    GetReservationPurchaseRecommendationResponse
