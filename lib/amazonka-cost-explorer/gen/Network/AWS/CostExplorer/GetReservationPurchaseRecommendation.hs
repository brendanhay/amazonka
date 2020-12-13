{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetReservationPurchaseRecommendation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets recommendations for which reservations to purchase. These recommendations could help you reduce your costs. Reservations provide a discounted hourly rate (up to 75%) compared to On-Demand pricing.
--
-- AWS generates your recommendations by identifying your On-Demand usage during a specific time period and collecting your usage into categories that are eligible for a reservation. After AWS has these categories, it simulates every combination of reservations in each category of usage to identify the best number of each type of RI to purchase to maximize your estimated savings.
-- For example, AWS automatically aggregates your Amazon EC2 Linux, shared tenancy, and c4 family usage in the US West (Oregon) Region and recommends that you buy size-flexible regional reservations to apply to the c4 family usage. AWS recommends the smallest size instance in an instance family. This makes it easier to purchase a size-flexible RI. AWS also shows the equal number of normalized units so that you can purchase any instance size that you want. For this example, your RI recommendation would be for @c4.large@ because that is the smallest size instance in the c4 instance family.
module Network.AWS.CostExplorer.GetReservationPurchaseRecommendation
  ( -- * Creating a request
    GetReservationPurchaseRecommendation (..),
    mkGetReservationPurchaseRecommendation,

    -- ** Request lenses
    grprNextPageToken,
    grprTermInYears,
    grprService,
    grprServiceSpecification,
    grprAccountScope,
    grprAccountId,
    grprPageSize,
    grprLookbackPeriodInDays,
    grprPaymentOption,

    -- * Destructuring the response
    GetReservationPurchaseRecommendationResponse (..),
    mkGetReservationPurchaseRecommendationResponse,

    -- ** Response lenses
    grprrsNextPageToken,
    grprrsRecommendations,
    grprrsMetadata,
    grprrsResponseStatus,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetReservationPurchaseRecommendation' smart constructor.
data GetReservationPurchaseRecommendation = GetReservationPurchaseRecommendation'
  { -- | The pagination token that indicates the next set of results that you want to retrieve.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | The reservation term that you want recommendations for.
    termInYears :: Lude.Maybe TermInYears,
    -- | The specific service that you want recommendations for.
    service :: Lude.Text,
    -- | The hardware specifications for the service instances that you want recommendations for, such as standard or convertible Amazon EC2 instances.
    serviceSpecification :: Lude.Maybe ServiceSpecification,
    -- | The account scope that you want your recommendations for. Amazon Web Services calculates recommendations including the management account and member accounts if the value is set to @PAYER@ . If the value is @LINKED@ , recommendations are calculated for individual member accounts only.
    accountScope :: Lude.Maybe AccountScope,
    -- | The account ID that is associated with the recommendation.
    accountId :: Lude.Maybe Lude.Text,
    -- | The number of recommendations that you want returned in a single response object.
    pageSize :: Lude.Maybe Lude.Natural,
    -- | The number of previous days that you want AWS to consider when it calculates your recommendations.
    lookbackPeriodInDays :: Lude.Maybe LookbackPeriodInDays,
    -- | The reservation purchase option that you want recommendations for.
    paymentOption :: Lude.Maybe PaymentOption
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetReservationPurchaseRecommendation' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The pagination token that indicates the next set of results that you want to retrieve.
-- * 'termInYears' - The reservation term that you want recommendations for.
-- * 'service' - The specific service that you want recommendations for.
-- * 'serviceSpecification' - The hardware specifications for the service instances that you want recommendations for, such as standard or convertible Amazon EC2 instances.
-- * 'accountScope' - The account scope that you want your recommendations for. Amazon Web Services calculates recommendations including the management account and member accounts if the value is set to @PAYER@ . If the value is @LINKED@ , recommendations are calculated for individual member accounts only.
-- * 'accountId' - The account ID that is associated with the recommendation.
-- * 'pageSize' - The number of recommendations that you want returned in a single response object.
-- * 'lookbackPeriodInDays' - The number of previous days that you want AWS to consider when it calculates your recommendations.
-- * 'paymentOption' - The reservation purchase option that you want recommendations for.
mkGetReservationPurchaseRecommendation ::
  -- | 'service'
  Lude.Text ->
  GetReservationPurchaseRecommendation
mkGetReservationPurchaseRecommendation pService_ =
  GetReservationPurchaseRecommendation'
    { nextPageToken =
        Lude.Nothing,
      termInYears = Lude.Nothing,
      service = pService_,
      serviceSpecification = Lude.Nothing,
      accountScope = Lude.Nothing,
      accountId = Lude.Nothing,
      pageSize = Lude.Nothing,
      lookbackPeriodInDays = Lude.Nothing,
      paymentOption = Lude.Nothing
    }

-- | The pagination token that indicates the next set of results that you want to retrieve.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprNextPageToken :: Lens.Lens' GetReservationPurchaseRecommendation (Lude.Maybe Lude.Text)
grprNextPageToken = Lens.lens (nextPageToken :: GetReservationPurchaseRecommendation -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetReservationPurchaseRecommendation)
{-# DEPRECATED grprNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The reservation term that you want recommendations for.
--
-- /Note:/ Consider using 'termInYears' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprTermInYears :: Lens.Lens' GetReservationPurchaseRecommendation (Lude.Maybe TermInYears)
grprTermInYears = Lens.lens (termInYears :: GetReservationPurchaseRecommendation -> Lude.Maybe TermInYears) (\s a -> s {termInYears = a} :: GetReservationPurchaseRecommendation)
{-# DEPRECATED grprTermInYears "Use generic-lens or generic-optics with 'termInYears' instead." #-}

-- | The specific service that you want recommendations for.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprService :: Lens.Lens' GetReservationPurchaseRecommendation Lude.Text
grprService = Lens.lens (service :: GetReservationPurchaseRecommendation -> Lude.Text) (\s a -> s {service = a} :: GetReservationPurchaseRecommendation)
{-# DEPRECATED grprService "Use generic-lens or generic-optics with 'service' instead." #-}

-- | The hardware specifications for the service instances that you want recommendations for, such as standard or convertible Amazon EC2 instances.
--
-- /Note:/ Consider using 'serviceSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprServiceSpecification :: Lens.Lens' GetReservationPurchaseRecommendation (Lude.Maybe ServiceSpecification)
grprServiceSpecification = Lens.lens (serviceSpecification :: GetReservationPurchaseRecommendation -> Lude.Maybe ServiceSpecification) (\s a -> s {serviceSpecification = a} :: GetReservationPurchaseRecommendation)
{-# DEPRECATED grprServiceSpecification "Use generic-lens or generic-optics with 'serviceSpecification' instead." #-}

-- | The account scope that you want your recommendations for. Amazon Web Services calculates recommendations including the management account and member accounts if the value is set to @PAYER@ . If the value is @LINKED@ , recommendations are calculated for individual member accounts only.
--
-- /Note:/ Consider using 'accountScope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprAccountScope :: Lens.Lens' GetReservationPurchaseRecommendation (Lude.Maybe AccountScope)
grprAccountScope = Lens.lens (accountScope :: GetReservationPurchaseRecommendation -> Lude.Maybe AccountScope) (\s a -> s {accountScope = a} :: GetReservationPurchaseRecommendation)
{-# DEPRECATED grprAccountScope "Use generic-lens or generic-optics with 'accountScope' instead." #-}

-- | The account ID that is associated with the recommendation.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprAccountId :: Lens.Lens' GetReservationPurchaseRecommendation (Lude.Maybe Lude.Text)
grprAccountId = Lens.lens (accountId :: GetReservationPurchaseRecommendation -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: GetReservationPurchaseRecommendation)
{-# DEPRECATED grprAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The number of recommendations that you want returned in a single response object.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprPageSize :: Lens.Lens' GetReservationPurchaseRecommendation (Lude.Maybe Lude.Natural)
grprPageSize = Lens.lens (pageSize :: GetReservationPurchaseRecommendation -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: GetReservationPurchaseRecommendation)
{-# DEPRECATED grprPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The number of previous days that you want AWS to consider when it calculates your recommendations.
--
-- /Note:/ Consider using 'lookbackPeriodInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprLookbackPeriodInDays :: Lens.Lens' GetReservationPurchaseRecommendation (Lude.Maybe LookbackPeriodInDays)
grprLookbackPeriodInDays = Lens.lens (lookbackPeriodInDays :: GetReservationPurchaseRecommendation -> Lude.Maybe LookbackPeriodInDays) (\s a -> s {lookbackPeriodInDays = a} :: GetReservationPurchaseRecommendation)
{-# DEPRECATED grprLookbackPeriodInDays "Use generic-lens or generic-optics with 'lookbackPeriodInDays' instead." #-}

-- | The reservation purchase option that you want recommendations for.
--
-- /Note:/ Consider using 'paymentOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprPaymentOption :: Lens.Lens' GetReservationPurchaseRecommendation (Lude.Maybe PaymentOption)
grprPaymentOption = Lens.lens (paymentOption :: GetReservationPurchaseRecommendation -> Lude.Maybe PaymentOption) (\s a -> s {paymentOption = a} :: GetReservationPurchaseRecommendation)
{-# DEPRECATED grprPaymentOption "Use generic-lens or generic-optics with 'paymentOption' instead." #-}

instance Lude.AWSRequest GetReservationPurchaseRecommendation where
  type
    Rs GetReservationPurchaseRecommendation =
      GetReservationPurchaseRecommendationResponse
  request = Req.postJSON costExplorerService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetReservationPurchaseRecommendationResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "Recommendations" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Metadata")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetReservationPurchaseRecommendation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSInsightsIndexService.GetReservationPurchaseRecommendation" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetReservationPurchaseRecommendation where
  toJSON GetReservationPurchaseRecommendation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextPageToken" Lude..=) Lude.<$> nextPageToken,
            ("TermInYears" Lude..=) Lude.<$> termInYears,
            Lude.Just ("Service" Lude..= service),
            ("ServiceSpecification" Lude..=) Lude.<$> serviceSpecification,
            ("AccountScope" Lude..=) Lude.<$> accountScope,
            ("AccountId" Lude..=) Lude.<$> accountId,
            ("PageSize" Lude..=) Lude.<$> pageSize,
            ("LookbackPeriodInDays" Lude..=) Lude.<$> lookbackPeriodInDays,
            ("PaymentOption" Lude..=) Lude.<$> paymentOption
          ]
      )

instance Lude.ToPath GetReservationPurchaseRecommendation where
  toPath = Lude.const "/"

instance Lude.ToQuery GetReservationPurchaseRecommendation where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetReservationPurchaseRecommendationResponse' smart constructor.
data GetReservationPurchaseRecommendationResponse = GetReservationPurchaseRecommendationResponse'
  { -- | The pagination token for the next set of retrievable results.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | Recommendations for reservations to purchase.
    recommendations :: Lude.Maybe [ReservationPurchaseRecommendation],
    -- | Information about this specific recommendation call, such as the time stamp for when Cost Explorer generated this recommendation.
    metadata :: Lude.Maybe ReservationPurchaseRecommendationMetadata,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetReservationPurchaseRecommendationResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The pagination token for the next set of retrievable results.
-- * 'recommendations' - Recommendations for reservations to purchase.
-- * 'metadata' - Information about this specific recommendation call, such as the time stamp for when Cost Explorer generated this recommendation.
-- * 'responseStatus' - The response status code.
mkGetReservationPurchaseRecommendationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetReservationPurchaseRecommendationResponse
mkGetReservationPurchaseRecommendationResponse pResponseStatus_ =
  GetReservationPurchaseRecommendationResponse'
    { nextPageToken =
        Lude.Nothing,
      recommendations = Lude.Nothing,
      metadata = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The pagination token for the next set of retrievable results.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsNextPageToken :: Lens.Lens' GetReservationPurchaseRecommendationResponse (Lude.Maybe Lude.Text)
grprrsNextPageToken = Lens.lens (nextPageToken :: GetReservationPurchaseRecommendationResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetReservationPurchaseRecommendationResponse)
{-# DEPRECATED grprrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | Recommendations for reservations to purchase.
--
-- /Note:/ Consider using 'recommendations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsRecommendations :: Lens.Lens' GetReservationPurchaseRecommendationResponse (Lude.Maybe [ReservationPurchaseRecommendation])
grprrsRecommendations = Lens.lens (recommendations :: GetReservationPurchaseRecommendationResponse -> Lude.Maybe [ReservationPurchaseRecommendation]) (\s a -> s {recommendations = a} :: GetReservationPurchaseRecommendationResponse)
{-# DEPRECATED grprrsRecommendations "Use generic-lens or generic-optics with 'recommendations' instead." #-}

-- | Information about this specific recommendation call, such as the time stamp for when Cost Explorer generated this recommendation.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsMetadata :: Lens.Lens' GetReservationPurchaseRecommendationResponse (Lude.Maybe ReservationPurchaseRecommendationMetadata)
grprrsMetadata = Lens.lens (metadata :: GetReservationPurchaseRecommendationResponse -> Lude.Maybe ReservationPurchaseRecommendationMetadata) (\s a -> s {metadata = a} :: GetReservationPurchaseRecommendationResponse)
{-# DEPRECATED grprrsMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsResponseStatus :: Lens.Lens' GetReservationPurchaseRecommendationResponse Lude.Int
grprrsResponseStatus = Lens.lens (responseStatus :: GetReservationPurchaseRecommendationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetReservationPurchaseRecommendationResponse)
{-# DEPRECATED grprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
