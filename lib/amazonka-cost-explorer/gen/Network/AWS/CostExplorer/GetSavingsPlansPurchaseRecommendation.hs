{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetSavingsPlansPurchaseRecommendation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves your request parameters, Savings Plan Recommendations Summary and Details.
module Network.AWS.CostExplorer.GetSavingsPlansPurchaseRecommendation
  ( -- * Creating a request
    GetSavingsPlansPurchaseRecommendation (..),
    mkGetSavingsPlansPurchaseRecommendation,

    -- ** Request lenses
    gspprNextPageToken,
    gspprTermInYears,
    gspprAccountScope,
    gspprSavingsPlansType,
    gspprFilter,
    gspprPageSize,
    gspprLookbackPeriodInDays,
    gspprPaymentOption,

    -- * Destructuring the response
    GetSavingsPlansPurchaseRecommendationResponse (..),
    mkGetSavingsPlansPurchaseRecommendationResponse,

    -- ** Response lenses
    gspprrsNextPageToken,
    gspprrsSavingsPlansPurchaseRecommendation,
    gspprrsMetadata,
    gspprrsResponseStatus,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSavingsPlansPurchaseRecommendation' smart constructor.
data GetSavingsPlansPurchaseRecommendation = GetSavingsPlansPurchaseRecommendation'
  { -- | The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | The savings plan recommendation term used to generate these recommendations.
    termInYears :: TermInYears,
    -- | The account scope that you want your recommendations for. Amazon Web Services calculates recommendations including the management account and member accounts if the value is set to @PAYER@ . If the value is @LINKED@ , recommendations are calculated for individual member accounts only.
    accountScope :: Lude.Maybe AccountScope,
    -- | The Savings Plans recommendation type requested.
    savingsPlansType :: SupportedSavingsPlansType,
    -- | You can filter your recommendations by Account ID with the @LINKED_ACCOUNT@ dimension. To filter your recommendations by Account ID, specify @Key@ as @LINKED_ACCOUNT@ and @Value@ as the comma-separated Acount ID(s) for which you want to see Savings Plans purchase recommendations.
    --
    -- For GetSavingsPlansPurchaseRecommendation, the @Filter@ does not include @CostCategories@ or @Tags@ . It only includes @Dimensions@ . With @Dimensions@ , @Key@ must be @LINKED_ACCOUNT@ and @Value@ can be a single Account ID or multiple comma-separated Account IDs for which you want to see Savings Plans Purchase Recommendations. @AND@ and @OR@ operators are not supported.
    filter :: Lude.Maybe Expression,
    -- | The number of recommendations that you want returned in a single response object.
    pageSize :: Lude.Maybe Lude.Natural,
    -- | The lookback period used to generate the recommendation.
    lookbackPeriodInDays :: LookbackPeriodInDays,
    -- | The payment option used to generate these recommendations.
    paymentOption :: PaymentOption
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSavingsPlansPurchaseRecommendation' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
-- * 'termInYears' - The savings plan recommendation term used to generate these recommendations.
-- * 'accountScope' - The account scope that you want your recommendations for. Amazon Web Services calculates recommendations including the management account and member accounts if the value is set to @PAYER@ . If the value is @LINKED@ , recommendations are calculated for individual member accounts only.
-- * 'savingsPlansType' - The Savings Plans recommendation type requested.
-- * 'filter' - You can filter your recommendations by Account ID with the @LINKED_ACCOUNT@ dimension. To filter your recommendations by Account ID, specify @Key@ as @LINKED_ACCOUNT@ and @Value@ as the comma-separated Acount ID(s) for which you want to see Savings Plans purchase recommendations.
--
-- For GetSavingsPlansPurchaseRecommendation, the @Filter@ does not include @CostCategories@ or @Tags@ . It only includes @Dimensions@ . With @Dimensions@ , @Key@ must be @LINKED_ACCOUNT@ and @Value@ can be a single Account ID or multiple comma-separated Account IDs for which you want to see Savings Plans Purchase Recommendations. @AND@ and @OR@ operators are not supported.
-- * 'pageSize' - The number of recommendations that you want returned in a single response object.
-- * 'lookbackPeriodInDays' - The lookback period used to generate the recommendation.
-- * 'paymentOption' - The payment option used to generate these recommendations.
mkGetSavingsPlansPurchaseRecommendation ::
  -- | 'termInYears'
  TermInYears ->
  -- | 'savingsPlansType'
  SupportedSavingsPlansType ->
  -- | 'lookbackPeriodInDays'
  LookbackPeriodInDays ->
  -- | 'paymentOption'
  PaymentOption ->
  GetSavingsPlansPurchaseRecommendation
mkGetSavingsPlansPurchaseRecommendation
  pTermInYears_
  pSavingsPlansType_
  pLookbackPeriodInDays_
  pPaymentOption_ =
    GetSavingsPlansPurchaseRecommendation'
      { nextPageToken =
          Lude.Nothing,
        termInYears = pTermInYears_,
        accountScope = Lude.Nothing,
        savingsPlansType = pSavingsPlansType_,
        filter = Lude.Nothing,
        pageSize = Lude.Nothing,
        lookbackPeriodInDays = pLookbackPeriodInDays_,
        paymentOption = pPaymentOption_
      }

-- | The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspprNextPageToken :: Lens.Lens' GetSavingsPlansPurchaseRecommendation (Lude.Maybe Lude.Text)
gspprNextPageToken = Lens.lens (nextPageToken :: GetSavingsPlansPurchaseRecommendation -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetSavingsPlansPurchaseRecommendation)
{-# DEPRECATED gspprNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The savings plan recommendation term used to generate these recommendations.
--
-- /Note:/ Consider using 'termInYears' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspprTermInYears :: Lens.Lens' GetSavingsPlansPurchaseRecommendation TermInYears
gspprTermInYears = Lens.lens (termInYears :: GetSavingsPlansPurchaseRecommendation -> TermInYears) (\s a -> s {termInYears = a} :: GetSavingsPlansPurchaseRecommendation)
{-# DEPRECATED gspprTermInYears "Use generic-lens or generic-optics with 'termInYears' instead." #-}

-- | The account scope that you want your recommendations for. Amazon Web Services calculates recommendations including the management account and member accounts if the value is set to @PAYER@ . If the value is @LINKED@ , recommendations are calculated for individual member accounts only.
--
-- /Note:/ Consider using 'accountScope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspprAccountScope :: Lens.Lens' GetSavingsPlansPurchaseRecommendation (Lude.Maybe AccountScope)
gspprAccountScope = Lens.lens (accountScope :: GetSavingsPlansPurchaseRecommendation -> Lude.Maybe AccountScope) (\s a -> s {accountScope = a} :: GetSavingsPlansPurchaseRecommendation)
{-# DEPRECATED gspprAccountScope "Use generic-lens or generic-optics with 'accountScope' instead." #-}

-- | The Savings Plans recommendation type requested.
--
-- /Note:/ Consider using 'savingsPlansType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspprSavingsPlansType :: Lens.Lens' GetSavingsPlansPurchaseRecommendation SupportedSavingsPlansType
gspprSavingsPlansType = Lens.lens (savingsPlansType :: GetSavingsPlansPurchaseRecommendation -> SupportedSavingsPlansType) (\s a -> s {savingsPlansType = a} :: GetSavingsPlansPurchaseRecommendation)
{-# DEPRECATED gspprSavingsPlansType "Use generic-lens or generic-optics with 'savingsPlansType' instead." #-}

-- | You can filter your recommendations by Account ID with the @LINKED_ACCOUNT@ dimension. To filter your recommendations by Account ID, specify @Key@ as @LINKED_ACCOUNT@ and @Value@ as the comma-separated Acount ID(s) for which you want to see Savings Plans purchase recommendations.
--
-- For GetSavingsPlansPurchaseRecommendation, the @Filter@ does not include @CostCategories@ or @Tags@ . It only includes @Dimensions@ . With @Dimensions@ , @Key@ must be @LINKED_ACCOUNT@ and @Value@ can be a single Account ID or multiple comma-separated Account IDs for which you want to see Savings Plans Purchase Recommendations. @AND@ and @OR@ operators are not supported.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspprFilter :: Lens.Lens' GetSavingsPlansPurchaseRecommendation (Lude.Maybe Expression)
gspprFilter = Lens.lens (filter :: GetSavingsPlansPurchaseRecommendation -> Lude.Maybe Expression) (\s a -> s {filter = a} :: GetSavingsPlansPurchaseRecommendation)
{-# DEPRECATED gspprFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The number of recommendations that you want returned in a single response object.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspprPageSize :: Lens.Lens' GetSavingsPlansPurchaseRecommendation (Lude.Maybe Lude.Natural)
gspprPageSize = Lens.lens (pageSize :: GetSavingsPlansPurchaseRecommendation -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: GetSavingsPlansPurchaseRecommendation)
{-# DEPRECATED gspprPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The lookback period used to generate the recommendation.
--
-- /Note:/ Consider using 'lookbackPeriodInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspprLookbackPeriodInDays :: Lens.Lens' GetSavingsPlansPurchaseRecommendation LookbackPeriodInDays
gspprLookbackPeriodInDays = Lens.lens (lookbackPeriodInDays :: GetSavingsPlansPurchaseRecommendation -> LookbackPeriodInDays) (\s a -> s {lookbackPeriodInDays = a} :: GetSavingsPlansPurchaseRecommendation)
{-# DEPRECATED gspprLookbackPeriodInDays "Use generic-lens or generic-optics with 'lookbackPeriodInDays' instead." #-}

-- | The payment option used to generate these recommendations.
--
-- /Note:/ Consider using 'paymentOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspprPaymentOption :: Lens.Lens' GetSavingsPlansPurchaseRecommendation PaymentOption
gspprPaymentOption = Lens.lens (paymentOption :: GetSavingsPlansPurchaseRecommendation -> PaymentOption) (\s a -> s {paymentOption = a} :: GetSavingsPlansPurchaseRecommendation)
{-# DEPRECATED gspprPaymentOption "Use generic-lens or generic-optics with 'paymentOption' instead." #-}

instance Lude.AWSRequest GetSavingsPlansPurchaseRecommendation where
  type
    Rs GetSavingsPlansPurchaseRecommendation =
      GetSavingsPlansPurchaseRecommendationResponse
  request = Req.postJSON costExplorerService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSavingsPlansPurchaseRecommendationResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "SavingsPlansPurchaseRecommendation")
            Lude.<*> (x Lude..?> "Metadata")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSavingsPlansPurchaseRecommendation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSInsightsIndexService.GetSavingsPlansPurchaseRecommendation" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetSavingsPlansPurchaseRecommendation where
  toJSON GetSavingsPlansPurchaseRecommendation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextPageToken" Lude..=) Lude.<$> nextPageToken,
            Lude.Just ("TermInYears" Lude..= termInYears),
            ("AccountScope" Lude..=) Lude.<$> accountScope,
            Lude.Just ("SavingsPlansType" Lude..= savingsPlansType),
            ("Filter" Lude..=) Lude.<$> filter,
            ("PageSize" Lude..=) Lude.<$> pageSize,
            Lude.Just ("LookbackPeriodInDays" Lude..= lookbackPeriodInDays),
            Lude.Just ("PaymentOption" Lude..= paymentOption)
          ]
      )

instance Lude.ToPath GetSavingsPlansPurchaseRecommendation where
  toPath = Lude.const "/"

instance Lude.ToQuery GetSavingsPlansPurchaseRecommendation where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetSavingsPlansPurchaseRecommendationResponse' smart constructor.
data GetSavingsPlansPurchaseRecommendationResponse = GetSavingsPlansPurchaseRecommendationResponse'
  { -- | The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | Contains your request parameters, Savings Plan Recommendations Summary, and Details.
    savingsPlansPurchaseRecommendation :: Lude.Maybe SavingsPlansPurchaseRecommendation,
    -- | Information regarding this specific recommendation set.
    metadata :: Lude.Maybe SavingsPlansPurchaseRecommendationMetadata,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSavingsPlansPurchaseRecommendationResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
-- * 'savingsPlansPurchaseRecommendation' - Contains your request parameters, Savings Plan Recommendations Summary, and Details.
-- * 'metadata' - Information regarding this specific recommendation set.
-- * 'responseStatus' - The response status code.
mkGetSavingsPlansPurchaseRecommendationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSavingsPlansPurchaseRecommendationResponse
mkGetSavingsPlansPurchaseRecommendationResponse pResponseStatus_ =
  GetSavingsPlansPurchaseRecommendationResponse'
    { nextPageToken =
        Lude.Nothing,
      savingsPlansPurchaseRecommendation =
        Lude.Nothing,
      metadata = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspprrsNextPageToken :: Lens.Lens' GetSavingsPlansPurchaseRecommendationResponse (Lude.Maybe Lude.Text)
gspprrsNextPageToken = Lens.lens (nextPageToken :: GetSavingsPlansPurchaseRecommendationResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetSavingsPlansPurchaseRecommendationResponse)
{-# DEPRECATED gspprrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | Contains your request parameters, Savings Plan Recommendations Summary, and Details.
--
-- /Note:/ Consider using 'savingsPlansPurchaseRecommendation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspprrsSavingsPlansPurchaseRecommendation :: Lens.Lens' GetSavingsPlansPurchaseRecommendationResponse (Lude.Maybe SavingsPlansPurchaseRecommendation)
gspprrsSavingsPlansPurchaseRecommendation = Lens.lens (savingsPlansPurchaseRecommendation :: GetSavingsPlansPurchaseRecommendationResponse -> Lude.Maybe SavingsPlansPurchaseRecommendation) (\s a -> s {savingsPlansPurchaseRecommendation = a} :: GetSavingsPlansPurchaseRecommendationResponse)
{-# DEPRECATED gspprrsSavingsPlansPurchaseRecommendation "Use generic-lens or generic-optics with 'savingsPlansPurchaseRecommendation' instead." #-}

-- | Information regarding this specific recommendation set.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspprrsMetadata :: Lens.Lens' GetSavingsPlansPurchaseRecommendationResponse (Lude.Maybe SavingsPlansPurchaseRecommendationMetadata)
gspprrsMetadata = Lens.lens (metadata :: GetSavingsPlansPurchaseRecommendationResponse -> Lude.Maybe SavingsPlansPurchaseRecommendationMetadata) (\s a -> s {metadata = a} :: GetSavingsPlansPurchaseRecommendationResponse)
{-# DEPRECATED gspprrsMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspprrsResponseStatus :: Lens.Lens' GetSavingsPlansPurchaseRecommendationResponse Lude.Int
gspprrsResponseStatus = Lens.lens (responseStatus :: GetSavingsPlansPurchaseRecommendationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSavingsPlansPurchaseRecommendationResponse)
{-# DEPRECATED gspprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
