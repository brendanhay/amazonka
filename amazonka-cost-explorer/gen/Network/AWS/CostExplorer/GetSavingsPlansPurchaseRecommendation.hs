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
-- Module      : Network.AWS.CostExplorer.GetSavingsPlansPurchaseRecommendation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves your request parameters, Savings Plan Recommendations Summary
-- and Details.
module Network.AWS.CostExplorer.GetSavingsPlansPurchaseRecommendation
  ( -- * Creating a Request
    GetSavingsPlansPurchaseRecommendation (..),
    newGetSavingsPlansPurchaseRecommendation,

    -- * Request Lenses
    getSavingsPlansPurchaseRecommendation_pageSize,
    getSavingsPlansPurchaseRecommendation_accountScope,
    getSavingsPlansPurchaseRecommendation_nextPageToken,
    getSavingsPlansPurchaseRecommendation_filter,
    getSavingsPlansPurchaseRecommendation_savingsPlansType,
    getSavingsPlansPurchaseRecommendation_termInYears,
    getSavingsPlansPurchaseRecommendation_paymentOption,
    getSavingsPlansPurchaseRecommendation_lookbackPeriodInDays,

    -- * Destructuring the Response
    GetSavingsPlansPurchaseRecommendationResponse (..),
    newGetSavingsPlansPurchaseRecommendationResponse,

    -- * Response Lenses
    getSavingsPlansPurchaseRecommendationResponse_metadata,
    getSavingsPlansPurchaseRecommendationResponse_nextPageToken,
    getSavingsPlansPurchaseRecommendationResponse_savingsPlansPurchaseRecommendation,
    getSavingsPlansPurchaseRecommendationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetSavingsPlansPurchaseRecommendation' smart constructor.
data GetSavingsPlansPurchaseRecommendation = GetSavingsPlansPurchaseRecommendation'
  { -- | The number of recommendations that you want returned in a single
    -- response object.
    pageSize :: Core.Maybe Core.Natural,
    -- | The account scope that you want your recommendations for. Amazon Web
    -- Services calculates recommendations including the management account and
    -- member accounts if the value is set to @PAYER@. If the value is
    -- @LINKED@, recommendations are calculated for individual member accounts
    -- only.
    accountScope :: Core.Maybe AccountScope,
    -- | The token to retrieve the next set of results. Amazon Web Services
    -- provides the token when the response from a previous call has more
    -- results than the maximum page size.
    nextPageToken :: Core.Maybe Core.Text,
    -- | You can filter your recommendations by Account ID with the
    -- @LINKED_ACCOUNT@ dimension. To filter your recommendations by Account
    -- ID, specify @Key@ as @LINKED_ACCOUNT@ and @Value@ as the comma-separated
    -- Acount ID(s) for which you want to see Savings Plans purchase
    -- recommendations.
    --
    -- For GetSavingsPlansPurchaseRecommendation, the @Filter@ does not include
    -- @CostCategories@ or @Tags@. It only includes @Dimensions@. With
    -- @Dimensions@, @Key@ must be @LINKED_ACCOUNT@ and @Value@ can be a single
    -- Account ID or multiple comma-separated Account IDs for which you want to
    -- see Savings Plans Purchase Recommendations. @AND@ and @OR@ operators are
    -- not supported.
    filter' :: Core.Maybe Expression,
    -- | The Savings Plans recommendation type requested.
    savingsPlansType :: SupportedSavingsPlansType,
    -- | The savings plan recommendation term used to generate these
    -- recommendations.
    termInYears :: TermInYears,
    -- | The payment option used to generate these recommendations.
    paymentOption :: PaymentOption,
    -- | The lookback period used to generate the recommendation.
    lookbackPeriodInDays :: LookbackPeriodInDays
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSavingsPlansPurchaseRecommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'getSavingsPlansPurchaseRecommendation_pageSize' - The number of recommendations that you want returned in a single
-- response object.
--
-- 'accountScope', 'getSavingsPlansPurchaseRecommendation_accountScope' - The account scope that you want your recommendations for. Amazon Web
-- Services calculates recommendations including the management account and
-- member accounts if the value is set to @PAYER@. If the value is
-- @LINKED@, recommendations are calculated for individual member accounts
-- only.
--
-- 'nextPageToken', 'getSavingsPlansPurchaseRecommendation_nextPageToken' - The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
--
-- 'filter'', 'getSavingsPlansPurchaseRecommendation_filter' - You can filter your recommendations by Account ID with the
-- @LINKED_ACCOUNT@ dimension. To filter your recommendations by Account
-- ID, specify @Key@ as @LINKED_ACCOUNT@ and @Value@ as the comma-separated
-- Acount ID(s) for which you want to see Savings Plans purchase
-- recommendations.
--
-- For GetSavingsPlansPurchaseRecommendation, the @Filter@ does not include
-- @CostCategories@ or @Tags@. It only includes @Dimensions@. With
-- @Dimensions@, @Key@ must be @LINKED_ACCOUNT@ and @Value@ can be a single
-- Account ID or multiple comma-separated Account IDs for which you want to
-- see Savings Plans Purchase Recommendations. @AND@ and @OR@ operators are
-- not supported.
--
-- 'savingsPlansType', 'getSavingsPlansPurchaseRecommendation_savingsPlansType' - The Savings Plans recommendation type requested.
--
-- 'termInYears', 'getSavingsPlansPurchaseRecommendation_termInYears' - The savings plan recommendation term used to generate these
-- recommendations.
--
-- 'paymentOption', 'getSavingsPlansPurchaseRecommendation_paymentOption' - The payment option used to generate these recommendations.
--
-- 'lookbackPeriodInDays', 'getSavingsPlansPurchaseRecommendation_lookbackPeriodInDays' - The lookback period used to generate the recommendation.
newGetSavingsPlansPurchaseRecommendation ::
  -- | 'savingsPlansType'
  SupportedSavingsPlansType ->
  -- | 'termInYears'
  TermInYears ->
  -- | 'paymentOption'
  PaymentOption ->
  -- | 'lookbackPeriodInDays'
  LookbackPeriodInDays ->
  GetSavingsPlansPurchaseRecommendation
newGetSavingsPlansPurchaseRecommendation
  pSavingsPlansType_
  pTermInYears_
  pPaymentOption_
  pLookbackPeriodInDays_ =
    GetSavingsPlansPurchaseRecommendation'
      { pageSize =
          Core.Nothing,
        accountScope = Core.Nothing,
        nextPageToken = Core.Nothing,
        filter' = Core.Nothing,
        savingsPlansType =
          pSavingsPlansType_,
        termInYears = pTermInYears_,
        paymentOption = pPaymentOption_,
        lookbackPeriodInDays =
          pLookbackPeriodInDays_
      }

-- | The number of recommendations that you want returned in a single
-- response object.
getSavingsPlansPurchaseRecommendation_pageSize :: Lens.Lens' GetSavingsPlansPurchaseRecommendation (Core.Maybe Core.Natural)
getSavingsPlansPurchaseRecommendation_pageSize = Lens.lens (\GetSavingsPlansPurchaseRecommendation' {pageSize} -> pageSize) (\s@GetSavingsPlansPurchaseRecommendation' {} a -> s {pageSize = a} :: GetSavingsPlansPurchaseRecommendation)

-- | The account scope that you want your recommendations for. Amazon Web
-- Services calculates recommendations including the management account and
-- member accounts if the value is set to @PAYER@. If the value is
-- @LINKED@, recommendations are calculated for individual member accounts
-- only.
getSavingsPlansPurchaseRecommendation_accountScope :: Lens.Lens' GetSavingsPlansPurchaseRecommendation (Core.Maybe AccountScope)
getSavingsPlansPurchaseRecommendation_accountScope = Lens.lens (\GetSavingsPlansPurchaseRecommendation' {accountScope} -> accountScope) (\s@GetSavingsPlansPurchaseRecommendation' {} a -> s {accountScope = a} :: GetSavingsPlansPurchaseRecommendation)

-- | The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
getSavingsPlansPurchaseRecommendation_nextPageToken :: Lens.Lens' GetSavingsPlansPurchaseRecommendation (Core.Maybe Core.Text)
getSavingsPlansPurchaseRecommendation_nextPageToken = Lens.lens (\GetSavingsPlansPurchaseRecommendation' {nextPageToken} -> nextPageToken) (\s@GetSavingsPlansPurchaseRecommendation' {} a -> s {nextPageToken = a} :: GetSavingsPlansPurchaseRecommendation)

-- | You can filter your recommendations by Account ID with the
-- @LINKED_ACCOUNT@ dimension. To filter your recommendations by Account
-- ID, specify @Key@ as @LINKED_ACCOUNT@ and @Value@ as the comma-separated
-- Acount ID(s) for which you want to see Savings Plans purchase
-- recommendations.
--
-- For GetSavingsPlansPurchaseRecommendation, the @Filter@ does not include
-- @CostCategories@ or @Tags@. It only includes @Dimensions@. With
-- @Dimensions@, @Key@ must be @LINKED_ACCOUNT@ and @Value@ can be a single
-- Account ID or multiple comma-separated Account IDs for which you want to
-- see Savings Plans Purchase Recommendations. @AND@ and @OR@ operators are
-- not supported.
getSavingsPlansPurchaseRecommendation_filter :: Lens.Lens' GetSavingsPlansPurchaseRecommendation (Core.Maybe Expression)
getSavingsPlansPurchaseRecommendation_filter = Lens.lens (\GetSavingsPlansPurchaseRecommendation' {filter'} -> filter') (\s@GetSavingsPlansPurchaseRecommendation' {} a -> s {filter' = a} :: GetSavingsPlansPurchaseRecommendation)

-- | The Savings Plans recommendation type requested.
getSavingsPlansPurchaseRecommendation_savingsPlansType :: Lens.Lens' GetSavingsPlansPurchaseRecommendation SupportedSavingsPlansType
getSavingsPlansPurchaseRecommendation_savingsPlansType = Lens.lens (\GetSavingsPlansPurchaseRecommendation' {savingsPlansType} -> savingsPlansType) (\s@GetSavingsPlansPurchaseRecommendation' {} a -> s {savingsPlansType = a} :: GetSavingsPlansPurchaseRecommendation)

-- | The savings plan recommendation term used to generate these
-- recommendations.
getSavingsPlansPurchaseRecommendation_termInYears :: Lens.Lens' GetSavingsPlansPurchaseRecommendation TermInYears
getSavingsPlansPurchaseRecommendation_termInYears = Lens.lens (\GetSavingsPlansPurchaseRecommendation' {termInYears} -> termInYears) (\s@GetSavingsPlansPurchaseRecommendation' {} a -> s {termInYears = a} :: GetSavingsPlansPurchaseRecommendation)

-- | The payment option used to generate these recommendations.
getSavingsPlansPurchaseRecommendation_paymentOption :: Lens.Lens' GetSavingsPlansPurchaseRecommendation PaymentOption
getSavingsPlansPurchaseRecommendation_paymentOption = Lens.lens (\GetSavingsPlansPurchaseRecommendation' {paymentOption} -> paymentOption) (\s@GetSavingsPlansPurchaseRecommendation' {} a -> s {paymentOption = a} :: GetSavingsPlansPurchaseRecommendation)

-- | The lookback period used to generate the recommendation.
getSavingsPlansPurchaseRecommendation_lookbackPeriodInDays :: Lens.Lens' GetSavingsPlansPurchaseRecommendation LookbackPeriodInDays
getSavingsPlansPurchaseRecommendation_lookbackPeriodInDays = Lens.lens (\GetSavingsPlansPurchaseRecommendation' {lookbackPeriodInDays} -> lookbackPeriodInDays) (\s@GetSavingsPlansPurchaseRecommendation' {} a -> s {lookbackPeriodInDays = a} :: GetSavingsPlansPurchaseRecommendation)

instance
  Core.AWSRequest
    GetSavingsPlansPurchaseRecommendation
  where
  type
    AWSResponse
      GetSavingsPlansPurchaseRecommendation =
      GetSavingsPlansPurchaseRecommendationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSavingsPlansPurchaseRecommendationResponse'
            Core.<$> (x Core..?> "Metadata")
              Core.<*> (x Core..?> "NextPageToken")
              Core.<*> (x Core..?> "SavingsPlansPurchaseRecommendation")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    GetSavingsPlansPurchaseRecommendation

instance
  Core.NFData
    GetSavingsPlansPurchaseRecommendation

instance
  Core.ToHeaders
    GetSavingsPlansPurchaseRecommendation
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSInsightsIndexService.GetSavingsPlansPurchaseRecommendation" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    GetSavingsPlansPurchaseRecommendation
  where
  toJSON GetSavingsPlansPurchaseRecommendation' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PageSize" Core..=) Core.<$> pageSize,
            ("AccountScope" Core..=) Core.<$> accountScope,
            ("NextPageToken" Core..=) Core.<$> nextPageToken,
            ("Filter" Core..=) Core.<$> filter',
            Core.Just
              ("SavingsPlansType" Core..= savingsPlansType),
            Core.Just ("TermInYears" Core..= termInYears),
            Core.Just ("PaymentOption" Core..= paymentOption),
            Core.Just
              ( "LookbackPeriodInDays"
                  Core..= lookbackPeriodInDays
              )
          ]
      )

instance
  Core.ToPath
    GetSavingsPlansPurchaseRecommendation
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    GetSavingsPlansPurchaseRecommendation
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetSavingsPlansPurchaseRecommendationResponse' smart constructor.
data GetSavingsPlansPurchaseRecommendationResponse = GetSavingsPlansPurchaseRecommendationResponse'
  { -- | Information regarding this specific recommendation set.
    metadata :: Core.Maybe SavingsPlansPurchaseRecommendationMetadata,
    -- | The token for the next set of retrievable results. AWS provides the
    -- token when the response from a previous call has more results than the
    -- maximum page size.
    nextPageToken :: Core.Maybe Core.Text,
    -- | Contains your request parameters, Savings Plan Recommendations Summary,
    -- and Details.
    savingsPlansPurchaseRecommendation :: Core.Maybe SavingsPlansPurchaseRecommendation,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSavingsPlansPurchaseRecommendationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadata', 'getSavingsPlansPurchaseRecommendationResponse_metadata' - Information regarding this specific recommendation set.
--
-- 'nextPageToken', 'getSavingsPlansPurchaseRecommendationResponse_nextPageToken' - The token for the next set of retrievable results. AWS provides the
-- token when the response from a previous call has more results than the
-- maximum page size.
--
-- 'savingsPlansPurchaseRecommendation', 'getSavingsPlansPurchaseRecommendationResponse_savingsPlansPurchaseRecommendation' - Contains your request parameters, Savings Plan Recommendations Summary,
-- and Details.
--
-- 'httpStatus', 'getSavingsPlansPurchaseRecommendationResponse_httpStatus' - The response's http status code.
newGetSavingsPlansPurchaseRecommendationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetSavingsPlansPurchaseRecommendationResponse
newGetSavingsPlansPurchaseRecommendationResponse
  pHttpStatus_ =
    GetSavingsPlansPurchaseRecommendationResponse'
      { metadata =
          Core.Nothing,
        nextPageToken = Core.Nothing,
        savingsPlansPurchaseRecommendation =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information regarding this specific recommendation set.
getSavingsPlansPurchaseRecommendationResponse_metadata :: Lens.Lens' GetSavingsPlansPurchaseRecommendationResponse (Core.Maybe SavingsPlansPurchaseRecommendationMetadata)
getSavingsPlansPurchaseRecommendationResponse_metadata = Lens.lens (\GetSavingsPlansPurchaseRecommendationResponse' {metadata} -> metadata) (\s@GetSavingsPlansPurchaseRecommendationResponse' {} a -> s {metadata = a} :: GetSavingsPlansPurchaseRecommendationResponse)

-- | The token for the next set of retrievable results. AWS provides the
-- token when the response from a previous call has more results than the
-- maximum page size.
getSavingsPlansPurchaseRecommendationResponse_nextPageToken :: Lens.Lens' GetSavingsPlansPurchaseRecommendationResponse (Core.Maybe Core.Text)
getSavingsPlansPurchaseRecommendationResponse_nextPageToken = Lens.lens (\GetSavingsPlansPurchaseRecommendationResponse' {nextPageToken} -> nextPageToken) (\s@GetSavingsPlansPurchaseRecommendationResponse' {} a -> s {nextPageToken = a} :: GetSavingsPlansPurchaseRecommendationResponse)

-- | Contains your request parameters, Savings Plan Recommendations Summary,
-- and Details.
getSavingsPlansPurchaseRecommendationResponse_savingsPlansPurchaseRecommendation :: Lens.Lens' GetSavingsPlansPurchaseRecommendationResponse (Core.Maybe SavingsPlansPurchaseRecommendation)
getSavingsPlansPurchaseRecommendationResponse_savingsPlansPurchaseRecommendation = Lens.lens (\GetSavingsPlansPurchaseRecommendationResponse' {savingsPlansPurchaseRecommendation} -> savingsPlansPurchaseRecommendation) (\s@GetSavingsPlansPurchaseRecommendationResponse' {} a -> s {savingsPlansPurchaseRecommendation = a} :: GetSavingsPlansPurchaseRecommendationResponse)

-- | The response's http status code.
getSavingsPlansPurchaseRecommendationResponse_httpStatus :: Lens.Lens' GetSavingsPlansPurchaseRecommendationResponse Core.Int
getSavingsPlansPurchaseRecommendationResponse_httpStatus = Lens.lens (\GetSavingsPlansPurchaseRecommendationResponse' {httpStatus} -> httpStatus) (\s@GetSavingsPlansPurchaseRecommendationResponse' {} a -> s {httpStatus = a} :: GetSavingsPlansPurchaseRecommendationResponse)

instance
  Core.NFData
    GetSavingsPlansPurchaseRecommendationResponse
