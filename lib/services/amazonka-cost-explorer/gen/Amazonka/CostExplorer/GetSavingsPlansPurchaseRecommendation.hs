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
-- Module      : Amazonka.CostExplorer.GetSavingsPlansPurchaseRecommendation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the Savings Plans recommendations for your account. First use
-- @StartSavingsPlansPurchaseRecommendationGeneration@ to generate a new
-- set of recommendations, and then use
-- @GetSavingsPlansPurchaseRecommendation@ to retrieve them.
module Amazonka.CostExplorer.GetSavingsPlansPurchaseRecommendation
  ( -- * Creating a Request
    GetSavingsPlansPurchaseRecommendation (..),
    newGetSavingsPlansPurchaseRecommendation,

    -- * Request Lenses
    getSavingsPlansPurchaseRecommendation_accountScope,
    getSavingsPlansPurchaseRecommendation_filter,
    getSavingsPlansPurchaseRecommendation_nextPageToken,
    getSavingsPlansPurchaseRecommendation_pageSize,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSavingsPlansPurchaseRecommendation' smart constructor.
data GetSavingsPlansPurchaseRecommendation = GetSavingsPlansPurchaseRecommendation'
  { -- | The account scope that you want your recommendations for. Amazon Web
    -- Services calculates recommendations including the management account and
    -- member accounts if the value is set to @PAYER@. If the value is
    -- @LINKED@, recommendations are calculated for individual member accounts
    -- only.
    accountScope :: Prelude.Maybe AccountScope,
    -- | You can filter your recommendations by Account ID with the
    -- @LINKED_ACCOUNT@ dimension. To filter your recommendations by Account
    -- ID, specify @Key@ as @LINKED_ACCOUNT@ and @Value@ as the comma-separated
    -- Acount ID(s) that you want to see Savings Plans purchase recommendations
    -- for.
    --
    -- For GetSavingsPlansPurchaseRecommendation, the @Filter@ doesn\'t include
    -- @CostCategories@ or @Tags@. It only includes @Dimensions@. With
    -- @Dimensions@, @Key@ must be @LINKED_ACCOUNT@ and @Value@ can be a single
    -- Account ID or multiple comma-separated Account IDs that you want to see
    -- Savings Plans Purchase Recommendations for. @AND@ and @OR@ operators are
    -- not supported.
    filter' :: Prelude.Maybe Expression,
    -- | The token to retrieve the next set of results. Amazon Web Services
    -- provides the token when the response from a previous call has more
    -- results than the maximum page size.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The number of recommendations that you want returned in a single
    -- response object.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The Savings Plans recommendation type that\'s requested.
    savingsPlansType :: SupportedSavingsPlansType,
    -- | The savings plan recommendation term that\'s used to generate these
    -- recommendations.
    termInYears :: TermInYears,
    -- | The payment option that\'s used to generate these recommendations.
    paymentOption :: PaymentOption,
    -- | The lookback period that\'s used to generate the recommendation.
    lookbackPeriodInDays :: LookbackPeriodInDays
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSavingsPlansPurchaseRecommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountScope', 'getSavingsPlansPurchaseRecommendation_accountScope' - The account scope that you want your recommendations for. Amazon Web
-- Services calculates recommendations including the management account and
-- member accounts if the value is set to @PAYER@. If the value is
-- @LINKED@, recommendations are calculated for individual member accounts
-- only.
--
-- 'filter'', 'getSavingsPlansPurchaseRecommendation_filter' - You can filter your recommendations by Account ID with the
-- @LINKED_ACCOUNT@ dimension. To filter your recommendations by Account
-- ID, specify @Key@ as @LINKED_ACCOUNT@ and @Value@ as the comma-separated
-- Acount ID(s) that you want to see Savings Plans purchase recommendations
-- for.
--
-- For GetSavingsPlansPurchaseRecommendation, the @Filter@ doesn\'t include
-- @CostCategories@ or @Tags@. It only includes @Dimensions@. With
-- @Dimensions@, @Key@ must be @LINKED_ACCOUNT@ and @Value@ can be a single
-- Account ID or multiple comma-separated Account IDs that you want to see
-- Savings Plans Purchase Recommendations for. @AND@ and @OR@ operators are
-- not supported.
--
-- 'nextPageToken', 'getSavingsPlansPurchaseRecommendation_nextPageToken' - The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
--
-- 'pageSize', 'getSavingsPlansPurchaseRecommendation_pageSize' - The number of recommendations that you want returned in a single
-- response object.
--
-- 'savingsPlansType', 'getSavingsPlansPurchaseRecommendation_savingsPlansType' - The Savings Plans recommendation type that\'s requested.
--
-- 'termInYears', 'getSavingsPlansPurchaseRecommendation_termInYears' - The savings plan recommendation term that\'s used to generate these
-- recommendations.
--
-- 'paymentOption', 'getSavingsPlansPurchaseRecommendation_paymentOption' - The payment option that\'s used to generate these recommendations.
--
-- 'lookbackPeriodInDays', 'getSavingsPlansPurchaseRecommendation_lookbackPeriodInDays' - The lookback period that\'s used to generate the recommendation.
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
      { accountScope =
          Prelude.Nothing,
        filter' = Prelude.Nothing,
        nextPageToken = Prelude.Nothing,
        pageSize = Prelude.Nothing,
        savingsPlansType =
          pSavingsPlansType_,
        termInYears = pTermInYears_,
        paymentOption = pPaymentOption_,
        lookbackPeriodInDays =
          pLookbackPeriodInDays_
      }

-- | The account scope that you want your recommendations for. Amazon Web
-- Services calculates recommendations including the management account and
-- member accounts if the value is set to @PAYER@. If the value is
-- @LINKED@, recommendations are calculated for individual member accounts
-- only.
getSavingsPlansPurchaseRecommendation_accountScope :: Lens.Lens' GetSavingsPlansPurchaseRecommendation (Prelude.Maybe AccountScope)
getSavingsPlansPurchaseRecommendation_accountScope = Lens.lens (\GetSavingsPlansPurchaseRecommendation' {accountScope} -> accountScope) (\s@GetSavingsPlansPurchaseRecommendation' {} a -> s {accountScope = a} :: GetSavingsPlansPurchaseRecommendation)

-- | You can filter your recommendations by Account ID with the
-- @LINKED_ACCOUNT@ dimension. To filter your recommendations by Account
-- ID, specify @Key@ as @LINKED_ACCOUNT@ and @Value@ as the comma-separated
-- Acount ID(s) that you want to see Savings Plans purchase recommendations
-- for.
--
-- For GetSavingsPlansPurchaseRecommendation, the @Filter@ doesn\'t include
-- @CostCategories@ or @Tags@. It only includes @Dimensions@. With
-- @Dimensions@, @Key@ must be @LINKED_ACCOUNT@ and @Value@ can be a single
-- Account ID or multiple comma-separated Account IDs that you want to see
-- Savings Plans Purchase Recommendations for. @AND@ and @OR@ operators are
-- not supported.
getSavingsPlansPurchaseRecommendation_filter :: Lens.Lens' GetSavingsPlansPurchaseRecommendation (Prelude.Maybe Expression)
getSavingsPlansPurchaseRecommendation_filter = Lens.lens (\GetSavingsPlansPurchaseRecommendation' {filter'} -> filter') (\s@GetSavingsPlansPurchaseRecommendation' {} a -> s {filter' = a} :: GetSavingsPlansPurchaseRecommendation)

-- | The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
getSavingsPlansPurchaseRecommendation_nextPageToken :: Lens.Lens' GetSavingsPlansPurchaseRecommendation (Prelude.Maybe Prelude.Text)
getSavingsPlansPurchaseRecommendation_nextPageToken = Lens.lens (\GetSavingsPlansPurchaseRecommendation' {nextPageToken} -> nextPageToken) (\s@GetSavingsPlansPurchaseRecommendation' {} a -> s {nextPageToken = a} :: GetSavingsPlansPurchaseRecommendation)

-- | The number of recommendations that you want returned in a single
-- response object.
getSavingsPlansPurchaseRecommendation_pageSize :: Lens.Lens' GetSavingsPlansPurchaseRecommendation (Prelude.Maybe Prelude.Natural)
getSavingsPlansPurchaseRecommendation_pageSize = Lens.lens (\GetSavingsPlansPurchaseRecommendation' {pageSize} -> pageSize) (\s@GetSavingsPlansPurchaseRecommendation' {} a -> s {pageSize = a} :: GetSavingsPlansPurchaseRecommendation)

-- | The Savings Plans recommendation type that\'s requested.
getSavingsPlansPurchaseRecommendation_savingsPlansType :: Lens.Lens' GetSavingsPlansPurchaseRecommendation SupportedSavingsPlansType
getSavingsPlansPurchaseRecommendation_savingsPlansType = Lens.lens (\GetSavingsPlansPurchaseRecommendation' {savingsPlansType} -> savingsPlansType) (\s@GetSavingsPlansPurchaseRecommendation' {} a -> s {savingsPlansType = a} :: GetSavingsPlansPurchaseRecommendation)

-- | The savings plan recommendation term that\'s used to generate these
-- recommendations.
getSavingsPlansPurchaseRecommendation_termInYears :: Lens.Lens' GetSavingsPlansPurchaseRecommendation TermInYears
getSavingsPlansPurchaseRecommendation_termInYears = Lens.lens (\GetSavingsPlansPurchaseRecommendation' {termInYears} -> termInYears) (\s@GetSavingsPlansPurchaseRecommendation' {} a -> s {termInYears = a} :: GetSavingsPlansPurchaseRecommendation)

-- | The payment option that\'s used to generate these recommendations.
getSavingsPlansPurchaseRecommendation_paymentOption :: Lens.Lens' GetSavingsPlansPurchaseRecommendation PaymentOption
getSavingsPlansPurchaseRecommendation_paymentOption = Lens.lens (\GetSavingsPlansPurchaseRecommendation' {paymentOption} -> paymentOption) (\s@GetSavingsPlansPurchaseRecommendation' {} a -> s {paymentOption = a} :: GetSavingsPlansPurchaseRecommendation)

-- | The lookback period that\'s used to generate the recommendation.
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSavingsPlansPurchaseRecommendationResponse'
            Prelude.<$> (x Data..?> "Metadata")
            Prelude.<*> (x Data..?> "NextPageToken")
            Prelude.<*> (x Data..?> "SavingsPlansPurchaseRecommendation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetSavingsPlansPurchaseRecommendation
  where
  hashWithSalt
    _salt
    GetSavingsPlansPurchaseRecommendation' {..} =
      _salt
        `Prelude.hashWithSalt` accountScope
        `Prelude.hashWithSalt` filter'
        `Prelude.hashWithSalt` nextPageToken
        `Prelude.hashWithSalt` pageSize
        `Prelude.hashWithSalt` savingsPlansType
        `Prelude.hashWithSalt` termInYears
        `Prelude.hashWithSalt` paymentOption
        `Prelude.hashWithSalt` lookbackPeriodInDays

instance
  Prelude.NFData
    GetSavingsPlansPurchaseRecommendation
  where
  rnf GetSavingsPlansPurchaseRecommendation' {..} =
    Prelude.rnf accountScope `Prelude.seq`
      Prelude.rnf filter' `Prelude.seq`
        Prelude.rnf nextPageToken `Prelude.seq`
          Prelude.rnf pageSize `Prelude.seq`
            Prelude.rnf savingsPlansType `Prelude.seq`
              Prelude.rnf termInYears `Prelude.seq`
                Prelude.rnf paymentOption `Prelude.seq`
                  Prelude.rnf lookbackPeriodInDays

instance
  Data.ToHeaders
    GetSavingsPlansPurchaseRecommendation
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSInsightsIndexService.GetSavingsPlansPurchaseRecommendation" ::
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
    GetSavingsPlansPurchaseRecommendation
  where
  toJSON GetSavingsPlansPurchaseRecommendation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccountScope" Data..=) Prelude.<$> accountScope,
            ("Filter" Data..=) Prelude.<$> filter',
            ("NextPageToken" Data..=) Prelude.<$> nextPageToken,
            ("PageSize" Data..=) Prelude.<$> pageSize,
            Prelude.Just
              ("SavingsPlansType" Data..= savingsPlansType),
            Prelude.Just ("TermInYears" Data..= termInYears),
            Prelude.Just ("PaymentOption" Data..= paymentOption),
            Prelude.Just
              ( "LookbackPeriodInDays"
                  Data..= lookbackPeriodInDays
              )
          ]
      )

instance
  Data.ToPath
    GetSavingsPlansPurchaseRecommendation
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetSavingsPlansPurchaseRecommendation
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSavingsPlansPurchaseRecommendationResponse' smart constructor.
data GetSavingsPlansPurchaseRecommendationResponse = GetSavingsPlansPurchaseRecommendationResponse'
  { -- | Information that regards this specific recommendation set.
    metadata :: Prelude.Maybe SavingsPlansPurchaseRecommendationMetadata,
    -- | The token for the next set of retrievable results. Amazon Web Services
    -- provides the token when the response from a previous call has more
    -- results than the maximum page size.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | Contains your request parameters, Savings Plan Recommendations Summary,
    -- and Details.
    savingsPlansPurchaseRecommendation :: Prelude.Maybe SavingsPlansPurchaseRecommendation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSavingsPlansPurchaseRecommendationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadata', 'getSavingsPlansPurchaseRecommendationResponse_metadata' - Information that regards this specific recommendation set.
--
-- 'nextPageToken', 'getSavingsPlansPurchaseRecommendationResponse_nextPageToken' - The token for the next set of retrievable results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
--
-- 'savingsPlansPurchaseRecommendation', 'getSavingsPlansPurchaseRecommendationResponse_savingsPlansPurchaseRecommendation' - Contains your request parameters, Savings Plan Recommendations Summary,
-- and Details.
--
-- 'httpStatus', 'getSavingsPlansPurchaseRecommendationResponse_httpStatus' - The response's http status code.
newGetSavingsPlansPurchaseRecommendationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSavingsPlansPurchaseRecommendationResponse
newGetSavingsPlansPurchaseRecommendationResponse
  pHttpStatus_ =
    GetSavingsPlansPurchaseRecommendationResponse'
      { metadata =
          Prelude.Nothing,
        nextPageToken =
          Prelude.Nothing,
        savingsPlansPurchaseRecommendation =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information that regards this specific recommendation set.
getSavingsPlansPurchaseRecommendationResponse_metadata :: Lens.Lens' GetSavingsPlansPurchaseRecommendationResponse (Prelude.Maybe SavingsPlansPurchaseRecommendationMetadata)
getSavingsPlansPurchaseRecommendationResponse_metadata = Lens.lens (\GetSavingsPlansPurchaseRecommendationResponse' {metadata} -> metadata) (\s@GetSavingsPlansPurchaseRecommendationResponse' {} a -> s {metadata = a} :: GetSavingsPlansPurchaseRecommendationResponse)

-- | The token for the next set of retrievable results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
getSavingsPlansPurchaseRecommendationResponse_nextPageToken :: Lens.Lens' GetSavingsPlansPurchaseRecommendationResponse (Prelude.Maybe Prelude.Text)
getSavingsPlansPurchaseRecommendationResponse_nextPageToken = Lens.lens (\GetSavingsPlansPurchaseRecommendationResponse' {nextPageToken} -> nextPageToken) (\s@GetSavingsPlansPurchaseRecommendationResponse' {} a -> s {nextPageToken = a} :: GetSavingsPlansPurchaseRecommendationResponse)

-- | Contains your request parameters, Savings Plan Recommendations Summary,
-- and Details.
getSavingsPlansPurchaseRecommendationResponse_savingsPlansPurchaseRecommendation :: Lens.Lens' GetSavingsPlansPurchaseRecommendationResponse (Prelude.Maybe SavingsPlansPurchaseRecommendation)
getSavingsPlansPurchaseRecommendationResponse_savingsPlansPurchaseRecommendation = Lens.lens (\GetSavingsPlansPurchaseRecommendationResponse' {savingsPlansPurchaseRecommendation} -> savingsPlansPurchaseRecommendation) (\s@GetSavingsPlansPurchaseRecommendationResponse' {} a -> s {savingsPlansPurchaseRecommendation = a} :: GetSavingsPlansPurchaseRecommendationResponse)

-- | The response's http status code.
getSavingsPlansPurchaseRecommendationResponse_httpStatus :: Lens.Lens' GetSavingsPlansPurchaseRecommendationResponse Prelude.Int
getSavingsPlansPurchaseRecommendationResponse_httpStatus = Lens.lens (\GetSavingsPlansPurchaseRecommendationResponse' {httpStatus} -> httpStatus) (\s@GetSavingsPlansPurchaseRecommendationResponse' {} a -> s {httpStatus = a} :: GetSavingsPlansPurchaseRecommendationResponse)

instance
  Prelude.NFData
    GetSavingsPlansPurchaseRecommendationResponse
  where
  rnf
    GetSavingsPlansPurchaseRecommendationResponse' {..} =
      Prelude.rnf metadata `Prelude.seq`
        Prelude.rnf nextPageToken `Prelude.seq`
          Prelude.rnf savingsPlansPurchaseRecommendation `Prelude.seq`
            Prelude.rnf httpStatus
