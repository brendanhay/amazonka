{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
  ( -- * Creating a Request
    getSavingsPlansPurchaseRecommendation,
    GetSavingsPlansPurchaseRecommendation,

    -- * Request Lenses
    gspprNextPageToken,
    gspprAccountScope,
    gspprFilter,
    gspprPageSize,
    gspprSavingsPlansType,
    gspprTermInYears,
    gspprPaymentOption,
    gspprLookbackPeriodInDays,

    -- * Destructuring the Response
    getSavingsPlansPurchaseRecommendationResponse,
    GetSavingsPlansPurchaseRecommendationResponse,

    -- * Response Lenses
    gspprrsNextPageToken,
    gspprrsSavingsPlansPurchaseRecommendation,
    gspprrsMetadata,
    gspprrsResponseStatus,
  )
where

import Network.AWS.CostExplorer.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getSavingsPlansPurchaseRecommendation' smart constructor.
data GetSavingsPlansPurchaseRecommendation = GetSavingsPlansPurchaseRecommendation'
  { _gspprNextPageToken ::
      !(Maybe Text),
    _gspprAccountScope ::
      !( Maybe
           AccountScope
       ),
    _gspprFilter ::
      !( Maybe
           Expression
       ),
    _gspprPageSize ::
      !(Maybe Nat),
    _gspprSavingsPlansType ::
      !SupportedSavingsPlansType,
    _gspprTermInYears ::
      !TermInYears,
    _gspprPaymentOption ::
      !PaymentOption,
    _gspprLookbackPeriodInDays ::
      !LookbackPeriodInDays
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSavingsPlansPurchaseRecommendation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gspprNextPageToken' - The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
--
-- * 'gspprAccountScope' - The account scope that you want your recommendations for. Amazon Web Services calculates recommendations including the management account and member accounts if the value is set to @PAYER@ . If the value is @LINKED@ , recommendations are calculated for individual member accounts only.
--
-- * 'gspprFilter' - You can filter your recommendations by Account ID with the @LINKED_ACCOUNT@ dimension. To filter your recommendations by Account ID, specify @Key@ as @LINKED_ACCOUNT@ and @Value@ as the comma-separated Acount ID(s) for which you want to see Savings Plans purchase recommendations. For GetSavingsPlansPurchaseRecommendation, the @Filter@ does not include @CostCategories@ or @Tags@ . It only includes @Dimensions@ . With @Dimensions@ , @Key@ must be @LINKED_ACCOUNT@ and @Value@ can be a single Account ID or multiple comma-separated Account IDs for which you want to see Savings Plans Purchase Recommendations. @AND@ and @OR@ operators are not supported.
--
-- * 'gspprPageSize' - The number of recommendations that you want returned in a single response object.
--
-- * 'gspprSavingsPlansType' - The Savings Plans recommendation type requested.
--
-- * 'gspprTermInYears' - The savings plan recommendation term used to generate these recommendations.
--
-- * 'gspprPaymentOption' - The payment option used to generate these recommendations.
--
-- * 'gspprLookbackPeriodInDays' - The lookback period used to generate the recommendation.
getSavingsPlansPurchaseRecommendation ::
  -- | 'gspprSavingsPlansType'
  SupportedSavingsPlansType ->
  -- | 'gspprTermInYears'
  TermInYears ->
  -- | 'gspprPaymentOption'
  PaymentOption ->
  -- | 'gspprLookbackPeriodInDays'
  LookbackPeriodInDays ->
  GetSavingsPlansPurchaseRecommendation
getSavingsPlansPurchaseRecommendation
  pSavingsPlansType_
  pTermInYears_
  pPaymentOption_
  pLookbackPeriodInDays_ =
    GetSavingsPlansPurchaseRecommendation'
      { _gspprNextPageToken =
          Nothing,
        _gspprAccountScope = Nothing,
        _gspprFilter = Nothing,
        _gspprPageSize = Nothing,
        _gspprSavingsPlansType = pSavingsPlansType_,
        _gspprTermInYears = pTermInYears_,
        _gspprPaymentOption = pPaymentOption_,
        _gspprLookbackPeriodInDays = pLookbackPeriodInDays_
      }

-- | The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
gspprNextPageToken :: Lens' GetSavingsPlansPurchaseRecommendation (Maybe Text)
gspprNextPageToken = lens _gspprNextPageToken (\s a -> s {_gspprNextPageToken = a})

-- | The account scope that you want your recommendations for. Amazon Web Services calculates recommendations including the management account and member accounts if the value is set to @PAYER@ . If the value is @LINKED@ , recommendations are calculated for individual member accounts only.
gspprAccountScope :: Lens' GetSavingsPlansPurchaseRecommendation (Maybe AccountScope)
gspprAccountScope = lens _gspprAccountScope (\s a -> s {_gspprAccountScope = a})

-- | You can filter your recommendations by Account ID with the @LINKED_ACCOUNT@ dimension. To filter your recommendations by Account ID, specify @Key@ as @LINKED_ACCOUNT@ and @Value@ as the comma-separated Acount ID(s) for which you want to see Savings Plans purchase recommendations. For GetSavingsPlansPurchaseRecommendation, the @Filter@ does not include @CostCategories@ or @Tags@ . It only includes @Dimensions@ . With @Dimensions@ , @Key@ must be @LINKED_ACCOUNT@ and @Value@ can be a single Account ID or multiple comma-separated Account IDs for which you want to see Savings Plans Purchase Recommendations. @AND@ and @OR@ operators are not supported.
gspprFilter :: Lens' GetSavingsPlansPurchaseRecommendation (Maybe Expression)
gspprFilter = lens _gspprFilter (\s a -> s {_gspprFilter = a})

-- | The number of recommendations that you want returned in a single response object.
gspprPageSize :: Lens' GetSavingsPlansPurchaseRecommendation (Maybe Natural)
gspprPageSize = lens _gspprPageSize (\s a -> s {_gspprPageSize = a}) . mapping _Nat

-- | The Savings Plans recommendation type requested.
gspprSavingsPlansType :: Lens' GetSavingsPlansPurchaseRecommendation SupportedSavingsPlansType
gspprSavingsPlansType = lens _gspprSavingsPlansType (\s a -> s {_gspprSavingsPlansType = a})

-- | The savings plan recommendation term used to generate these recommendations.
gspprTermInYears :: Lens' GetSavingsPlansPurchaseRecommendation TermInYears
gspprTermInYears = lens _gspprTermInYears (\s a -> s {_gspprTermInYears = a})

-- | The payment option used to generate these recommendations.
gspprPaymentOption :: Lens' GetSavingsPlansPurchaseRecommendation PaymentOption
gspprPaymentOption = lens _gspprPaymentOption (\s a -> s {_gspprPaymentOption = a})

-- | The lookback period used to generate the recommendation.
gspprLookbackPeriodInDays :: Lens' GetSavingsPlansPurchaseRecommendation LookbackPeriodInDays
gspprLookbackPeriodInDays = lens _gspprLookbackPeriodInDays (\s a -> s {_gspprLookbackPeriodInDays = a})

instance AWSRequest GetSavingsPlansPurchaseRecommendation where
  type
    Rs GetSavingsPlansPurchaseRecommendation =
      GetSavingsPlansPurchaseRecommendationResponse
  request = postJSON costExplorer
  response =
    receiveJSON
      ( \s h x ->
          GetSavingsPlansPurchaseRecommendationResponse'
            <$> (x .?> "NextPageToken")
            <*> (x .?> "SavingsPlansPurchaseRecommendation")
            <*> (x .?> "Metadata")
            <*> (pure (fromEnum s))
      )

instance Hashable GetSavingsPlansPurchaseRecommendation

instance NFData GetSavingsPlansPurchaseRecommendation

instance ToHeaders GetSavingsPlansPurchaseRecommendation where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSInsightsIndexService.GetSavingsPlansPurchaseRecommendation" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetSavingsPlansPurchaseRecommendation where
  toJSON GetSavingsPlansPurchaseRecommendation' {..} =
    object
      ( catMaybes
          [ ("NextPageToken" .=) <$> _gspprNextPageToken,
            ("AccountScope" .=) <$> _gspprAccountScope,
            ("Filter" .=) <$> _gspprFilter,
            ("PageSize" .=) <$> _gspprPageSize,
            Just ("SavingsPlansType" .= _gspprSavingsPlansType),
            Just ("TermInYears" .= _gspprTermInYears),
            Just ("PaymentOption" .= _gspprPaymentOption),
            Just ("LookbackPeriodInDays" .= _gspprLookbackPeriodInDays)
          ]
      )

instance ToPath GetSavingsPlansPurchaseRecommendation where
  toPath = const "/"

instance ToQuery GetSavingsPlansPurchaseRecommendation where
  toQuery = const mempty

-- | /See:/ 'getSavingsPlansPurchaseRecommendationResponse' smart constructor.
data GetSavingsPlansPurchaseRecommendationResponse = GetSavingsPlansPurchaseRecommendationResponse'
  { _gspprrsNextPageToken ::
      !( Maybe
           Text
       ),
    _gspprrsSavingsPlansPurchaseRecommendation ::
      !( Maybe
           SavingsPlansPurchaseRecommendation
       ),
    _gspprrsMetadata ::
      !( Maybe
           SavingsPlansPurchaseRecommendationMetadata
       ),
    _gspprrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'GetSavingsPlansPurchaseRecommendationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gspprrsNextPageToken' - The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- * 'gspprrsSavingsPlansPurchaseRecommendation' - Contains your request parameters, Savings Plan Recommendations Summary, and Details.
--
-- * 'gspprrsMetadata' - Information regarding this specific recommendation set.
--
-- * 'gspprrsResponseStatus' - -- | The response status code.
getSavingsPlansPurchaseRecommendationResponse ::
  -- | 'gspprrsResponseStatus'
  Int ->
  GetSavingsPlansPurchaseRecommendationResponse
getSavingsPlansPurchaseRecommendationResponse pResponseStatus_ =
  GetSavingsPlansPurchaseRecommendationResponse'
    { _gspprrsNextPageToken =
        Nothing,
      _gspprrsSavingsPlansPurchaseRecommendation =
        Nothing,
      _gspprrsMetadata = Nothing,
      _gspprrsResponseStatus = pResponseStatus_
    }

-- | The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
gspprrsNextPageToken :: Lens' GetSavingsPlansPurchaseRecommendationResponse (Maybe Text)
gspprrsNextPageToken = lens _gspprrsNextPageToken (\s a -> s {_gspprrsNextPageToken = a})

-- | Contains your request parameters, Savings Plan Recommendations Summary, and Details.
gspprrsSavingsPlansPurchaseRecommendation :: Lens' GetSavingsPlansPurchaseRecommendationResponse (Maybe SavingsPlansPurchaseRecommendation)
gspprrsSavingsPlansPurchaseRecommendation = lens _gspprrsSavingsPlansPurchaseRecommendation (\s a -> s {_gspprrsSavingsPlansPurchaseRecommendation = a})

-- | Information regarding this specific recommendation set.
gspprrsMetadata :: Lens' GetSavingsPlansPurchaseRecommendationResponse (Maybe SavingsPlansPurchaseRecommendationMetadata)
gspprrsMetadata = lens _gspprrsMetadata (\s a -> s {_gspprrsMetadata = a})

-- | -- | The response status code.
gspprrsResponseStatus :: Lens' GetSavingsPlansPurchaseRecommendationResponse Int
gspprrsResponseStatus = lens _gspprrsResponseStatus (\s a -> s {_gspprrsResponseStatus = a})

instance NFData GetSavingsPlansPurchaseRecommendationResponse
