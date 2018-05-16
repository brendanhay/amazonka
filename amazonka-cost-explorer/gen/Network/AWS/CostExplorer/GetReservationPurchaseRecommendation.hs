{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetReservationPurchaseRecommendation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets recommendations for which reservations to purchase. These recommendations could help you reduce your costs. Reservations provide a discounted hourly rate (up to 75%) compared to On-Demand pricing.
--
--
-- AWS generates your recommendations by identifying your On-Demand usage during a specific time period and collecting your usage into categories that are eligible for a reservation. After AWS has these categories, it simulates every combination of reservations in each category of usage to identify the best number of each type of RI to purchase to maximize your estimated savings.
--
-- For example, AWS automatically aggregates your EC2 Linux, shared tenancy, and c4 family usage in the US West (Oregon) Region and recommends that you buy size-flexible regional reservations to apply to the c4 family usage. AWS recommends the smallest size instance in an instance family. This makes it easier to purchase a size-flexible RI. AWS also shows the equal number of normalized units so that you can purchase any instance size that you want. For this example, your RI recommendation would be for @c4.large@ , because that is the smallest size instance in the c4 instance family.
--
module Network.AWS.CostExplorer.GetReservationPurchaseRecommendation
    (
    -- * Creating a Request
      getReservationPurchaseRecommendation
    , GetReservationPurchaseRecommendation
    -- * Request Lenses
    , grprNextPageToken
    , grprTermInYears
    , grprServiceSpecification
    , grprAccountScope
    , grprAccountId
    , grprPageSize
    , grprLookbackPeriodInDays
    , grprPaymentOption
    , grprService

    -- * Destructuring the Response
    , getReservationPurchaseRecommendationResponse
    , GetReservationPurchaseRecommendationResponse
    -- * Response Lenses
    , grprrsNextPageToken
    , grprrsRecommendations
    , grprrsMetadata
    , grprrsResponseStatus
    ) where

import Network.AWS.CostExplorer.Types
import Network.AWS.CostExplorer.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getReservationPurchaseRecommendation' smart constructor.
data GetReservationPurchaseRecommendation = GetReservationPurchaseRecommendation'
  { _grprNextPageToken        :: !(Maybe Text)
  , _grprTermInYears          :: !(Maybe TermInYears)
  , _grprServiceSpecification :: !(Maybe ServiceSpecification)
  , _grprAccountScope         :: !(Maybe AccountScope)
  , _grprAccountId            :: !(Maybe Text)
  , _grprPageSize             :: !(Maybe Nat)
  , _grprLookbackPeriodInDays :: !(Maybe LookbackPeriodInDays)
  , _grprPaymentOption        :: !(Maybe PaymentOption)
  , _grprService              :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetReservationPurchaseRecommendation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grprNextPageToken' - The pagination token that indicates the next set of results that you want to retrieve.
--
-- * 'grprTermInYears' - The reservation term that you want recommendations for.
--
-- * 'grprServiceSpecification' - The hardware specifications for the service instances that you want recommendations for, such as standard or convertible EC2 instances.
--
-- * 'grprAccountScope' - The account scope that you want recommendations for. The only valid value is @Payer@ . This means that AWS includes the master account and any member accounts when it calculates its recommendations.
--
-- * 'grprAccountId' - The account ID that is associated with the recommendation.
--
-- * 'grprPageSize' - The number of recommendations that you want returned in a single response object.
--
-- * 'grprLookbackPeriodInDays' - The number of previous days that you want AWS to consider when it calculates your recommendations.
--
-- * 'grprPaymentOption' - The reservation purchase option that you want recommendations for.
--
-- * 'grprService' - The specific service that you want recommendations for.
getReservationPurchaseRecommendation
    :: Text -- ^ 'grprService'
    -> GetReservationPurchaseRecommendation
getReservationPurchaseRecommendation pService_ =
  GetReservationPurchaseRecommendation'
    { _grprNextPageToken = Nothing
    , _grprTermInYears = Nothing
    , _grprServiceSpecification = Nothing
    , _grprAccountScope = Nothing
    , _grprAccountId = Nothing
    , _grprPageSize = Nothing
    , _grprLookbackPeriodInDays = Nothing
    , _grprPaymentOption = Nothing
    , _grprService = pService_
    }


-- | The pagination token that indicates the next set of results that you want to retrieve.
grprNextPageToken :: Lens' GetReservationPurchaseRecommendation (Maybe Text)
grprNextPageToken = lens _grprNextPageToken (\ s a -> s{_grprNextPageToken = a})

-- | The reservation term that you want recommendations for.
grprTermInYears :: Lens' GetReservationPurchaseRecommendation (Maybe TermInYears)
grprTermInYears = lens _grprTermInYears (\ s a -> s{_grprTermInYears = a})

-- | The hardware specifications for the service instances that you want recommendations for, such as standard or convertible EC2 instances.
grprServiceSpecification :: Lens' GetReservationPurchaseRecommendation (Maybe ServiceSpecification)
grprServiceSpecification = lens _grprServiceSpecification (\ s a -> s{_grprServiceSpecification = a})

-- | The account scope that you want recommendations for. The only valid value is @Payer@ . This means that AWS includes the master account and any member accounts when it calculates its recommendations.
grprAccountScope :: Lens' GetReservationPurchaseRecommendation (Maybe AccountScope)
grprAccountScope = lens _grprAccountScope (\ s a -> s{_grprAccountScope = a})

-- | The account ID that is associated with the recommendation.
grprAccountId :: Lens' GetReservationPurchaseRecommendation (Maybe Text)
grprAccountId = lens _grprAccountId (\ s a -> s{_grprAccountId = a})

-- | The number of recommendations that you want returned in a single response object.
grprPageSize :: Lens' GetReservationPurchaseRecommendation (Maybe Natural)
grprPageSize = lens _grprPageSize (\ s a -> s{_grprPageSize = a}) . mapping _Nat

-- | The number of previous days that you want AWS to consider when it calculates your recommendations.
grprLookbackPeriodInDays :: Lens' GetReservationPurchaseRecommendation (Maybe LookbackPeriodInDays)
grprLookbackPeriodInDays = lens _grprLookbackPeriodInDays (\ s a -> s{_grprLookbackPeriodInDays = a})

-- | The reservation purchase option that you want recommendations for.
grprPaymentOption :: Lens' GetReservationPurchaseRecommendation (Maybe PaymentOption)
grprPaymentOption = lens _grprPaymentOption (\ s a -> s{_grprPaymentOption = a})

-- | The specific service that you want recommendations for.
grprService :: Lens' GetReservationPurchaseRecommendation Text
grprService = lens _grprService (\ s a -> s{_grprService = a})

instance AWSRequest
           GetReservationPurchaseRecommendation
         where
        type Rs GetReservationPurchaseRecommendation =
             GetReservationPurchaseRecommendationResponse
        request = postJSON costExplorer
        response
          = receiveJSON
              (\ s h x ->
                 GetReservationPurchaseRecommendationResponse' <$>
                   (x .?> "NextPageToken") <*>
                     (x .?> "Recommendations" .!@ mempty)
                     <*> (x .?> "Metadata")
                     <*> (pure (fromEnum s)))

instance Hashable
           GetReservationPurchaseRecommendation
         where

instance NFData GetReservationPurchaseRecommendation
         where

instance ToHeaders
           GetReservationPurchaseRecommendation
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSInsightsIndexService.GetReservationPurchaseRecommendation"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetReservationPurchaseRecommendation
         where
        toJSON GetReservationPurchaseRecommendation'{..}
          = object
              (catMaybes
                 [("NextPageToken" .=) <$> _grprNextPageToken,
                  ("TermInYears" .=) <$> _grprTermInYears,
                  ("ServiceSpecification" .=) <$>
                    _grprServiceSpecification,
                  ("AccountScope" .=) <$> _grprAccountScope,
                  ("AccountId" .=) <$> _grprAccountId,
                  ("PageSize" .=) <$> _grprPageSize,
                  ("LookbackPeriodInDays" .=) <$>
                    _grprLookbackPeriodInDays,
                  ("PaymentOption" .=) <$> _grprPaymentOption,
                  Just ("Service" .= _grprService)])

instance ToPath GetReservationPurchaseRecommendation
         where
        toPath = const "/"

instance ToQuery GetReservationPurchaseRecommendation
         where
        toQuery = const mempty

-- | /See:/ 'getReservationPurchaseRecommendationResponse' smart constructor.
data GetReservationPurchaseRecommendationResponse = GetReservationPurchaseRecommendationResponse'
  { _grprrsNextPageToken   :: !(Maybe Text)
  , _grprrsRecommendations :: !(Maybe [ReservationPurchaseRecommendation])
  , _grprrsMetadata        :: !(Maybe ReservationPurchaseRecommendationMetadata)
  , _grprrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetReservationPurchaseRecommendationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grprrsNextPageToken' - The pagination token for the next set of retrievable results.
--
-- * 'grprrsRecommendations' - Recommendations for reservations to purchase.
--
-- * 'grprrsMetadata' - Information about this specific recommendation call, such as the time stamp for when Cost Explorer generated this recommendation.
--
-- * 'grprrsResponseStatus' - -- | The response status code.
getReservationPurchaseRecommendationResponse
    :: Int -- ^ 'grprrsResponseStatus'
    -> GetReservationPurchaseRecommendationResponse
getReservationPurchaseRecommendationResponse pResponseStatus_ =
  GetReservationPurchaseRecommendationResponse'
    { _grprrsNextPageToken = Nothing
    , _grprrsRecommendations = Nothing
    , _grprrsMetadata = Nothing
    , _grprrsResponseStatus = pResponseStatus_
    }


-- | The pagination token for the next set of retrievable results.
grprrsNextPageToken :: Lens' GetReservationPurchaseRecommendationResponse (Maybe Text)
grprrsNextPageToken = lens _grprrsNextPageToken (\ s a -> s{_grprrsNextPageToken = a})

-- | Recommendations for reservations to purchase.
grprrsRecommendations :: Lens' GetReservationPurchaseRecommendationResponse [ReservationPurchaseRecommendation]
grprrsRecommendations = lens _grprrsRecommendations (\ s a -> s{_grprrsRecommendations = a}) . _Default . _Coerce

-- | Information about this specific recommendation call, such as the time stamp for when Cost Explorer generated this recommendation.
grprrsMetadata :: Lens' GetReservationPurchaseRecommendationResponse (Maybe ReservationPurchaseRecommendationMetadata)
grprrsMetadata = lens _grprrsMetadata (\ s a -> s{_grprrsMetadata = a})

-- | -- | The response status code.
grprrsResponseStatus :: Lens' GetReservationPurchaseRecommendationResponse Int
grprrsResponseStatus = lens _grprrsResponseStatus (\ s a -> s{_grprrsResponseStatus = a})

instance NFData
           GetReservationPurchaseRecommendationResponse
         where
