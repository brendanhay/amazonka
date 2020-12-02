{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendation where

import Network.AWS.CostExplorer.Types.AccountScope
import Network.AWS.CostExplorer.Types.LookbackPeriodInDays
import Network.AWS.CostExplorer.Types.PaymentOption
import Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationDetail
import Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationSummary
import Network.AWS.CostExplorer.Types.ServiceSpecification
import Network.AWS.CostExplorer.Types.TermInYears
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A specific reservation that AWS recommends for purchase.
--
--
--
-- /See:/ 'reservationPurchaseRecommendation' smart constructor.
data ReservationPurchaseRecommendation = ReservationPurchaseRecommendation'
  { _rprTermInYears ::
      !(Maybe TermInYears),
    _rprRecommendationSummary ::
      !( Maybe
           ReservationPurchaseRecommendationSummary
       ),
    _rprServiceSpecification ::
      !( Maybe
           ServiceSpecification
       ),
    _rprAccountScope ::
      !(Maybe AccountScope),
    _rprRecommendationDetails ::
      !( Maybe
           [ReservationPurchaseRecommendationDetail]
       ),
    _rprLookbackPeriodInDays ::
      !( Maybe
           LookbackPeriodInDays
       ),
    _rprPaymentOption ::
      !(Maybe PaymentOption)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReservationPurchaseRecommendation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rprTermInYears' - The term of the reservation that you want recommendations for, in years.
--
-- * 'rprRecommendationSummary' - A summary about the recommended purchase.
--
-- * 'rprServiceSpecification' - Hardware specifications for the service that you want recommendations for.
--
-- * 'rprAccountScope' - The account scope that AWS recommends that you purchase this instance for. For example, you can purchase this reservation for an entire organization in AWS Organizations.
--
-- * 'rprRecommendationDetails' - Details about the recommended purchases.
--
-- * 'rprLookbackPeriodInDays' - How many days of previous usage that AWS considers when making this recommendation.
--
-- * 'rprPaymentOption' - The payment option for the reservation. For example, @AllUpfront@ or @NoUpfront@ .
reservationPurchaseRecommendation ::
  ReservationPurchaseRecommendation
reservationPurchaseRecommendation =
  ReservationPurchaseRecommendation'
    { _rprTermInYears = Nothing,
      _rprRecommendationSummary = Nothing,
      _rprServiceSpecification = Nothing,
      _rprAccountScope = Nothing,
      _rprRecommendationDetails = Nothing,
      _rprLookbackPeriodInDays = Nothing,
      _rprPaymentOption = Nothing
    }

-- | The term of the reservation that you want recommendations for, in years.
rprTermInYears :: Lens' ReservationPurchaseRecommendation (Maybe TermInYears)
rprTermInYears = lens _rprTermInYears (\s a -> s {_rprTermInYears = a})

-- | A summary about the recommended purchase.
rprRecommendationSummary :: Lens' ReservationPurchaseRecommendation (Maybe ReservationPurchaseRecommendationSummary)
rprRecommendationSummary = lens _rprRecommendationSummary (\s a -> s {_rprRecommendationSummary = a})

-- | Hardware specifications for the service that you want recommendations for.
rprServiceSpecification :: Lens' ReservationPurchaseRecommendation (Maybe ServiceSpecification)
rprServiceSpecification = lens _rprServiceSpecification (\s a -> s {_rprServiceSpecification = a})

-- | The account scope that AWS recommends that you purchase this instance for. For example, you can purchase this reservation for an entire organization in AWS Organizations.
rprAccountScope :: Lens' ReservationPurchaseRecommendation (Maybe AccountScope)
rprAccountScope = lens _rprAccountScope (\s a -> s {_rprAccountScope = a})

-- | Details about the recommended purchases.
rprRecommendationDetails :: Lens' ReservationPurchaseRecommendation [ReservationPurchaseRecommendationDetail]
rprRecommendationDetails = lens _rprRecommendationDetails (\s a -> s {_rprRecommendationDetails = a}) . _Default . _Coerce

-- | How many days of previous usage that AWS considers when making this recommendation.
rprLookbackPeriodInDays :: Lens' ReservationPurchaseRecommendation (Maybe LookbackPeriodInDays)
rprLookbackPeriodInDays = lens _rprLookbackPeriodInDays (\s a -> s {_rprLookbackPeriodInDays = a})

-- | The payment option for the reservation. For example, @AllUpfront@ or @NoUpfront@ .
rprPaymentOption :: Lens' ReservationPurchaseRecommendation (Maybe PaymentOption)
rprPaymentOption = lens _rprPaymentOption (\s a -> s {_rprPaymentOption = a})

instance FromJSON ReservationPurchaseRecommendation where
  parseJSON =
    withObject
      "ReservationPurchaseRecommendation"
      ( \x ->
          ReservationPurchaseRecommendation'
            <$> (x .:? "TermInYears")
            <*> (x .:? "RecommendationSummary")
            <*> (x .:? "ServiceSpecification")
            <*> (x .:? "AccountScope")
            <*> (x .:? "RecommendationDetails" .!= mempty)
            <*> (x .:? "LookbackPeriodInDays")
            <*> (x .:? "PaymentOption")
      )

instance Hashable ReservationPurchaseRecommendation

instance NFData ReservationPurchaseRecommendation
