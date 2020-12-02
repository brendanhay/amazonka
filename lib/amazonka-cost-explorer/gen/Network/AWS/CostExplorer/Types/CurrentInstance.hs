{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CurrentInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CurrentInstance where

import Network.AWS.CostExplorer.Types.ResourceDetails
import Network.AWS.CostExplorer.Types.ResourceUtilization
import Network.AWS.CostExplorer.Types.TagValues
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Context about the current instance.
--
--
--
-- /See:/ 'currentInstance' smart constructor.
data CurrentInstance = CurrentInstance'
  { _ciResourceId ::
      !(Maybe Text),
    _ciCurrencyCode :: !(Maybe Text),
    _ciResourceUtilization :: !(Maybe ResourceUtilization),
    _ciResourceDetails :: !(Maybe ResourceDetails),
    _ciTotalRunningHoursInLookbackPeriod :: !(Maybe Text),
    _ciReservationCoveredHoursInLookbackPeriod :: !(Maybe Text),
    _ciOnDemandHoursInLookbackPeriod :: !(Maybe Text),
    _ciMonthlyCost :: !(Maybe Text),
    _ciInstanceName :: !(Maybe Text),
    _ciSavingsPlansCoveredHoursInLookbackPeriod ::
      !(Maybe Text),
    _ciTags :: !(Maybe [TagValues])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CurrentInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciResourceId' - Resource ID of the current instance.
--
-- * 'ciCurrencyCode' - The currency code that AWS used to calculate the costs for this instance.
--
-- * 'ciResourceUtilization' - Utilization information of the current instance during the lookback period.
--
-- * 'ciResourceDetails' - Details about the resource and utilization.
--
-- * 'ciTotalRunningHoursInLookbackPeriod' - The total number of hours the instance ran during the lookback period.
--
-- * 'ciReservationCoveredHoursInLookbackPeriod' - Number of hours during the lookback period covered by reservations.
--
-- * 'ciOnDemandHoursInLookbackPeriod' - Number of hours during the lookback period billed at On-Demand rates.
--
-- * 'ciMonthlyCost' - Current On-Demand cost of operating this instance on a monthly basis.
--
-- * 'ciInstanceName' - The name you've given an instance. This field will show as blank if you haven't given the instance a name.
--
-- * 'ciSavingsPlansCoveredHoursInLookbackPeriod' - Number of hours during the lookback period covered by Savings Plans.
--
-- * 'ciTags' - Cost allocation resource tags applied to the instance.
currentInstance ::
  CurrentInstance
currentInstance =
  CurrentInstance'
    { _ciResourceId = Nothing,
      _ciCurrencyCode = Nothing,
      _ciResourceUtilization = Nothing,
      _ciResourceDetails = Nothing,
      _ciTotalRunningHoursInLookbackPeriod = Nothing,
      _ciReservationCoveredHoursInLookbackPeriod = Nothing,
      _ciOnDemandHoursInLookbackPeriod = Nothing,
      _ciMonthlyCost = Nothing,
      _ciInstanceName = Nothing,
      _ciSavingsPlansCoveredHoursInLookbackPeriod = Nothing,
      _ciTags = Nothing
    }

-- | Resource ID of the current instance.
ciResourceId :: Lens' CurrentInstance (Maybe Text)
ciResourceId = lens _ciResourceId (\s a -> s {_ciResourceId = a})

-- | The currency code that AWS used to calculate the costs for this instance.
ciCurrencyCode :: Lens' CurrentInstance (Maybe Text)
ciCurrencyCode = lens _ciCurrencyCode (\s a -> s {_ciCurrencyCode = a})

-- | Utilization information of the current instance during the lookback period.
ciResourceUtilization :: Lens' CurrentInstance (Maybe ResourceUtilization)
ciResourceUtilization = lens _ciResourceUtilization (\s a -> s {_ciResourceUtilization = a})

-- | Details about the resource and utilization.
ciResourceDetails :: Lens' CurrentInstance (Maybe ResourceDetails)
ciResourceDetails = lens _ciResourceDetails (\s a -> s {_ciResourceDetails = a})

-- | The total number of hours the instance ran during the lookback period.
ciTotalRunningHoursInLookbackPeriod :: Lens' CurrentInstance (Maybe Text)
ciTotalRunningHoursInLookbackPeriod = lens _ciTotalRunningHoursInLookbackPeriod (\s a -> s {_ciTotalRunningHoursInLookbackPeriod = a})

-- | Number of hours during the lookback period covered by reservations.
ciReservationCoveredHoursInLookbackPeriod :: Lens' CurrentInstance (Maybe Text)
ciReservationCoveredHoursInLookbackPeriod = lens _ciReservationCoveredHoursInLookbackPeriod (\s a -> s {_ciReservationCoveredHoursInLookbackPeriod = a})

-- | Number of hours during the lookback period billed at On-Demand rates.
ciOnDemandHoursInLookbackPeriod :: Lens' CurrentInstance (Maybe Text)
ciOnDemandHoursInLookbackPeriod = lens _ciOnDemandHoursInLookbackPeriod (\s a -> s {_ciOnDemandHoursInLookbackPeriod = a})

-- | Current On-Demand cost of operating this instance on a monthly basis.
ciMonthlyCost :: Lens' CurrentInstance (Maybe Text)
ciMonthlyCost = lens _ciMonthlyCost (\s a -> s {_ciMonthlyCost = a})

-- | The name you've given an instance. This field will show as blank if you haven't given the instance a name.
ciInstanceName :: Lens' CurrentInstance (Maybe Text)
ciInstanceName = lens _ciInstanceName (\s a -> s {_ciInstanceName = a})

-- | Number of hours during the lookback period covered by Savings Plans.
ciSavingsPlansCoveredHoursInLookbackPeriod :: Lens' CurrentInstance (Maybe Text)
ciSavingsPlansCoveredHoursInLookbackPeriod = lens _ciSavingsPlansCoveredHoursInLookbackPeriod (\s a -> s {_ciSavingsPlansCoveredHoursInLookbackPeriod = a})

-- | Cost allocation resource tags applied to the instance.
ciTags :: Lens' CurrentInstance [TagValues]
ciTags = lens _ciTags (\s a -> s {_ciTags = a}) . _Default . _Coerce

instance FromJSON CurrentInstance where
  parseJSON =
    withObject
      "CurrentInstance"
      ( \x ->
          CurrentInstance'
            <$> (x .:? "ResourceId")
            <*> (x .:? "CurrencyCode")
            <*> (x .:? "ResourceUtilization")
            <*> (x .:? "ResourceDetails")
            <*> (x .:? "TotalRunningHoursInLookbackPeriod")
            <*> (x .:? "ReservationCoveredHoursInLookbackPeriod")
            <*> (x .:? "OnDemandHoursInLookbackPeriod")
            <*> (x .:? "MonthlyCost")
            <*> (x .:? "InstanceName")
            <*> (x .:? "SavingsPlansCoveredHoursInLookbackPeriod")
            <*> (x .:? "Tags" .!= mempty)
      )

instance Hashable CurrentInstance

instance NFData CurrentInstance
