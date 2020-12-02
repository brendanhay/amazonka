{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.EC2InstanceDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.EC2InstanceDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details about the Amazon EC2 instances that AWS recommends that you purchase.
--
--
--
-- /See:/ 'ec2InstanceDetails' smart constructor.
data EC2InstanceDetails = EC2InstanceDetails'
  { _eidCurrentGeneration ::
      !(Maybe Bool),
    _eidPlatform :: !(Maybe Text),
    _eidFamily :: !(Maybe Text),
    _eidInstanceType :: !(Maybe Text),
    _eidAvailabilityZone :: !(Maybe Text),
    _eidSizeFlexEligible :: !(Maybe Bool),
    _eidTenancy :: !(Maybe Text),
    _eidRegion :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EC2InstanceDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eidCurrentGeneration' - Whether the recommendation is for a current-generation instance.
--
-- * 'eidPlatform' - The platform of the recommended reservation. The platform is the specific combination of operating system, license model, and software on an instance.
--
-- * 'eidFamily' - The instance family of the recommended reservation.
--
-- * 'eidInstanceType' - The type of instance that AWS recommends.
--
-- * 'eidAvailabilityZone' - The Availability Zone of the recommended reservation.
--
-- * 'eidSizeFlexEligible' - Whether the recommended reservation is size flexible.
--
-- * 'eidTenancy' - Whether the recommended reservation is dedicated or shared.
--
-- * 'eidRegion' - The AWS Region of the recommended reservation.
ec2InstanceDetails ::
  EC2InstanceDetails
ec2InstanceDetails =
  EC2InstanceDetails'
    { _eidCurrentGeneration = Nothing,
      _eidPlatform = Nothing,
      _eidFamily = Nothing,
      _eidInstanceType = Nothing,
      _eidAvailabilityZone = Nothing,
      _eidSizeFlexEligible = Nothing,
      _eidTenancy = Nothing,
      _eidRegion = Nothing
    }

-- | Whether the recommendation is for a current-generation instance.
eidCurrentGeneration :: Lens' EC2InstanceDetails (Maybe Bool)
eidCurrentGeneration = lens _eidCurrentGeneration (\s a -> s {_eidCurrentGeneration = a})

-- | The platform of the recommended reservation. The platform is the specific combination of operating system, license model, and software on an instance.
eidPlatform :: Lens' EC2InstanceDetails (Maybe Text)
eidPlatform = lens _eidPlatform (\s a -> s {_eidPlatform = a})

-- | The instance family of the recommended reservation.
eidFamily :: Lens' EC2InstanceDetails (Maybe Text)
eidFamily = lens _eidFamily (\s a -> s {_eidFamily = a})

-- | The type of instance that AWS recommends.
eidInstanceType :: Lens' EC2InstanceDetails (Maybe Text)
eidInstanceType = lens _eidInstanceType (\s a -> s {_eidInstanceType = a})

-- | The Availability Zone of the recommended reservation.
eidAvailabilityZone :: Lens' EC2InstanceDetails (Maybe Text)
eidAvailabilityZone = lens _eidAvailabilityZone (\s a -> s {_eidAvailabilityZone = a})

-- | Whether the recommended reservation is size flexible.
eidSizeFlexEligible :: Lens' EC2InstanceDetails (Maybe Bool)
eidSizeFlexEligible = lens _eidSizeFlexEligible (\s a -> s {_eidSizeFlexEligible = a})

-- | Whether the recommended reservation is dedicated or shared.
eidTenancy :: Lens' EC2InstanceDetails (Maybe Text)
eidTenancy = lens _eidTenancy (\s a -> s {_eidTenancy = a})

-- | The AWS Region of the recommended reservation.
eidRegion :: Lens' EC2InstanceDetails (Maybe Text)
eidRegion = lens _eidRegion (\s a -> s {_eidRegion = a})

instance FromJSON EC2InstanceDetails where
  parseJSON =
    withObject
      "EC2InstanceDetails"
      ( \x ->
          EC2InstanceDetails'
            <$> (x .:? "CurrentGeneration")
            <*> (x .:? "Platform")
            <*> (x .:? "Family")
            <*> (x .:? "InstanceType")
            <*> (x .:? "AvailabilityZone")
            <*> (x .:? "SizeFlexEligible")
            <*> (x .:? "Tenancy")
            <*> (x .:? "Region")
      )

instance Hashable EC2InstanceDetails

instance NFData EC2InstanceDetails
