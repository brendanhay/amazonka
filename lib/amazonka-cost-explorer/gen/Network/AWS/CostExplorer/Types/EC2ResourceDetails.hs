{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.EC2ResourceDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.EC2ResourceDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details on the Amazon EC2 Resource.
--
--
--
-- /See:/ 'ec2ResourceDetails' smart constructor.
data EC2ResourceDetails = EC2ResourceDetails'
  { _erdPlatform ::
      !(Maybe Text),
    _erdVcpu :: !(Maybe Text),
    _erdNetworkPerformance :: !(Maybe Text),
    _erdMemory :: !(Maybe Text),
    _erdInstanceType :: !(Maybe Text),
    _erdStorage :: !(Maybe Text),
    _erdSku :: !(Maybe Text),
    _erdRegion :: !(Maybe Text),
    _erdHourlyOnDemandRate :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EC2ResourceDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erdPlatform' - The platform of the AWS instance. The platform is the specific combination of operating system, license model, and software on an instance.
--
-- * 'erdVcpu' - Number of VCPU cores in the AWS instance type.
--
-- * 'erdNetworkPerformance' - Network performance capacity of the AWS instance.
--
-- * 'erdMemory' - Memory capacity of the AWS instance.
--
-- * 'erdInstanceType' - The type of AWS instance.
--
-- * 'erdStorage' - The disk storage of the AWS instance (not EBS storage).
--
-- * 'erdSku' - The SKU of the product.
--
-- * 'erdRegion' - The AWS Region of the instance.
--
-- * 'erdHourlyOnDemandRate' - Hourly public On-Demand rate for the instance type.
ec2ResourceDetails ::
  EC2ResourceDetails
ec2ResourceDetails =
  EC2ResourceDetails'
    { _erdPlatform = Nothing,
      _erdVcpu = Nothing,
      _erdNetworkPerformance = Nothing,
      _erdMemory = Nothing,
      _erdInstanceType = Nothing,
      _erdStorage = Nothing,
      _erdSku = Nothing,
      _erdRegion = Nothing,
      _erdHourlyOnDemandRate = Nothing
    }

-- | The platform of the AWS instance. The platform is the specific combination of operating system, license model, and software on an instance.
erdPlatform :: Lens' EC2ResourceDetails (Maybe Text)
erdPlatform = lens _erdPlatform (\s a -> s {_erdPlatform = a})

-- | Number of VCPU cores in the AWS instance type.
erdVcpu :: Lens' EC2ResourceDetails (Maybe Text)
erdVcpu = lens _erdVcpu (\s a -> s {_erdVcpu = a})

-- | Network performance capacity of the AWS instance.
erdNetworkPerformance :: Lens' EC2ResourceDetails (Maybe Text)
erdNetworkPerformance = lens _erdNetworkPerformance (\s a -> s {_erdNetworkPerformance = a})

-- | Memory capacity of the AWS instance.
erdMemory :: Lens' EC2ResourceDetails (Maybe Text)
erdMemory = lens _erdMemory (\s a -> s {_erdMemory = a})

-- | The type of AWS instance.
erdInstanceType :: Lens' EC2ResourceDetails (Maybe Text)
erdInstanceType = lens _erdInstanceType (\s a -> s {_erdInstanceType = a})

-- | The disk storage of the AWS instance (not EBS storage).
erdStorage :: Lens' EC2ResourceDetails (Maybe Text)
erdStorage = lens _erdStorage (\s a -> s {_erdStorage = a})

-- | The SKU of the product.
erdSku :: Lens' EC2ResourceDetails (Maybe Text)
erdSku = lens _erdSku (\s a -> s {_erdSku = a})

-- | The AWS Region of the instance.
erdRegion :: Lens' EC2ResourceDetails (Maybe Text)
erdRegion = lens _erdRegion (\s a -> s {_erdRegion = a})

-- | Hourly public On-Demand rate for the instance type.
erdHourlyOnDemandRate :: Lens' EC2ResourceDetails (Maybe Text)
erdHourlyOnDemandRate = lens _erdHourlyOnDemandRate (\s a -> s {_erdHourlyOnDemandRate = a})

instance FromJSON EC2ResourceDetails where
  parseJSON =
    withObject
      "EC2ResourceDetails"
      ( \x ->
          EC2ResourceDetails'
            <$> (x .:? "Platform")
            <*> (x .:? "Vcpu")
            <*> (x .:? "NetworkPerformance")
            <*> (x .:? "Memory")
            <*> (x .:? "InstanceType")
            <*> (x .:? "Storage")
            <*> (x .:? "Sku")
            <*> (x .:? "Region")
            <*> (x .:? "HourlyOnDemandRate")
      )

instance Hashable EC2ResourceDetails

instance NFData EC2ResourceDetails
