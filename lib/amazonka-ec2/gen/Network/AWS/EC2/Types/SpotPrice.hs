{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotPrice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotPrice where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.RIProductDescription
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the maximum price per hour that you are willing to pay for a Spot Instance.
--
--
--
-- /See:/ 'spotPrice' smart constructor.
data SpotPrice = SpotPrice'
  { _sProductDescription ::
      !(Maybe RIProductDescription),
    _sSpotPrice :: !(Maybe Text),
    _sInstanceType :: !(Maybe InstanceType),
    _sAvailabilityZone :: !(Maybe Text),
    _sTimestamp :: !(Maybe ISO8601)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SpotPrice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sProductDescription' - A general description of the AMI.
--
-- * 'sSpotPrice' - The maximum price per hour that you are willing to pay for a Spot Instance.
--
-- * 'sInstanceType' - The instance type.
--
-- * 'sAvailabilityZone' - The Availability Zone.
--
-- * 'sTimestamp' - The date and time the request was created, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
spotPrice ::
  SpotPrice
spotPrice =
  SpotPrice'
    { _sProductDescription = Nothing,
      _sSpotPrice = Nothing,
      _sInstanceType = Nothing,
      _sAvailabilityZone = Nothing,
      _sTimestamp = Nothing
    }

-- | A general description of the AMI.
sProductDescription :: Lens' SpotPrice (Maybe RIProductDescription)
sProductDescription = lens _sProductDescription (\s a -> s {_sProductDescription = a})

-- | The maximum price per hour that you are willing to pay for a Spot Instance.
sSpotPrice :: Lens' SpotPrice (Maybe Text)
sSpotPrice = lens _sSpotPrice (\s a -> s {_sSpotPrice = a})

-- | The instance type.
sInstanceType :: Lens' SpotPrice (Maybe InstanceType)
sInstanceType = lens _sInstanceType (\s a -> s {_sInstanceType = a})

-- | The Availability Zone.
sAvailabilityZone :: Lens' SpotPrice (Maybe Text)
sAvailabilityZone = lens _sAvailabilityZone (\s a -> s {_sAvailabilityZone = a})

-- | The date and time the request was created, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
sTimestamp :: Lens' SpotPrice (Maybe UTCTime)
sTimestamp = lens _sTimestamp (\s a -> s {_sTimestamp = a}) . mapping _Time

instance FromXML SpotPrice where
  parseXML x =
    SpotPrice'
      <$> (x .@? "productDescription")
      <*> (x .@? "spotPrice")
      <*> (x .@? "instanceType")
      <*> (x .@? "availabilityZone")
      <*> (x .@? "timestamp")

instance Hashable SpotPrice

instance NFData SpotPrice
