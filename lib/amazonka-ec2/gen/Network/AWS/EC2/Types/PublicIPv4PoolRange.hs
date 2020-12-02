{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PublicIPv4PoolRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PublicIPv4PoolRange where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an address range of an IPv4 address pool.
--
--
--
-- /See:/ 'publicIPv4PoolRange' smart constructor.
data PublicIPv4PoolRange = PublicIPv4PoolRange'
  { _piprAvailableAddressCount ::
      !(Maybe Int),
    _piprLastAddress :: !(Maybe Text),
    _piprFirstAddress :: !(Maybe Text),
    _piprAddressCount :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PublicIPv4PoolRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piprAvailableAddressCount' - The number of available addresses in the range.
--
-- * 'piprLastAddress' - The last IP address in the range.
--
-- * 'piprFirstAddress' - The first IP address in the range.
--
-- * 'piprAddressCount' - The number of addresses in the range.
publicIPv4PoolRange ::
  PublicIPv4PoolRange
publicIPv4PoolRange =
  PublicIPv4PoolRange'
    { _piprAvailableAddressCount = Nothing,
      _piprLastAddress = Nothing,
      _piprFirstAddress = Nothing,
      _piprAddressCount = Nothing
    }

-- | The number of available addresses in the range.
piprAvailableAddressCount :: Lens' PublicIPv4PoolRange (Maybe Int)
piprAvailableAddressCount = lens _piprAvailableAddressCount (\s a -> s {_piprAvailableAddressCount = a})

-- | The last IP address in the range.
piprLastAddress :: Lens' PublicIPv4PoolRange (Maybe Text)
piprLastAddress = lens _piprLastAddress (\s a -> s {_piprLastAddress = a})

-- | The first IP address in the range.
piprFirstAddress :: Lens' PublicIPv4PoolRange (Maybe Text)
piprFirstAddress = lens _piprFirstAddress (\s a -> s {_piprFirstAddress = a})

-- | The number of addresses in the range.
piprAddressCount :: Lens' PublicIPv4PoolRange (Maybe Int)
piprAddressCount = lens _piprAddressCount (\s a -> s {_piprAddressCount = a})

instance FromXML PublicIPv4PoolRange where
  parseXML x =
    PublicIPv4PoolRange'
      <$> (x .@? "availableAddressCount")
      <*> (x .@? "lastAddress")
      <*> (x .@? "firstAddress")
      <*> (x .@? "addressCount")

instance Hashable PublicIPv4PoolRange

instance NFData PublicIPv4PoolRange
