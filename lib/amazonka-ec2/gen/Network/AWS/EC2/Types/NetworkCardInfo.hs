{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkCardInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkCardInfo where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the network card support of the instance type.
--
--
--
-- /See:/ 'networkCardInfo' smart constructor.
data NetworkCardInfo = NetworkCardInfo'
  { _nciMaximumNetworkInterfaces ::
      !(Maybe Int),
    _nciNetworkPerformance :: !(Maybe Text),
    _nciNetworkCardIndex :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NetworkCardInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nciMaximumNetworkInterfaces' - The maximum number of network interfaces for the network card.
--
-- * 'nciNetworkPerformance' - The network performance of the network card.
--
-- * 'nciNetworkCardIndex' - The index of the network card.
networkCardInfo ::
  NetworkCardInfo
networkCardInfo =
  NetworkCardInfo'
    { _nciMaximumNetworkInterfaces = Nothing,
      _nciNetworkPerformance = Nothing,
      _nciNetworkCardIndex = Nothing
    }

-- | The maximum number of network interfaces for the network card.
nciMaximumNetworkInterfaces :: Lens' NetworkCardInfo (Maybe Int)
nciMaximumNetworkInterfaces = lens _nciMaximumNetworkInterfaces (\s a -> s {_nciMaximumNetworkInterfaces = a})

-- | The network performance of the network card.
nciNetworkPerformance :: Lens' NetworkCardInfo (Maybe Text)
nciNetworkPerformance = lens _nciNetworkPerformance (\s a -> s {_nciNetworkPerformance = a})

-- | The index of the network card.
nciNetworkCardIndex :: Lens' NetworkCardInfo (Maybe Int)
nciNetworkCardIndex = lens _nciNetworkCardIndex (\s a -> s {_nciNetworkCardIndex = a})

instance FromXML NetworkCardInfo where
  parseXML x =
    NetworkCardInfo'
      <$> (x .@? "maximumNetworkInterfaces")
      <*> (x .@? "networkPerformance")
      <*> (x .@? "networkCardIndex")

instance Hashable NetworkCardInfo

instance NFData NetworkCardInfo
