{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.BrokerInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.BrokerInstance where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about all brokers.
--
-- /See:/ 'brokerInstance' smart constructor.
data BrokerInstance = BrokerInstance'
  { _biIPAddress ::
      !(Maybe Text),
    _biConsoleURL :: !(Maybe Text),
    _biEndpoints :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BrokerInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'biIPAddress' - The IP address of the Elastic Network Interface (ENI) attached to the broker. Does not apply to RabbitMQ brokers
--
-- * 'biConsoleURL' - The URL of the broker's Web Console.
--
-- * 'biEndpoints' - The broker's wire-level protocol endpoints.
brokerInstance ::
  BrokerInstance
brokerInstance =
  BrokerInstance'
    { _biIPAddress = Nothing,
      _biConsoleURL = Nothing,
      _biEndpoints = Nothing
    }

-- | The IP address of the Elastic Network Interface (ENI) attached to the broker. Does not apply to RabbitMQ brokers
biIPAddress :: Lens' BrokerInstance (Maybe Text)
biIPAddress = lens _biIPAddress (\s a -> s {_biIPAddress = a})

-- | The URL of the broker's Web Console.
biConsoleURL :: Lens' BrokerInstance (Maybe Text)
biConsoleURL = lens _biConsoleURL (\s a -> s {_biConsoleURL = a})

-- | The broker's wire-level protocol endpoints.
biEndpoints :: Lens' BrokerInstance [Text]
biEndpoints = lens _biEndpoints (\s a -> s {_biEndpoints = a}) . _Default . _Coerce

instance FromJSON BrokerInstance where
  parseJSON =
    withObject
      "BrokerInstance"
      ( \x ->
          BrokerInstance'
            <$> (x .:? "ipAddress")
            <*> (x .:? "consoleURL")
            <*> (x .:? "endpoints" .!= mempty)
      )

instance Hashable BrokerInstance

instance NFData BrokerInstance
