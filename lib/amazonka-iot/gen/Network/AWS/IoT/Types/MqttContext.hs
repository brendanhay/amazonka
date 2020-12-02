{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.MqttContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.MqttContext where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the MQTT context to use for the test authorizer request
--
--
--
-- /See:/ 'mqttContext' smart constructor.
data MqttContext = MqttContext'
  { _mcClientId :: !(Maybe Text),
    _mcUsername :: !(Maybe Text),
    _mcPassword :: !(Maybe Base64)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MqttContext' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcClientId' - The value of the @clientId@ key in an MQTT authorization request.
--
-- * 'mcUsername' - The value of the @username@ key in an MQTT authorization request.
--
-- * 'mcPassword' - The value of the @password@ key in an MQTT authorization request.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
mqttContext ::
  MqttContext
mqttContext =
  MqttContext'
    { _mcClientId = Nothing,
      _mcUsername = Nothing,
      _mcPassword = Nothing
    }

-- | The value of the @clientId@ key in an MQTT authorization request.
mcClientId :: Lens' MqttContext (Maybe Text)
mcClientId = lens _mcClientId (\s a -> s {_mcClientId = a})

-- | The value of the @username@ key in an MQTT authorization request.
mcUsername :: Lens' MqttContext (Maybe Text)
mcUsername = lens _mcUsername (\s a -> s {_mcUsername = a})

-- | The value of the @password@ key in an MQTT authorization request.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
mcPassword :: Lens' MqttContext (Maybe ByteString)
mcPassword = lens _mcPassword (\s a -> s {_mcPassword = a}) . mapping _Base64

instance Hashable MqttContext

instance NFData MqttContext

instance ToJSON MqttContext where
  toJSON MqttContext' {..} =
    object
      ( catMaybes
          [ ("clientId" .=) <$> _mcClientId,
            ("username" .=) <$> _mcUsername,
            ("password" .=) <$> _mcPassword
          ]
      )
