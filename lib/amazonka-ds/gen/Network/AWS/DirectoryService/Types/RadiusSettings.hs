{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.RadiusSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.RadiusSettings where

import Network.AWS.DirectoryService.Types.RadiusAuthenticationProtocol
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a Remote Authentication Dial In User Service (RADIUS) server.
--
--
--
-- /See:/ 'radiusSettings' smart constructor.
data RadiusSettings = RadiusSettings'
  { _rsDisplayLabel ::
      !(Maybe Text),
    _rsRadiusRetries :: !(Maybe Nat),
    _rsAuthenticationProtocol ::
      !(Maybe RadiusAuthenticationProtocol),
    _rsRadiusServers :: !(Maybe [Text]),
    _rsUseSameUsername :: !(Maybe Bool),
    _rsSharedSecret :: !(Maybe (Sensitive Text)),
    _rsRadiusTimeout :: !(Maybe Nat),
    _rsRadiusPort :: !(Maybe Nat)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'RadiusSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsDisplayLabel' - Not currently used.
--
-- * 'rsRadiusRetries' - The maximum number of times that communication with the RADIUS server is attempted.
--
-- * 'rsAuthenticationProtocol' - The protocol specified for your RADIUS endpoints.
--
-- * 'rsRadiusServers' - An array of strings that contains the fully qualified domain name (FQDN) or IP addresses of the RADIUS server endpoints, or the FQDN or IP addresses of your RADIUS server load balancer.
--
-- * 'rsUseSameUsername' - Not currently used.
--
-- * 'rsSharedSecret' - Required for enabling RADIUS on the directory.
--
-- * 'rsRadiusTimeout' - The amount of time, in seconds, to wait for the RADIUS server to respond.
--
-- * 'rsRadiusPort' - The port that your RADIUS server is using for communications. Your on-premises network must allow inbound traffic over this port from the AWS Directory Service servers.
radiusSettings ::
  RadiusSettings
radiusSettings =
  RadiusSettings'
    { _rsDisplayLabel = Nothing,
      _rsRadiusRetries = Nothing,
      _rsAuthenticationProtocol = Nothing,
      _rsRadiusServers = Nothing,
      _rsUseSameUsername = Nothing,
      _rsSharedSecret = Nothing,
      _rsRadiusTimeout = Nothing,
      _rsRadiusPort = Nothing
    }

-- | Not currently used.
rsDisplayLabel :: Lens' RadiusSettings (Maybe Text)
rsDisplayLabel = lens _rsDisplayLabel (\s a -> s {_rsDisplayLabel = a})

-- | The maximum number of times that communication with the RADIUS server is attempted.
rsRadiusRetries :: Lens' RadiusSettings (Maybe Natural)
rsRadiusRetries = lens _rsRadiusRetries (\s a -> s {_rsRadiusRetries = a}) . mapping _Nat

-- | The protocol specified for your RADIUS endpoints.
rsAuthenticationProtocol :: Lens' RadiusSettings (Maybe RadiusAuthenticationProtocol)
rsAuthenticationProtocol = lens _rsAuthenticationProtocol (\s a -> s {_rsAuthenticationProtocol = a})

-- | An array of strings that contains the fully qualified domain name (FQDN) or IP addresses of the RADIUS server endpoints, or the FQDN or IP addresses of your RADIUS server load balancer.
rsRadiusServers :: Lens' RadiusSettings [Text]
rsRadiusServers = lens _rsRadiusServers (\s a -> s {_rsRadiusServers = a}) . _Default . _Coerce

-- | Not currently used.
rsUseSameUsername :: Lens' RadiusSettings (Maybe Bool)
rsUseSameUsername = lens _rsUseSameUsername (\s a -> s {_rsUseSameUsername = a})

-- | Required for enabling RADIUS on the directory.
rsSharedSecret :: Lens' RadiusSettings (Maybe Text)
rsSharedSecret = lens _rsSharedSecret (\s a -> s {_rsSharedSecret = a}) . mapping _Sensitive

-- | The amount of time, in seconds, to wait for the RADIUS server to respond.
rsRadiusTimeout :: Lens' RadiusSettings (Maybe Natural)
rsRadiusTimeout = lens _rsRadiusTimeout (\s a -> s {_rsRadiusTimeout = a}) . mapping _Nat

-- | The port that your RADIUS server is using for communications. Your on-premises network must allow inbound traffic over this port from the AWS Directory Service servers.
rsRadiusPort :: Lens' RadiusSettings (Maybe Natural)
rsRadiusPort = lens _rsRadiusPort (\s a -> s {_rsRadiusPort = a}) . mapping _Nat

instance FromJSON RadiusSettings where
  parseJSON =
    withObject
      "RadiusSettings"
      ( \x ->
          RadiusSettings'
            <$> (x .:? "DisplayLabel")
            <*> (x .:? "RadiusRetries")
            <*> (x .:? "AuthenticationProtocol")
            <*> (x .:? "RadiusServers" .!= mempty)
            <*> (x .:? "UseSameUsername")
            <*> (x .:? "SharedSecret")
            <*> (x .:? "RadiusTimeout")
            <*> (x .:? "RadiusPort")
      )

instance Hashable RadiusSettings

instance NFData RadiusSettings

instance ToJSON RadiusSettings where
  toJSON RadiusSettings' {..} =
    object
      ( catMaybes
          [ ("DisplayLabel" .=) <$> _rsDisplayLabel,
            ("RadiusRetries" .=) <$> _rsRadiusRetries,
            ("AuthenticationProtocol" .=) <$> _rsAuthenticationProtocol,
            ("RadiusServers" .=) <$> _rsRadiusServers,
            ("UseSameUsername" .=) <$> _rsUseSameUsername,
            ("SharedSecret" .=) <$> _rsSharedSecret,
            ("RadiusTimeout" .=) <$> _rsRadiusTimeout,
            ("RadiusPort" .=) <$> _rsRadiusPort
          ]
      )
