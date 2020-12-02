{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.Session
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.Session where

import Network.AWS.AppStream.Types.AuthenticationType
import Network.AWS.AppStream.Types.NetworkAccessConfiguration
import Network.AWS.AppStream.Types.SessionConnectionState
import Network.AWS.AppStream.Types.SessionState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a streaming session.
--
--
--
-- /See:/ 'session' smart constructor.
data Session = Session'
  { _sNetworkAccessConfiguration ::
      !(Maybe NetworkAccessConfiguration),
    _sMaxExpirationTime :: !(Maybe POSIX),
    _sStartTime :: !(Maybe POSIX),
    _sAuthenticationType :: !(Maybe AuthenticationType),
    _sConnectionState :: !(Maybe SessionConnectionState),
    _sId :: !Text,
    _sUserId :: !Text,
    _sStackName :: !Text,
    _sFleetName :: !Text,
    _sState :: !SessionState
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Session' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sNetworkAccessConfiguration' - The network details for the streaming session.
--
-- * 'sMaxExpirationTime' - The time when the streaming session is set to expire. This time is based on the @MaxUserDurationinSeconds@ value, which determines the maximum length of time that a streaming session can run. A streaming session might end earlier than the time specified in @SessionMaxExpirationTime@ , when the @DisconnectTimeOutInSeconds@ elapses or the user chooses to end his or her session. If the @DisconnectTimeOutInSeconds@ elapses, or the user chooses to end his or her session, the streaming instance is terminated and the streaming session ends.
--
-- * 'sStartTime' - The time when a streaming instance is dedicated for the user.
--
-- * 'sAuthenticationType' - The authentication method. The user is authenticated using a streaming URL (@API@ ) or SAML 2.0 federation (@SAML@ ).
--
-- * 'sConnectionState' - Specifies whether a user is connected to the streaming session.
--
-- * 'sId' - The identifier of the streaming session.
--
-- * 'sUserId' - The identifier of the user for whom the session was created.
--
-- * 'sStackName' - The name of the stack for the streaming session.
--
-- * 'sFleetName' - The name of the fleet for the streaming session.
--
-- * 'sState' - The current state of the streaming session.
session ::
  -- | 'sId'
  Text ->
  -- | 'sUserId'
  Text ->
  -- | 'sStackName'
  Text ->
  -- | 'sFleetName'
  Text ->
  -- | 'sState'
  SessionState ->
  Session
session pId_ pUserId_ pStackName_ pFleetName_ pState_ =
  Session'
    { _sNetworkAccessConfiguration = Nothing,
      _sMaxExpirationTime = Nothing,
      _sStartTime = Nothing,
      _sAuthenticationType = Nothing,
      _sConnectionState = Nothing,
      _sId = pId_,
      _sUserId = pUserId_,
      _sStackName = pStackName_,
      _sFleetName = pFleetName_,
      _sState = pState_
    }

-- | The network details for the streaming session.
sNetworkAccessConfiguration :: Lens' Session (Maybe NetworkAccessConfiguration)
sNetworkAccessConfiguration = lens _sNetworkAccessConfiguration (\s a -> s {_sNetworkAccessConfiguration = a})

-- | The time when the streaming session is set to expire. This time is based on the @MaxUserDurationinSeconds@ value, which determines the maximum length of time that a streaming session can run. A streaming session might end earlier than the time specified in @SessionMaxExpirationTime@ , when the @DisconnectTimeOutInSeconds@ elapses or the user chooses to end his or her session. If the @DisconnectTimeOutInSeconds@ elapses, or the user chooses to end his or her session, the streaming instance is terminated and the streaming session ends.
sMaxExpirationTime :: Lens' Session (Maybe UTCTime)
sMaxExpirationTime = lens _sMaxExpirationTime (\s a -> s {_sMaxExpirationTime = a}) . mapping _Time

-- | The time when a streaming instance is dedicated for the user.
sStartTime :: Lens' Session (Maybe UTCTime)
sStartTime = lens _sStartTime (\s a -> s {_sStartTime = a}) . mapping _Time

-- | The authentication method. The user is authenticated using a streaming URL (@API@ ) or SAML 2.0 federation (@SAML@ ).
sAuthenticationType :: Lens' Session (Maybe AuthenticationType)
sAuthenticationType = lens _sAuthenticationType (\s a -> s {_sAuthenticationType = a})

-- | Specifies whether a user is connected to the streaming session.
sConnectionState :: Lens' Session (Maybe SessionConnectionState)
sConnectionState = lens _sConnectionState (\s a -> s {_sConnectionState = a})

-- | The identifier of the streaming session.
sId :: Lens' Session Text
sId = lens _sId (\s a -> s {_sId = a})

-- | The identifier of the user for whom the session was created.
sUserId :: Lens' Session Text
sUserId = lens _sUserId (\s a -> s {_sUserId = a})

-- | The name of the stack for the streaming session.
sStackName :: Lens' Session Text
sStackName = lens _sStackName (\s a -> s {_sStackName = a})

-- | The name of the fleet for the streaming session.
sFleetName :: Lens' Session Text
sFleetName = lens _sFleetName (\s a -> s {_sFleetName = a})

-- | The current state of the streaming session.
sState :: Lens' Session SessionState
sState = lens _sState (\s a -> s {_sState = a})

instance FromJSON Session where
  parseJSON =
    withObject
      "Session"
      ( \x ->
          Session'
            <$> (x .:? "NetworkAccessConfiguration")
            <*> (x .:? "MaxExpirationTime")
            <*> (x .:? "StartTime")
            <*> (x .:? "AuthenticationType")
            <*> (x .:? "ConnectionState")
            <*> (x .: "Id")
            <*> (x .: "UserId")
            <*> (x .: "StackName")
            <*> (x .: "FleetName")
            <*> (x .: "State")
      )

instance Hashable Session

instance NFData Session
