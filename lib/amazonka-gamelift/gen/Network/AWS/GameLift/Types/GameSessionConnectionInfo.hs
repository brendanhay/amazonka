{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameSessionConnectionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameSessionConnectionInfo where

import Network.AWS.GameLift.Types.MatchedPlayerSession
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Connection information for a new game session that is created in response to a 'StartMatchmaking' request. Once a match is made, the FlexMatch engine creates a new game session for it. This information, including the game session endpoint and player sessions for each player in the original matchmaking request, is added to the 'MatchmakingTicket' , which can be retrieved by calling 'DescribeMatchmaking' .
--
--
--
-- /See:/ 'gameSessionConnectionInfo' smart constructor.
data GameSessionConnectionInfo = GameSessionConnectionInfo'
  { _gsciMatchedPlayerSessions ::
      !(Maybe [MatchedPlayerSession]),
    _gsciIPAddress :: !(Maybe Text),
    _gsciGameSessionARN :: !(Maybe Text),
    _gsciDNSName :: !(Maybe Text),
    _gsciPort :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GameSessionConnectionInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsciMatchedPlayerSessions' - A collection of player session IDs, one for each player ID that was included in the original matchmaking request.
--
-- * 'gsciIPAddress' - IP address of the instance that is running the game session. When connecting to a Amazon GameLift game server, a client needs to reference an IP address (or DNS name) and port number.
--
-- * 'gsciGameSessionARN' - Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a game session and uniquely identifies it.
--
-- * 'gsciDNSName' - DNS identifier assigned to the instance that is running the game session. Values have the following format:     * TLS-enabled fleets: @<unique identifier>.<region identifier>.amazongamelift.com@ .     * Non-TLS-enabled fleets: @ec2-<unique identifier>.compute.amazonaws.com@ . (See <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-instance-addressing.html#concepts-public-addresses Amazon EC2 Instance IP Addressing> .) When connecting to a game session that is running on a TLS-enabled fleet, you must use the DNS name, not the IP address.
--
-- * 'gsciPort' - Port number for the game session. To connect to a Amazon GameLift game server, an app needs both the IP address and port number.
gameSessionConnectionInfo ::
  GameSessionConnectionInfo
gameSessionConnectionInfo =
  GameSessionConnectionInfo'
    { _gsciMatchedPlayerSessions = Nothing,
      _gsciIPAddress = Nothing,
      _gsciGameSessionARN = Nothing,
      _gsciDNSName = Nothing,
      _gsciPort = Nothing
    }

-- | A collection of player session IDs, one for each player ID that was included in the original matchmaking request.
gsciMatchedPlayerSessions :: Lens' GameSessionConnectionInfo [MatchedPlayerSession]
gsciMatchedPlayerSessions = lens _gsciMatchedPlayerSessions (\s a -> s {_gsciMatchedPlayerSessions = a}) . _Default . _Coerce

-- | IP address of the instance that is running the game session. When connecting to a Amazon GameLift game server, a client needs to reference an IP address (or DNS name) and port number.
gsciIPAddress :: Lens' GameSessionConnectionInfo (Maybe Text)
gsciIPAddress = lens _gsciIPAddress (\s a -> s {_gsciIPAddress = a})

-- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a game session and uniquely identifies it.
gsciGameSessionARN :: Lens' GameSessionConnectionInfo (Maybe Text)
gsciGameSessionARN = lens _gsciGameSessionARN (\s a -> s {_gsciGameSessionARN = a})

-- | DNS identifier assigned to the instance that is running the game session. Values have the following format:     * TLS-enabled fleets: @<unique identifier>.<region identifier>.amazongamelift.com@ .     * Non-TLS-enabled fleets: @ec2-<unique identifier>.compute.amazonaws.com@ . (See <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-instance-addressing.html#concepts-public-addresses Amazon EC2 Instance IP Addressing> .) When connecting to a game session that is running on a TLS-enabled fleet, you must use the DNS name, not the IP address.
gsciDNSName :: Lens' GameSessionConnectionInfo (Maybe Text)
gsciDNSName = lens _gsciDNSName (\s a -> s {_gsciDNSName = a})

-- | Port number for the game session. To connect to a Amazon GameLift game server, an app needs both the IP address and port number.
gsciPort :: Lens' GameSessionConnectionInfo (Maybe Natural)
gsciPort = lens _gsciPort (\s a -> s {_gsciPort = a}) . mapping _Nat

instance FromJSON GameSessionConnectionInfo where
  parseJSON =
    withObject
      "GameSessionConnectionInfo"
      ( \x ->
          GameSessionConnectionInfo'
            <$> (x .:? "MatchedPlayerSessions" .!= mempty)
            <*> (x .:? "IpAddress")
            <*> (x .:? "GameSessionArn")
            <*> (x .:? "DnsName")
            <*> (x .:? "Port")
      )

instance Hashable GameSessionConnectionInfo

instance NFData GameSessionConnectionInfo
