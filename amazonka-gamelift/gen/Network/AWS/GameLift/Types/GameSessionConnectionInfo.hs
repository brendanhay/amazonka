{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameSessionConnectionInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameSessionConnectionInfo where

import Network.AWS.GameLift.Types.MatchedPlayerSession
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Connection information for a new game session that is created in
-- response to a StartMatchmaking request. Once a match is made, the
-- FlexMatch engine creates a new game session for it. This information,
-- including the game session endpoint and player sessions for each player
-- in the original matchmaking request, is added to the MatchmakingTicket,
-- which can be retrieved by calling DescribeMatchmaking.
--
-- /See:/ 'newGameSessionConnectionInfo' smart constructor.
data GameSessionConnectionInfo = GameSessionConnectionInfo'
  { -- | Amazon Resource Name
    -- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
    -- that is assigned to a game session and uniquely identifies it.
    gameSessionArn :: Prelude.Maybe Prelude.Text,
    -- | IP address of the instance that is running the game session. When
    -- connecting to a Amazon GameLift game server, a client needs to reference
    -- an IP address (or DNS name) and port number.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | A collection of player session IDs, one for each player ID that was
    -- included in the original matchmaking request.
    matchedPlayerSessions :: Prelude.Maybe [MatchedPlayerSession],
    -- | Port number for the game session. To connect to a Amazon GameLift game
    -- server, an app needs both the IP address and port number.
    port :: Prelude.Maybe Prelude.Natural,
    -- | DNS identifier assigned to the instance that is running the game
    -- session. Values have the following format:
    --
    -- -   TLS-enabled fleets:
    --     @\<unique identifier>.\<region identifier>.amazongamelift.com@.
    --
    -- -   Non-TLS-enabled fleets:
    --     @ec2-\<unique identifier>.compute.amazonaws.com@. (See
    --     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-instance-addressing.html#concepts-public-addresses Amazon EC2 Instance IP Addressing>.)
    --
    -- When connecting to a game session that is running on a TLS-enabled
    -- fleet, you must use the DNS name, not the IP address.
    dnsName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GameSessionConnectionInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameSessionArn', 'gameSessionConnectionInfo_gameSessionArn' - Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- that is assigned to a game session and uniquely identifies it.
--
-- 'ipAddress', 'gameSessionConnectionInfo_ipAddress' - IP address of the instance that is running the game session. When
-- connecting to a Amazon GameLift game server, a client needs to reference
-- an IP address (or DNS name) and port number.
--
-- 'matchedPlayerSessions', 'gameSessionConnectionInfo_matchedPlayerSessions' - A collection of player session IDs, one for each player ID that was
-- included in the original matchmaking request.
--
-- 'port', 'gameSessionConnectionInfo_port' - Port number for the game session. To connect to a Amazon GameLift game
-- server, an app needs both the IP address and port number.
--
-- 'dnsName', 'gameSessionConnectionInfo_dnsName' - DNS identifier assigned to the instance that is running the game
-- session. Values have the following format:
--
-- -   TLS-enabled fleets:
--     @\<unique identifier>.\<region identifier>.amazongamelift.com@.
--
-- -   Non-TLS-enabled fleets:
--     @ec2-\<unique identifier>.compute.amazonaws.com@. (See
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-instance-addressing.html#concepts-public-addresses Amazon EC2 Instance IP Addressing>.)
--
-- When connecting to a game session that is running on a TLS-enabled
-- fleet, you must use the DNS name, not the IP address.
newGameSessionConnectionInfo ::
  GameSessionConnectionInfo
newGameSessionConnectionInfo =
  GameSessionConnectionInfo'
    { gameSessionArn =
        Prelude.Nothing,
      ipAddress = Prelude.Nothing,
      matchedPlayerSessions = Prelude.Nothing,
      port = Prelude.Nothing,
      dnsName = Prelude.Nothing
    }

-- | Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- that is assigned to a game session and uniquely identifies it.
gameSessionConnectionInfo_gameSessionArn :: Lens.Lens' GameSessionConnectionInfo (Prelude.Maybe Prelude.Text)
gameSessionConnectionInfo_gameSessionArn = Lens.lens (\GameSessionConnectionInfo' {gameSessionArn} -> gameSessionArn) (\s@GameSessionConnectionInfo' {} a -> s {gameSessionArn = a} :: GameSessionConnectionInfo)

-- | IP address of the instance that is running the game session. When
-- connecting to a Amazon GameLift game server, a client needs to reference
-- an IP address (or DNS name) and port number.
gameSessionConnectionInfo_ipAddress :: Lens.Lens' GameSessionConnectionInfo (Prelude.Maybe Prelude.Text)
gameSessionConnectionInfo_ipAddress = Lens.lens (\GameSessionConnectionInfo' {ipAddress} -> ipAddress) (\s@GameSessionConnectionInfo' {} a -> s {ipAddress = a} :: GameSessionConnectionInfo)

-- | A collection of player session IDs, one for each player ID that was
-- included in the original matchmaking request.
gameSessionConnectionInfo_matchedPlayerSessions :: Lens.Lens' GameSessionConnectionInfo (Prelude.Maybe [MatchedPlayerSession])
gameSessionConnectionInfo_matchedPlayerSessions = Lens.lens (\GameSessionConnectionInfo' {matchedPlayerSessions} -> matchedPlayerSessions) (\s@GameSessionConnectionInfo' {} a -> s {matchedPlayerSessions = a} :: GameSessionConnectionInfo) Prelude.. Lens.mapping Prelude._Coerce

-- | Port number for the game session. To connect to a Amazon GameLift game
-- server, an app needs both the IP address and port number.
gameSessionConnectionInfo_port :: Lens.Lens' GameSessionConnectionInfo (Prelude.Maybe Prelude.Natural)
gameSessionConnectionInfo_port = Lens.lens (\GameSessionConnectionInfo' {port} -> port) (\s@GameSessionConnectionInfo' {} a -> s {port = a} :: GameSessionConnectionInfo)

-- | DNS identifier assigned to the instance that is running the game
-- session. Values have the following format:
--
-- -   TLS-enabled fleets:
--     @\<unique identifier>.\<region identifier>.amazongamelift.com@.
--
-- -   Non-TLS-enabled fleets:
--     @ec2-\<unique identifier>.compute.amazonaws.com@. (See
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-instance-addressing.html#concepts-public-addresses Amazon EC2 Instance IP Addressing>.)
--
-- When connecting to a game session that is running on a TLS-enabled
-- fleet, you must use the DNS name, not the IP address.
gameSessionConnectionInfo_dnsName :: Lens.Lens' GameSessionConnectionInfo (Prelude.Maybe Prelude.Text)
gameSessionConnectionInfo_dnsName = Lens.lens (\GameSessionConnectionInfo' {dnsName} -> dnsName) (\s@GameSessionConnectionInfo' {} a -> s {dnsName = a} :: GameSessionConnectionInfo)

instance Prelude.FromJSON GameSessionConnectionInfo where
  parseJSON =
    Prelude.withObject
      "GameSessionConnectionInfo"
      ( \x ->
          GameSessionConnectionInfo'
            Prelude.<$> (x Prelude..:? "GameSessionArn")
            Prelude.<*> (x Prelude..:? "IpAddress")
            Prelude.<*> ( x Prelude..:? "MatchedPlayerSessions"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Port")
            Prelude.<*> (x Prelude..:? "DnsName")
      )

instance Prelude.Hashable GameSessionConnectionInfo

instance Prelude.NFData GameSessionConnectionInfo
