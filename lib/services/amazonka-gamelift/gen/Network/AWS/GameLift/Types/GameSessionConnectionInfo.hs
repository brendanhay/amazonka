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

import qualified Network.AWS.Core as Core
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
  { -- | A collection of player session IDs, one for each player ID that was
    -- included in the original matchmaking request.
    matchedPlayerSessions :: Prelude.Maybe [MatchedPlayerSession],
    -- | The IP address of the game session. To connect to a GameLift game
    -- server, an app needs both the IP address and port number.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the game session. Use the game session ID.
    gameSessionArn :: Prelude.Maybe Prelude.Text,
    -- | The DNS identifier assigned to the instance that is running the game
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
    dnsName :: Prelude.Maybe Prelude.Text,
    -- | The port number for the game session. To connect to a GameLift game
    -- server, an app needs both the IP address and port number.
    port :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GameSessionConnectionInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'matchedPlayerSessions', 'gameSessionConnectionInfo_matchedPlayerSessions' - A collection of player session IDs, one for each player ID that was
-- included in the original matchmaking request.
--
-- 'ipAddress', 'gameSessionConnectionInfo_ipAddress' - The IP address of the game session. To connect to a GameLift game
-- server, an app needs both the IP address and port number.
--
-- 'gameSessionArn', 'gameSessionConnectionInfo_gameSessionArn' - A unique identifier for the game session. Use the game session ID.
--
-- 'dnsName', 'gameSessionConnectionInfo_dnsName' - The DNS identifier assigned to the instance that is running the game
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
--
-- 'port', 'gameSessionConnectionInfo_port' - The port number for the game session. To connect to a GameLift game
-- server, an app needs both the IP address and port number.
newGameSessionConnectionInfo ::
  GameSessionConnectionInfo
newGameSessionConnectionInfo =
  GameSessionConnectionInfo'
    { matchedPlayerSessions =
        Prelude.Nothing,
      ipAddress = Prelude.Nothing,
      gameSessionArn = Prelude.Nothing,
      dnsName = Prelude.Nothing,
      port = Prelude.Nothing
    }

-- | A collection of player session IDs, one for each player ID that was
-- included in the original matchmaking request.
gameSessionConnectionInfo_matchedPlayerSessions :: Lens.Lens' GameSessionConnectionInfo (Prelude.Maybe [MatchedPlayerSession])
gameSessionConnectionInfo_matchedPlayerSessions = Lens.lens (\GameSessionConnectionInfo' {matchedPlayerSessions} -> matchedPlayerSessions) (\s@GameSessionConnectionInfo' {} a -> s {matchedPlayerSessions = a} :: GameSessionConnectionInfo) Prelude.. Lens.mapping Lens.coerced

-- | The IP address of the game session. To connect to a GameLift game
-- server, an app needs both the IP address and port number.
gameSessionConnectionInfo_ipAddress :: Lens.Lens' GameSessionConnectionInfo (Prelude.Maybe Prelude.Text)
gameSessionConnectionInfo_ipAddress = Lens.lens (\GameSessionConnectionInfo' {ipAddress} -> ipAddress) (\s@GameSessionConnectionInfo' {} a -> s {ipAddress = a} :: GameSessionConnectionInfo)

-- | A unique identifier for the game session. Use the game session ID.
gameSessionConnectionInfo_gameSessionArn :: Lens.Lens' GameSessionConnectionInfo (Prelude.Maybe Prelude.Text)
gameSessionConnectionInfo_gameSessionArn = Lens.lens (\GameSessionConnectionInfo' {gameSessionArn} -> gameSessionArn) (\s@GameSessionConnectionInfo' {} a -> s {gameSessionArn = a} :: GameSessionConnectionInfo)

-- | The DNS identifier assigned to the instance that is running the game
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

-- | The port number for the game session. To connect to a GameLift game
-- server, an app needs both the IP address and port number.
gameSessionConnectionInfo_port :: Lens.Lens' GameSessionConnectionInfo (Prelude.Maybe Prelude.Natural)
gameSessionConnectionInfo_port = Lens.lens (\GameSessionConnectionInfo' {port} -> port) (\s@GameSessionConnectionInfo' {} a -> s {port = a} :: GameSessionConnectionInfo)

instance Core.FromJSON GameSessionConnectionInfo where
  parseJSON =
    Core.withObject
      "GameSessionConnectionInfo"
      ( \x ->
          GameSessionConnectionInfo'
            Prelude.<$> ( x Core..:? "MatchedPlayerSessions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "IpAddress")
            Prelude.<*> (x Core..:? "GameSessionArn")
            Prelude.<*> (x Core..:? "DnsName")
            Prelude.<*> (x Core..:? "Port")
      )

instance Prelude.Hashable GameSessionConnectionInfo

instance Prelude.NFData GameSessionConnectionInfo
