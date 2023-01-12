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
-- Module      : Amazonka.GameLift.Types.GameSessionConnectionInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.GameSessionConnectionInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types.MatchedPlayerSession
import qualified Amazonka.Prelude as Prelude

-- | Connection information for a new game session that is created in
-- response to a start matchmaking request. Once a match is made, the
-- FlexMatch engine creates a new game session for it. This information,
-- including the game session endpoint and player sessions for each player
-- in the original matchmaking request, is added to the matchmaking ticket.
--
-- /See:/ 'newGameSessionConnectionInfo' smart constructor.
data GameSessionConnectionInfo = GameSessionConnectionInfo'
  { -- | The DNS identifier assigned to the instance that is running the game
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
    -- | A unique identifier for the game session. Use the game session ID.
    gameSessionArn :: Prelude.Maybe Prelude.Text,
    -- | The IP address of the game session. To connect to a GameLift game
    -- server, an app needs both the IP address and port number.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | A collection of player session IDs, one for each player ID that was
    -- included in the original matchmaking request.
    matchedPlayerSessions :: Prelude.Maybe [MatchedPlayerSession],
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
-- 'gameSessionArn', 'gameSessionConnectionInfo_gameSessionArn' - A unique identifier for the game session. Use the game session ID.
--
-- 'ipAddress', 'gameSessionConnectionInfo_ipAddress' - The IP address of the game session. To connect to a GameLift game
-- server, an app needs both the IP address and port number.
--
-- 'matchedPlayerSessions', 'gameSessionConnectionInfo_matchedPlayerSessions' - A collection of player session IDs, one for each player ID that was
-- included in the original matchmaking request.
--
-- 'port', 'gameSessionConnectionInfo_port' - The port number for the game session. To connect to a GameLift game
-- server, an app needs both the IP address and port number.
newGameSessionConnectionInfo ::
  GameSessionConnectionInfo
newGameSessionConnectionInfo =
  GameSessionConnectionInfo'
    { dnsName =
        Prelude.Nothing,
      gameSessionArn = Prelude.Nothing,
      ipAddress = Prelude.Nothing,
      matchedPlayerSessions = Prelude.Nothing,
      port = Prelude.Nothing
    }

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

-- | A unique identifier for the game session. Use the game session ID.
gameSessionConnectionInfo_gameSessionArn :: Lens.Lens' GameSessionConnectionInfo (Prelude.Maybe Prelude.Text)
gameSessionConnectionInfo_gameSessionArn = Lens.lens (\GameSessionConnectionInfo' {gameSessionArn} -> gameSessionArn) (\s@GameSessionConnectionInfo' {} a -> s {gameSessionArn = a} :: GameSessionConnectionInfo)

-- | The IP address of the game session. To connect to a GameLift game
-- server, an app needs both the IP address and port number.
gameSessionConnectionInfo_ipAddress :: Lens.Lens' GameSessionConnectionInfo (Prelude.Maybe Prelude.Text)
gameSessionConnectionInfo_ipAddress = Lens.lens (\GameSessionConnectionInfo' {ipAddress} -> ipAddress) (\s@GameSessionConnectionInfo' {} a -> s {ipAddress = a} :: GameSessionConnectionInfo)

-- | A collection of player session IDs, one for each player ID that was
-- included in the original matchmaking request.
gameSessionConnectionInfo_matchedPlayerSessions :: Lens.Lens' GameSessionConnectionInfo (Prelude.Maybe [MatchedPlayerSession])
gameSessionConnectionInfo_matchedPlayerSessions = Lens.lens (\GameSessionConnectionInfo' {matchedPlayerSessions} -> matchedPlayerSessions) (\s@GameSessionConnectionInfo' {} a -> s {matchedPlayerSessions = a} :: GameSessionConnectionInfo) Prelude.. Lens.mapping Lens.coerced

-- | The port number for the game session. To connect to a GameLift game
-- server, an app needs both the IP address and port number.
gameSessionConnectionInfo_port :: Lens.Lens' GameSessionConnectionInfo (Prelude.Maybe Prelude.Natural)
gameSessionConnectionInfo_port = Lens.lens (\GameSessionConnectionInfo' {port} -> port) (\s@GameSessionConnectionInfo' {} a -> s {port = a} :: GameSessionConnectionInfo)

instance Data.FromJSON GameSessionConnectionInfo where
  parseJSON =
    Data.withObject
      "GameSessionConnectionInfo"
      ( \x ->
          GameSessionConnectionInfo'
            Prelude.<$> (x Data..:? "DnsName")
            Prelude.<*> (x Data..:? "GameSessionArn")
            Prelude.<*> (x Data..:? "IpAddress")
            Prelude.<*> ( x Data..:? "MatchedPlayerSessions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Port")
      )

instance Prelude.Hashable GameSessionConnectionInfo where
  hashWithSalt _salt GameSessionConnectionInfo' {..} =
    _salt `Prelude.hashWithSalt` dnsName
      `Prelude.hashWithSalt` gameSessionArn
      `Prelude.hashWithSalt` ipAddress
      `Prelude.hashWithSalt` matchedPlayerSessions
      `Prelude.hashWithSalt` port

instance Prelude.NFData GameSessionConnectionInfo where
  rnf GameSessionConnectionInfo' {..} =
    Prelude.rnf dnsName
      `Prelude.seq` Prelude.rnf gameSessionArn
      `Prelude.seq` Prelude.rnf ipAddress
      `Prelude.seq` Prelude.rnf matchedPlayerSessions
      `Prelude.seq` Prelude.rnf port
