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
-- Module      : Amazonka.GameLift.Types.PlayerSession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.PlayerSession where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types.PlayerSessionStatus
import qualified Amazonka.Prelude as Prelude

-- | Represents a player session. Player sessions are created either for a
-- specific game session, or as part of a game session placement or
-- matchmaking request. A player session can represents a reserved player
-- slot in a game session (when status is @RESERVED@) or actual player
-- activity in a game session (when status is @ACTIVE@). A player session
-- object, including player data, is automatically passed to a game session
-- when the player connects to the game session and is validated. After the
-- game session ends, player sessions information is retained for 30 days
-- and then removed.
--
-- __Related actions__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
--
-- /See:/ 'newPlayerSession' smart constructor.
data PlayerSession = PlayerSession'
  { -- | A time stamp indicating when this data object was created. Format is a
    -- number expressed in Unix time as milliseconds (for example
    -- @\"1469498468.057\"@).
    creationTime :: Prelude.Maybe Data.POSIX,
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
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- associated with the GameLift fleet that the player\'s game session is
    -- running on.
    fleetArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the fleet that the player\'s game session is
    -- running on.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the game session that the player session is
    -- connected to.
    gameSessionId :: Prelude.Maybe Prelude.Text,
    -- | The IP address of the game session. To connect to a GameLift game
    -- server, an app needs both the IP address and port number.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | Developer-defined information related to a player. GameLift does not use
    -- this data, so it can be formatted as needed for use in the game.
    playerData :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a player that is associated with this player
    -- session.
    playerId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a player session.
    playerSessionId :: Prelude.Maybe Prelude.Text,
    -- | Port number for the game session. To connect to a Amazon GameLift server
    -- process, an app needs both the IP address and port number.
    port :: Prelude.Maybe Prelude.Natural,
    -- | Current status of the player session.
    --
    -- Possible player session statuses include the following:
    --
    -- -   __RESERVED__ -- The player session request has been received, but
    --     the player has not yet connected to the server process and\/or been
    --     validated.
    --
    -- -   __ACTIVE__ -- The player has been validated by the server process
    --     and is currently connected.
    --
    -- -   __COMPLETED__ -- The player connection has been dropped.
    --
    -- -   __TIMEDOUT__ -- A player session request was received, but the
    --     player did not connect and\/or was not validated within the timeout
    --     limit (60 seconds).
    status :: Prelude.Maybe PlayerSessionStatus,
    -- | A time stamp indicating when this data object was terminated. Format is
    -- a number expressed in Unix time as milliseconds (for example
    -- @\"1469498468.057\"@).
    terminationTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PlayerSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'playerSession_creationTime' - A time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
--
-- 'dnsName', 'playerSession_dnsName' - The DNS identifier assigned to the instance that is running the game
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
-- 'fleetArn', 'playerSession_fleetArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- associated with the GameLift fleet that the player\'s game session is
-- running on.
--
-- 'fleetId', 'playerSession_fleetId' - A unique identifier for the fleet that the player\'s game session is
-- running on.
--
-- 'gameSessionId', 'playerSession_gameSessionId' - A unique identifier for the game session that the player session is
-- connected to.
--
-- 'ipAddress', 'playerSession_ipAddress' - The IP address of the game session. To connect to a GameLift game
-- server, an app needs both the IP address and port number.
--
-- 'playerData', 'playerSession_playerData' - Developer-defined information related to a player. GameLift does not use
-- this data, so it can be formatted as needed for use in the game.
--
-- 'playerId', 'playerSession_playerId' - A unique identifier for a player that is associated with this player
-- session.
--
-- 'playerSessionId', 'playerSession_playerSessionId' - A unique identifier for a player session.
--
-- 'port', 'playerSession_port' - Port number for the game session. To connect to a Amazon GameLift server
-- process, an app needs both the IP address and port number.
--
-- 'status', 'playerSession_status' - Current status of the player session.
--
-- Possible player session statuses include the following:
--
-- -   __RESERVED__ -- The player session request has been received, but
--     the player has not yet connected to the server process and\/or been
--     validated.
--
-- -   __ACTIVE__ -- The player has been validated by the server process
--     and is currently connected.
--
-- -   __COMPLETED__ -- The player connection has been dropped.
--
-- -   __TIMEDOUT__ -- A player session request was received, but the
--     player did not connect and\/or was not validated within the timeout
--     limit (60 seconds).
--
-- 'terminationTime', 'playerSession_terminationTime' - A time stamp indicating when this data object was terminated. Format is
-- a number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
newPlayerSession ::
  PlayerSession
newPlayerSession =
  PlayerSession'
    { creationTime = Prelude.Nothing,
      dnsName = Prelude.Nothing,
      fleetArn = Prelude.Nothing,
      fleetId = Prelude.Nothing,
      gameSessionId = Prelude.Nothing,
      ipAddress = Prelude.Nothing,
      playerData = Prelude.Nothing,
      playerId = Prelude.Nothing,
      playerSessionId = Prelude.Nothing,
      port = Prelude.Nothing,
      status = Prelude.Nothing,
      terminationTime = Prelude.Nothing
    }

-- | A time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
playerSession_creationTime :: Lens.Lens' PlayerSession (Prelude.Maybe Prelude.UTCTime)
playerSession_creationTime = Lens.lens (\PlayerSession' {creationTime} -> creationTime) (\s@PlayerSession' {} a -> s {creationTime = a} :: PlayerSession) Prelude.. Lens.mapping Data._Time

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
playerSession_dnsName :: Lens.Lens' PlayerSession (Prelude.Maybe Prelude.Text)
playerSession_dnsName = Lens.lens (\PlayerSession' {dnsName} -> dnsName) (\s@PlayerSession' {} a -> s {dnsName = a} :: PlayerSession)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- associated with the GameLift fleet that the player\'s game session is
-- running on.
playerSession_fleetArn :: Lens.Lens' PlayerSession (Prelude.Maybe Prelude.Text)
playerSession_fleetArn = Lens.lens (\PlayerSession' {fleetArn} -> fleetArn) (\s@PlayerSession' {} a -> s {fleetArn = a} :: PlayerSession)

-- | A unique identifier for the fleet that the player\'s game session is
-- running on.
playerSession_fleetId :: Lens.Lens' PlayerSession (Prelude.Maybe Prelude.Text)
playerSession_fleetId = Lens.lens (\PlayerSession' {fleetId} -> fleetId) (\s@PlayerSession' {} a -> s {fleetId = a} :: PlayerSession)

-- | A unique identifier for the game session that the player session is
-- connected to.
playerSession_gameSessionId :: Lens.Lens' PlayerSession (Prelude.Maybe Prelude.Text)
playerSession_gameSessionId = Lens.lens (\PlayerSession' {gameSessionId} -> gameSessionId) (\s@PlayerSession' {} a -> s {gameSessionId = a} :: PlayerSession)

-- | The IP address of the game session. To connect to a GameLift game
-- server, an app needs both the IP address and port number.
playerSession_ipAddress :: Lens.Lens' PlayerSession (Prelude.Maybe Prelude.Text)
playerSession_ipAddress = Lens.lens (\PlayerSession' {ipAddress} -> ipAddress) (\s@PlayerSession' {} a -> s {ipAddress = a} :: PlayerSession)

-- | Developer-defined information related to a player. GameLift does not use
-- this data, so it can be formatted as needed for use in the game.
playerSession_playerData :: Lens.Lens' PlayerSession (Prelude.Maybe Prelude.Text)
playerSession_playerData = Lens.lens (\PlayerSession' {playerData} -> playerData) (\s@PlayerSession' {} a -> s {playerData = a} :: PlayerSession)

-- | A unique identifier for a player that is associated with this player
-- session.
playerSession_playerId :: Lens.Lens' PlayerSession (Prelude.Maybe Prelude.Text)
playerSession_playerId = Lens.lens (\PlayerSession' {playerId} -> playerId) (\s@PlayerSession' {} a -> s {playerId = a} :: PlayerSession)

-- | A unique identifier for a player session.
playerSession_playerSessionId :: Lens.Lens' PlayerSession (Prelude.Maybe Prelude.Text)
playerSession_playerSessionId = Lens.lens (\PlayerSession' {playerSessionId} -> playerSessionId) (\s@PlayerSession' {} a -> s {playerSessionId = a} :: PlayerSession)

-- | Port number for the game session. To connect to a Amazon GameLift server
-- process, an app needs both the IP address and port number.
playerSession_port :: Lens.Lens' PlayerSession (Prelude.Maybe Prelude.Natural)
playerSession_port = Lens.lens (\PlayerSession' {port} -> port) (\s@PlayerSession' {} a -> s {port = a} :: PlayerSession)

-- | Current status of the player session.
--
-- Possible player session statuses include the following:
--
-- -   __RESERVED__ -- The player session request has been received, but
--     the player has not yet connected to the server process and\/or been
--     validated.
--
-- -   __ACTIVE__ -- The player has been validated by the server process
--     and is currently connected.
--
-- -   __COMPLETED__ -- The player connection has been dropped.
--
-- -   __TIMEDOUT__ -- A player session request was received, but the
--     player did not connect and\/or was not validated within the timeout
--     limit (60 seconds).
playerSession_status :: Lens.Lens' PlayerSession (Prelude.Maybe PlayerSessionStatus)
playerSession_status = Lens.lens (\PlayerSession' {status} -> status) (\s@PlayerSession' {} a -> s {status = a} :: PlayerSession)

-- | A time stamp indicating when this data object was terminated. Format is
-- a number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
playerSession_terminationTime :: Lens.Lens' PlayerSession (Prelude.Maybe Prelude.UTCTime)
playerSession_terminationTime = Lens.lens (\PlayerSession' {terminationTime} -> terminationTime) (\s@PlayerSession' {} a -> s {terminationTime = a} :: PlayerSession) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON PlayerSession where
  parseJSON =
    Data.withObject
      "PlayerSession"
      ( \x ->
          PlayerSession'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "DnsName")
            Prelude.<*> (x Data..:? "FleetArn")
            Prelude.<*> (x Data..:? "FleetId")
            Prelude.<*> (x Data..:? "GameSessionId")
            Prelude.<*> (x Data..:? "IpAddress")
            Prelude.<*> (x Data..:? "PlayerData")
            Prelude.<*> (x Data..:? "PlayerId")
            Prelude.<*> (x Data..:? "PlayerSessionId")
            Prelude.<*> (x Data..:? "Port")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "TerminationTime")
      )

instance Prelude.Hashable PlayerSession where
  hashWithSalt _salt PlayerSession' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` dnsName
      `Prelude.hashWithSalt` fleetArn
      `Prelude.hashWithSalt` fleetId
      `Prelude.hashWithSalt` gameSessionId
      `Prelude.hashWithSalt` ipAddress
      `Prelude.hashWithSalt` playerData
      `Prelude.hashWithSalt` playerId
      `Prelude.hashWithSalt` playerSessionId
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` terminationTime

instance Prelude.NFData PlayerSession where
  rnf PlayerSession' {..} =
    Prelude.rnf creationTime `Prelude.seq`
      Prelude.rnf dnsName `Prelude.seq`
        Prelude.rnf fleetArn `Prelude.seq`
          Prelude.rnf fleetId `Prelude.seq`
            Prelude.rnf gameSessionId `Prelude.seq`
              Prelude.rnf ipAddress `Prelude.seq`
                Prelude.rnf playerData `Prelude.seq`
                  Prelude.rnf playerId `Prelude.seq`
                    Prelude.rnf playerSessionId `Prelude.seq`
                      Prelude.rnf port `Prelude.seq`
                        Prelude.rnf status `Prelude.seq`
                          Prelude.rnf terminationTime
