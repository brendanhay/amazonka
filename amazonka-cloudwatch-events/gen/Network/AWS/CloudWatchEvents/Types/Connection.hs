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
-- Module      : Network.AWS.CloudWatchEvents.Types.Connection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.Connection where

import Network.AWS.CloudWatchEvents.Types.ConnectionAuthorizationType
import Network.AWS.CloudWatchEvents.Types.ConnectionState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a connection.
--
-- /See:/ 'newConnection' smart constructor.
data Connection = Connection'
  { -- | A time stamp for the time that the connection was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The state of the connection.
    connectionState :: Prelude.Maybe ConnectionState,
    -- | The reason that the connection is in the connection state.
    stateReason :: Prelude.Maybe Prelude.Text,
    -- | The authorization type specified for the connection.
    authorizationType :: Prelude.Maybe ConnectionAuthorizationType,
    -- | The ARN of the connection.
    connectionArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the connection.
    name :: Prelude.Maybe Prelude.Text,
    -- | A time stamp for the time that the connection was last modified.
    lastModifiedTime :: Prelude.Maybe Prelude.POSIX,
    -- | A time stamp for the time that the connection was last authorized.
    lastAuthorizedTime :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Connection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'connection_creationTime' - A time stamp for the time that the connection was created.
--
-- 'connectionState', 'connection_connectionState' - The state of the connection.
--
-- 'stateReason', 'connection_stateReason' - The reason that the connection is in the connection state.
--
-- 'authorizationType', 'connection_authorizationType' - The authorization type specified for the connection.
--
-- 'connectionArn', 'connection_connectionArn' - The ARN of the connection.
--
-- 'name', 'connection_name' - The name of the connection.
--
-- 'lastModifiedTime', 'connection_lastModifiedTime' - A time stamp for the time that the connection was last modified.
--
-- 'lastAuthorizedTime', 'connection_lastAuthorizedTime' - A time stamp for the time that the connection was last authorized.
newConnection ::
  Connection
newConnection =
  Connection'
    { creationTime = Prelude.Nothing,
      connectionState = Prelude.Nothing,
      stateReason = Prelude.Nothing,
      authorizationType = Prelude.Nothing,
      connectionArn = Prelude.Nothing,
      name = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      lastAuthorizedTime = Prelude.Nothing
    }

-- | A time stamp for the time that the connection was created.
connection_creationTime :: Lens.Lens' Connection (Prelude.Maybe Prelude.UTCTime)
connection_creationTime = Lens.lens (\Connection' {creationTime} -> creationTime) (\s@Connection' {} a -> s {creationTime = a} :: Connection) Prelude.. Lens.mapping Prelude._Time

-- | The state of the connection.
connection_connectionState :: Lens.Lens' Connection (Prelude.Maybe ConnectionState)
connection_connectionState = Lens.lens (\Connection' {connectionState} -> connectionState) (\s@Connection' {} a -> s {connectionState = a} :: Connection)

-- | The reason that the connection is in the connection state.
connection_stateReason :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_stateReason = Lens.lens (\Connection' {stateReason} -> stateReason) (\s@Connection' {} a -> s {stateReason = a} :: Connection)

-- | The authorization type specified for the connection.
connection_authorizationType :: Lens.Lens' Connection (Prelude.Maybe ConnectionAuthorizationType)
connection_authorizationType = Lens.lens (\Connection' {authorizationType} -> authorizationType) (\s@Connection' {} a -> s {authorizationType = a} :: Connection)

-- | The ARN of the connection.
connection_connectionArn :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_connectionArn = Lens.lens (\Connection' {connectionArn} -> connectionArn) (\s@Connection' {} a -> s {connectionArn = a} :: Connection)

-- | The name of the connection.
connection_name :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_name = Lens.lens (\Connection' {name} -> name) (\s@Connection' {} a -> s {name = a} :: Connection)

-- | A time stamp for the time that the connection was last modified.
connection_lastModifiedTime :: Lens.Lens' Connection (Prelude.Maybe Prelude.UTCTime)
connection_lastModifiedTime = Lens.lens (\Connection' {lastModifiedTime} -> lastModifiedTime) (\s@Connection' {} a -> s {lastModifiedTime = a} :: Connection) Prelude.. Lens.mapping Prelude._Time

-- | A time stamp for the time that the connection was last authorized.
connection_lastAuthorizedTime :: Lens.Lens' Connection (Prelude.Maybe Prelude.UTCTime)
connection_lastAuthorizedTime = Lens.lens (\Connection' {lastAuthorizedTime} -> lastAuthorizedTime) (\s@Connection' {} a -> s {lastAuthorizedTime = a} :: Connection) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON Connection where
  parseJSON =
    Prelude.withObject
      "Connection"
      ( \x ->
          Connection'
            Prelude.<$> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "ConnectionState")
            Prelude.<*> (x Prelude..:? "StateReason")
            Prelude.<*> (x Prelude..:? "AuthorizationType")
            Prelude.<*> (x Prelude..:? "ConnectionArn")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "LastModifiedTime")
            Prelude.<*> (x Prelude..:? "LastAuthorizedTime")
      )

instance Prelude.Hashable Connection

instance Prelude.NFData Connection
