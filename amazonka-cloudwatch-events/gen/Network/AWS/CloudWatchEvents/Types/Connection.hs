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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about a connection.
--
-- /See:/ 'newConnection' smart constructor.
data Connection = Connection'
  { -- | A time stamp for the time that the connection was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The state of the connection.
    connectionState :: Core.Maybe ConnectionState,
    -- | The reason that the connection is in the connection state.
    stateReason :: Core.Maybe Core.Text,
    -- | The authorization type specified for the connection.
    authorizationType :: Core.Maybe ConnectionAuthorizationType,
    -- | The ARN of the connection.
    connectionArn :: Core.Maybe Core.Text,
    -- | The name of the connection.
    name :: Core.Maybe Core.Text,
    -- | A time stamp for the time that the connection was last modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | A time stamp for the time that the connection was last authorized.
    lastAuthorizedTime :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { creationTime = Core.Nothing,
      connectionState = Core.Nothing,
      stateReason = Core.Nothing,
      authorizationType = Core.Nothing,
      connectionArn = Core.Nothing,
      name = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      lastAuthorizedTime = Core.Nothing
    }

-- | A time stamp for the time that the connection was created.
connection_creationTime :: Lens.Lens' Connection (Core.Maybe Core.UTCTime)
connection_creationTime = Lens.lens (\Connection' {creationTime} -> creationTime) (\s@Connection' {} a -> s {creationTime = a} :: Connection) Core.. Lens.mapping Core._Time

-- | The state of the connection.
connection_connectionState :: Lens.Lens' Connection (Core.Maybe ConnectionState)
connection_connectionState = Lens.lens (\Connection' {connectionState} -> connectionState) (\s@Connection' {} a -> s {connectionState = a} :: Connection)

-- | The reason that the connection is in the connection state.
connection_stateReason :: Lens.Lens' Connection (Core.Maybe Core.Text)
connection_stateReason = Lens.lens (\Connection' {stateReason} -> stateReason) (\s@Connection' {} a -> s {stateReason = a} :: Connection)

-- | The authorization type specified for the connection.
connection_authorizationType :: Lens.Lens' Connection (Core.Maybe ConnectionAuthorizationType)
connection_authorizationType = Lens.lens (\Connection' {authorizationType} -> authorizationType) (\s@Connection' {} a -> s {authorizationType = a} :: Connection)

-- | The ARN of the connection.
connection_connectionArn :: Lens.Lens' Connection (Core.Maybe Core.Text)
connection_connectionArn = Lens.lens (\Connection' {connectionArn} -> connectionArn) (\s@Connection' {} a -> s {connectionArn = a} :: Connection)

-- | The name of the connection.
connection_name :: Lens.Lens' Connection (Core.Maybe Core.Text)
connection_name = Lens.lens (\Connection' {name} -> name) (\s@Connection' {} a -> s {name = a} :: Connection)

-- | A time stamp for the time that the connection was last modified.
connection_lastModifiedTime :: Lens.Lens' Connection (Core.Maybe Core.UTCTime)
connection_lastModifiedTime = Lens.lens (\Connection' {lastModifiedTime} -> lastModifiedTime) (\s@Connection' {} a -> s {lastModifiedTime = a} :: Connection) Core.. Lens.mapping Core._Time

-- | A time stamp for the time that the connection was last authorized.
connection_lastAuthorizedTime :: Lens.Lens' Connection (Core.Maybe Core.UTCTime)
connection_lastAuthorizedTime = Lens.lens (\Connection' {lastAuthorizedTime} -> lastAuthorizedTime) (\s@Connection' {} a -> s {lastAuthorizedTime = a} :: Connection) Core.. Lens.mapping Core._Time

instance Core.FromJSON Connection where
  parseJSON =
    Core.withObject
      "Connection"
      ( \x ->
          Connection'
            Core.<$> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "ConnectionState")
            Core.<*> (x Core..:? "StateReason")
            Core.<*> (x Core..:? "AuthorizationType")
            Core.<*> (x Core..:? "ConnectionArn")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "LastAuthorizedTime")
      )

instance Core.Hashable Connection

instance Core.NFData Connection
