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
-- Module      : Amazonka.CloudWatchEvents.Types.Connection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.Connection where

import Amazonka.CloudWatchEvents.Types.ConnectionAuthorizationType
import Amazonka.CloudWatchEvents.Types.ConnectionState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a connection.
--
-- /See:/ 'newConnection' smart constructor.
data Connection = Connection'
  { -- | The name of the connection.
    name :: Prelude.Maybe Prelude.Text,
    -- | The state of the connection.
    connectionState :: Prelude.Maybe ConnectionState,
    -- | The ARN of the connection.
    connectionArn :: Prelude.Maybe Prelude.Text,
    -- | A time stamp for the time that the connection was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | A time stamp for the time that the connection was last authorized.
    lastAuthorizedTime :: Prelude.Maybe Data.POSIX,
    -- | A time stamp for the time that the connection was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The authorization type specified for the connection.
    authorizationType :: Prelude.Maybe ConnectionAuthorizationType,
    -- | The reason that the connection is in the connection state.
    stateReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Connection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'connection_name' - The name of the connection.
--
-- 'connectionState', 'connection_connectionState' - The state of the connection.
--
-- 'connectionArn', 'connection_connectionArn' - The ARN of the connection.
--
-- 'lastModifiedTime', 'connection_lastModifiedTime' - A time stamp for the time that the connection was last modified.
--
-- 'lastAuthorizedTime', 'connection_lastAuthorizedTime' - A time stamp for the time that the connection was last authorized.
--
-- 'creationTime', 'connection_creationTime' - A time stamp for the time that the connection was created.
--
-- 'authorizationType', 'connection_authorizationType' - The authorization type specified for the connection.
--
-- 'stateReason', 'connection_stateReason' - The reason that the connection is in the connection state.
newConnection ::
  Connection
newConnection =
  Connection'
    { name = Prelude.Nothing,
      connectionState = Prelude.Nothing,
      connectionArn = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      lastAuthorizedTime = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      authorizationType = Prelude.Nothing,
      stateReason = Prelude.Nothing
    }

-- | The name of the connection.
connection_name :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_name = Lens.lens (\Connection' {name} -> name) (\s@Connection' {} a -> s {name = a} :: Connection)

-- | The state of the connection.
connection_connectionState :: Lens.Lens' Connection (Prelude.Maybe ConnectionState)
connection_connectionState = Lens.lens (\Connection' {connectionState} -> connectionState) (\s@Connection' {} a -> s {connectionState = a} :: Connection)

-- | The ARN of the connection.
connection_connectionArn :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_connectionArn = Lens.lens (\Connection' {connectionArn} -> connectionArn) (\s@Connection' {} a -> s {connectionArn = a} :: Connection)

-- | A time stamp for the time that the connection was last modified.
connection_lastModifiedTime :: Lens.Lens' Connection (Prelude.Maybe Prelude.UTCTime)
connection_lastModifiedTime = Lens.lens (\Connection' {lastModifiedTime} -> lastModifiedTime) (\s@Connection' {} a -> s {lastModifiedTime = a} :: Connection) Prelude.. Lens.mapping Data._Time

-- | A time stamp for the time that the connection was last authorized.
connection_lastAuthorizedTime :: Lens.Lens' Connection (Prelude.Maybe Prelude.UTCTime)
connection_lastAuthorizedTime = Lens.lens (\Connection' {lastAuthorizedTime} -> lastAuthorizedTime) (\s@Connection' {} a -> s {lastAuthorizedTime = a} :: Connection) Prelude.. Lens.mapping Data._Time

-- | A time stamp for the time that the connection was created.
connection_creationTime :: Lens.Lens' Connection (Prelude.Maybe Prelude.UTCTime)
connection_creationTime = Lens.lens (\Connection' {creationTime} -> creationTime) (\s@Connection' {} a -> s {creationTime = a} :: Connection) Prelude.. Lens.mapping Data._Time

-- | The authorization type specified for the connection.
connection_authorizationType :: Lens.Lens' Connection (Prelude.Maybe ConnectionAuthorizationType)
connection_authorizationType = Lens.lens (\Connection' {authorizationType} -> authorizationType) (\s@Connection' {} a -> s {authorizationType = a} :: Connection)

-- | The reason that the connection is in the connection state.
connection_stateReason :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_stateReason = Lens.lens (\Connection' {stateReason} -> stateReason) (\s@Connection' {} a -> s {stateReason = a} :: Connection)

instance Data.FromJSON Connection where
  parseJSON =
    Data.withObject
      "Connection"
      ( \x ->
          Connection'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ConnectionState")
            Prelude.<*> (x Data..:? "ConnectionArn")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "LastAuthorizedTime")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "AuthorizationType")
            Prelude.<*> (x Data..:? "StateReason")
      )

instance Prelude.Hashable Connection where
  hashWithSalt _salt Connection' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` connectionState
      `Prelude.hashWithSalt` connectionArn
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` lastAuthorizedTime
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` authorizationType
      `Prelude.hashWithSalt` stateReason

instance Prelude.NFData Connection where
  rnf Connection' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf connectionState
      `Prelude.seq` Prelude.rnf connectionArn
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf lastAuthorizedTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf authorizationType
      `Prelude.seq` Prelude.rnf stateReason
