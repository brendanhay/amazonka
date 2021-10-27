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
-- Module      : Network.AWS.AppRunner.Types.Connection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppRunner.Types.Connection where

import Network.AWS.AppRunner.Types.ConnectionStatus
import Network.AWS.AppRunner.Types.ProviderType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an App Runner connection resource.
--
-- /See:/ 'newConnection' smart constructor.
data Connection = Connection'
  { -- | The current state of the App Runner connection. When the state is
    -- @AVAILABLE@, you can use the connection to create an App Runner service.
    status :: Prelude.Maybe ConnectionStatus,
    -- | The App Runner connection creation time, expressed as a Unix time stamp.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The source repository provider.
    providerType :: Prelude.Maybe ProviderType,
    -- | The customer-provided connection name.
    connectionName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of this connection.
    connectionArn :: Prelude.Maybe Prelude.Text
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
-- 'status', 'connection_status' - The current state of the App Runner connection. When the state is
-- @AVAILABLE@, you can use the connection to create an App Runner service.
--
-- 'createdAt', 'connection_createdAt' - The App Runner connection creation time, expressed as a Unix time stamp.
--
-- 'providerType', 'connection_providerType' - The source repository provider.
--
-- 'connectionName', 'connection_connectionName' - The customer-provided connection name.
--
-- 'connectionArn', 'connection_connectionArn' - The Amazon Resource Name (ARN) of this connection.
newConnection ::
  Connection
newConnection =
  Connection'
    { status = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      providerType = Prelude.Nothing,
      connectionName = Prelude.Nothing,
      connectionArn = Prelude.Nothing
    }

-- | The current state of the App Runner connection. When the state is
-- @AVAILABLE@, you can use the connection to create an App Runner service.
connection_status :: Lens.Lens' Connection (Prelude.Maybe ConnectionStatus)
connection_status = Lens.lens (\Connection' {status} -> status) (\s@Connection' {} a -> s {status = a} :: Connection)

-- | The App Runner connection creation time, expressed as a Unix time stamp.
connection_createdAt :: Lens.Lens' Connection (Prelude.Maybe Prelude.UTCTime)
connection_createdAt = Lens.lens (\Connection' {createdAt} -> createdAt) (\s@Connection' {} a -> s {createdAt = a} :: Connection) Prelude.. Lens.mapping Core._Time

-- | The source repository provider.
connection_providerType :: Lens.Lens' Connection (Prelude.Maybe ProviderType)
connection_providerType = Lens.lens (\Connection' {providerType} -> providerType) (\s@Connection' {} a -> s {providerType = a} :: Connection)

-- | The customer-provided connection name.
connection_connectionName :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_connectionName = Lens.lens (\Connection' {connectionName} -> connectionName) (\s@Connection' {} a -> s {connectionName = a} :: Connection)

-- | The Amazon Resource Name (ARN) of this connection.
connection_connectionArn :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_connectionArn = Lens.lens (\Connection' {connectionArn} -> connectionArn) (\s@Connection' {} a -> s {connectionArn = a} :: Connection)

instance Core.FromJSON Connection where
  parseJSON =
    Core.withObject
      "Connection"
      ( \x ->
          Connection'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "ProviderType")
            Prelude.<*> (x Core..:? "ConnectionName")
            Prelude.<*> (x Core..:? "ConnectionArn")
      )

instance Prelude.Hashable Connection

instance Prelude.NFData Connection
