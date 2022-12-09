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
-- Module      : Amazonka.AppRunner.Types.Connection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.Connection where

import Amazonka.AppRunner.Types.ConnectionStatus
import Amazonka.AppRunner.Types.ProviderType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an App Runner connection resource.
--
-- /See:/ 'newConnection' smart constructor.
data Connection = Connection'
  { -- | The Amazon Resource Name (ARN) of this connection.
    connectionArn :: Prelude.Maybe Prelude.Text,
    -- | The customer-provided connection name.
    connectionName :: Prelude.Maybe Prelude.Text,
    -- | The App Runner connection creation time, expressed as a Unix time stamp.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The source repository provider.
    providerType :: Prelude.Maybe ProviderType,
    -- | The current state of the App Runner connection. When the state is
    -- @AVAILABLE@, you can use the connection to create an App Runner service.
    status :: Prelude.Maybe ConnectionStatus
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
-- 'connectionArn', 'connection_connectionArn' - The Amazon Resource Name (ARN) of this connection.
--
-- 'connectionName', 'connection_connectionName' - The customer-provided connection name.
--
-- 'createdAt', 'connection_createdAt' - The App Runner connection creation time, expressed as a Unix time stamp.
--
-- 'providerType', 'connection_providerType' - The source repository provider.
--
-- 'status', 'connection_status' - The current state of the App Runner connection. When the state is
-- @AVAILABLE@, you can use the connection to create an App Runner service.
newConnection ::
  Connection
newConnection =
  Connection'
    { connectionArn = Prelude.Nothing,
      connectionName = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      providerType = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of this connection.
connection_connectionArn :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_connectionArn = Lens.lens (\Connection' {connectionArn} -> connectionArn) (\s@Connection' {} a -> s {connectionArn = a} :: Connection)

-- | The customer-provided connection name.
connection_connectionName :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_connectionName = Lens.lens (\Connection' {connectionName} -> connectionName) (\s@Connection' {} a -> s {connectionName = a} :: Connection)

-- | The App Runner connection creation time, expressed as a Unix time stamp.
connection_createdAt :: Lens.Lens' Connection (Prelude.Maybe Prelude.UTCTime)
connection_createdAt = Lens.lens (\Connection' {createdAt} -> createdAt) (\s@Connection' {} a -> s {createdAt = a} :: Connection) Prelude.. Lens.mapping Data._Time

-- | The source repository provider.
connection_providerType :: Lens.Lens' Connection (Prelude.Maybe ProviderType)
connection_providerType = Lens.lens (\Connection' {providerType} -> providerType) (\s@Connection' {} a -> s {providerType = a} :: Connection)

-- | The current state of the App Runner connection. When the state is
-- @AVAILABLE@, you can use the connection to create an App Runner service.
connection_status :: Lens.Lens' Connection (Prelude.Maybe ConnectionStatus)
connection_status = Lens.lens (\Connection' {status} -> status) (\s@Connection' {} a -> s {status = a} :: Connection)

instance Data.FromJSON Connection where
  parseJSON =
    Data.withObject
      "Connection"
      ( \x ->
          Connection'
            Prelude.<$> (x Data..:? "ConnectionArn")
            Prelude.<*> (x Data..:? "ConnectionName")
            Prelude.<*> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "ProviderType")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable Connection where
  hashWithSalt _salt Connection' {..} =
    _salt `Prelude.hashWithSalt` connectionArn
      `Prelude.hashWithSalt` connectionName
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` providerType
      `Prelude.hashWithSalt` status

instance Prelude.NFData Connection where
  rnf Connection' {..} =
    Prelude.rnf connectionArn
      `Prelude.seq` Prelude.rnf connectionName
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf providerType
      `Prelude.seq` Prelude.rnf status
