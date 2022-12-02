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
-- Module      : Amazonka.CodeStarConnections.Types.Connection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeStarConnections.Types.Connection where

import Amazonka.CodeStarConnections.Types.ConnectionStatus
import Amazonka.CodeStarConnections.Types.ProviderType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A resource that is used to connect third-party source providers with
-- services like AWS CodePipeline.
--
-- Note: A connection created through CloudFormation, the CLI, or the SDK
-- is in \`PENDING\` status by default. You can make its status
-- \`AVAILABLE\` by updating the connection in the console.
--
-- /See:/ 'newConnection' smart constructor.
data Connection = Connection'
  { -- | The Amazon Resource Name (ARN) of the host associated with the
    -- connection.
    hostArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the connection. The ARN is used as the
    -- connection reference when the connection is shared between AWS services.
    --
    -- The ARN is never reused if the connection is deleted.
    connectionArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the external provider where your third-party code repository
    -- is configured.
    providerType :: Prelude.Maybe ProviderType,
    -- | The identifier of the external provider where your third-party code
    -- repository is configured. For Bitbucket, this is the account ID of the
    -- owner of the Bitbucket repository.
    ownerAccountId :: Prelude.Maybe Prelude.Text,
    -- | The current status of the connection.
    connectionStatus :: Prelude.Maybe ConnectionStatus,
    -- | The name of the connection. Connection names must be unique in an AWS
    -- user account.
    connectionName :: Prelude.Maybe Prelude.Text
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
-- 'hostArn', 'connection_hostArn' - The Amazon Resource Name (ARN) of the host associated with the
-- connection.
--
-- 'connectionArn', 'connection_connectionArn' - The Amazon Resource Name (ARN) of the connection. The ARN is used as the
-- connection reference when the connection is shared between AWS services.
--
-- The ARN is never reused if the connection is deleted.
--
-- 'providerType', 'connection_providerType' - The name of the external provider where your third-party code repository
-- is configured.
--
-- 'ownerAccountId', 'connection_ownerAccountId' - The identifier of the external provider where your third-party code
-- repository is configured. For Bitbucket, this is the account ID of the
-- owner of the Bitbucket repository.
--
-- 'connectionStatus', 'connection_connectionStatus' - The current status of the connection.
--
-- 'connectionName', 'connection_connectionName' - The name of the connection. Connection names must be unique in an AWS
-- user account.
newConnection ::
  Connection
newConnection =
  Connection'
    { hostArn = Prelude.Nothing,
      connectionArn = Prelude.Nothing,
      providerType = Prelude.Nothing,
      ownerAccountId = Prelude.Nothing,
      connectionStatus = Prelude.Nothing,
      connectionName = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the host associated with the
-- connection.
connection_hostArn :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_hostArn = Lens.lens (\Connection' {hostArn} -> hostArn) (\s@Connection' {} a -> s {hostArn = a} :: Connection)

-- | The Amazon Resource Name (ARN) of the connection. The ARN is used as the
-- connection reference when the connection is shared between AWS services.
--
-- The ARN is never reused if the connection is deleted.
connection_connectionArn :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_connectionArn = Lens.lens (\Connection' {connectionArn} -> connectionArn) (\s@Connection' {} a -> s {connectionArn = a} :: Connection)

-- | The name of the external provider where your third-party code repository
-- is configured.
connection_providerType :: Lens.Lens' Connection (Prelude.Maybe ProviderType)
connection_providerType = Lens.lens (\Connection' {providerType} -> providerType) (\s@Connection' {} a -> s {providerType = a} :: Connection)

-- | The identifier of the external provider where your third-party code
-- repository is configured. For Bitbucket, this is the account ID of the
-- owner of the Bitbucket repository.
connection_ownerAccountId :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_ownerAccountId = Lens.lens (\Connection' {ownerAccountId} -> ownerAccountId) (\s@Connection' {} a -> s {ownerAccountId = a} :: Connection)

-- | The current status of the connection.
connection_connectionStatus :: Lens.Lens' Connection (Prelude.Maybe ConnectionStatus)
connection_connectionStatus = Lens.lens (\Connection' {connectionStatus} -> connectionStatus) (\s@Connection' {} a -> s {connectionStatus = a} :: Connection)

-- | The name of the connection. Connection names must be unique in an AWS
-- user account.
connection_connectionName :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_connectionName = Lens.lens (\Connection' {connectionName} -> connectionName) (\s@Connection' {} a -> s {connectionName = a} :: Connection)

instance Data.FromJSON Connection where
  parseJSON =
    Data.withObject
      "Connection"
      ( \x ->
          Connection'
            Prelude.<$> (x Data..:? "HostArn")
            Prelude.<*> (x Data..:? "ConnectionArn")
            Prelude.<*> (x Data..:? "ProviderType")
            Prelude.<*> (x Data..:? "OwnerAccountId")
            Prelude.<*> (x Data..:? "ConnectionStatus")
            Prelude.<*> (x Data..:? "ConnectionName")
      )

instance Prelude.Hashable Connection where
  hashWithSalt _salt Connection' {..} =
    _salt `Prelude.hashWithSalt` hostArn
      `Prelude.hashWithSalt` connectionArn
      `Prelude.hashWithSalt` providerType
      `Prelude.hashWithSalt` ownerAccountId
      `Prelude.hashWithSalt` connectionStatus
      `Prelude.hashWithSalt` connectionName

instance Prelude.NFData Connection where
  rnf Connection' {..} =
    Prelude.rnf hostArn
      `Prelude.seq` Prelude.rnf connectionArn
      `Prelude.seq` Prelude.rnf providerType
      `Prelude.seq` Prelude.rnf ownerAccountId
      `Prelude.seq` Prelude.rnf connectionStatus
      `Prelude.seq` Prelude.rnf connectionName
