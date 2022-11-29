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
-- Module      : Amazonka.DMS.Types.Connection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.Connection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Status of the connection between an endpoint and a replication instance,
-- including Amazon Resource Names (ARNs) and the last error message
-- issued.
--
-- /See:/ 'newConnection' smart constructor.
data Connection = Connection'
  { -- | The replication instance identifier. This parameter is stored as a
    -- lowercase string.
    replicationInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the replication instance.
    replicationInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The error message when the connection last failed.
    lastFailureMessage :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the endpoint. Identifiers must begin with a letter and
    -- must contain only ASCII letters, digits, and hyphens. They can\'t end
    -- with a hyphen or contain two consecutive hyphens.
    endpointIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The connection status. This parameter can return one of the following
    -- values:
    --
    -- -   @\"successful\"@
    --
    -- -   @\"testing\"@
    --
    -- -   @\"failed\"@
    --
    -- -   @\"deleting\"@
    status :: Prelude.Maybe Prelude.Text,
    -- | The ARN string that uniquely identifies the endpoint.
    endpointArn :: Prelude.Maybe Prelude.Text
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
-- 'replicationInstanceIdentifier', 'connection_replicationInstanceIdentifier' - The replication instance identifier. This parameter is stored as a
-- lowercase string.
--
-- 'replicationInstanceArn', 'connection_replicationInstanceArn' - The ARN of the replication instance.
--
-- 'lastFailureMessage', 'connection_lastFailureMessage' - The error message when the connection last failed.
--
-- 'endpointIdentifier', 'connection_endpointIdentifier' - The identifier of the endpoint. Identifiers must begin with a letter and
-- must contain only ASCII letters, digits, and hyphens. They can\'t end
-- with a hyphen or contain two consecutive hyphens.
--
-- 'status', 'connection_status' - The connection status. This parameter can return one of the following
-- values:
--
-- -   @\"successful\"@
--
-- -   @\"testing\"@
--
-- -   @\"failed\"@
--
-- -   @\"deleting\"@
--
-- 'endpointArn', 'connection_endpointArn' - The ARN string that uniquely identifies the endpoint.
newConnection ::
  Connection
newConnection =
  Connection'
    { replicationInstanceIdentifier =
        Prelude.Nothing,
      replicationInstanceArn = Prelude.Nothing,
      lastFailureMessage = Prelude.Nothing,
      endpointIdentifier = Prelude.Nothing,
      status = Prelude.Nothing,
      endpointArn = Prelude.Nothing
    }

-- | The replication instance identifier. This parameter is stored as a
-- lowercase string.
connection_replicationInstanceIdentifier :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_replicationInstanceIdentifier = Lens.lens (\Connection' {replicationInstanceIdentifier} -> replicationInstanceIdentifier) (\s@Connection' {} a -> s {replicationInstanceIdentifier = a} :: Connection)

-- | The ARN of the replication instance.
connection_replicationInstanceArn :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_replicationInstanceArn = Lens.lens (\Connection' {replicationInstanceArn} -> replicationInstanceArn) (\s@Connection' {} a -> s {replicationInstanceArn = a} :: Connection)

-- | The error message when the connection last failed.
connection_lastFailureMessage :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_lastFailureMessage = Lens.lens (\Connection' {lastFailureMessage} -> lastFailureMessage) (\s@Connection' {} a -> s {lastFailureMessage = a} :: Connection)

-- | The identifier of the endpoint. Identifiers must begin with a letter and
-- must contain only ASCII letters, digits, and hyphens. They can\'t end
-- with a hyphen or contain two consecutive hyphens.
connection_endpointIdentifier :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_endpointIdentifier = Lens.lens (\Connection' {endpointIdentifier} -> endpointIdentifier) (\s@Connection' {} a -> s {endpointIdentifier = a} :: Connection)

-- | The connection status. This parameter can return one of the following
-- values:
--
-- -   @\"successful\"@
--
-- -   @\"testing\"@
--
-- -   @\"failed\"@
--
-- -   @\"deleting\"@
connection_status :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_status = Lens.lens (\Connection' {status} -> status) (\s@Connection' {} a -> s {status = a} :: Connection)

-- | The ARN string that uniquely identifies the endpoint.
connection_endpointArn :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_endpointArn = Lens.lens (\Connection' {endpointArn} -> endpointArn) (\s@Connection' {} a -> s {endpointArn = a} :: Connection)

instance Core.FromJSON Connection where
  parseJSON =
    Core.withObject
      "Connection"
      ( \x ->
          Connection'
            Prelude.<$> (x Core..:? "ReplicationInstanceIdentifier")
            Prelude.<*> (x Core..:? "ReplicationInstanceArn")
            Prelude.<*> (x Core..:? "LastFailureMessage")
            Prelude.<*> (x Core..:? "EndpointIdentifier")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "EndpointArn")
      )

instance Prelude.Hashable Connection where
  hashWithSalt _salt Connection' {..} =
    _salt
      `Prelude.hashWithSalt` replicationInstanceIdentifier
      `Prelude.hashWithSalt` replicationInstanceArn
      `Prelude.hashWithSalt` lastFailureMessage
      `Prelude.hashWithSalt` endpointIdentifier
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` endpointArn

instance Prelude.NFData Connection where
  rnf Connection' {..} =
    Prelude.rnf replicationInstanceIdentifier
      `Prelude.seq` Prelude.rnf replicationInstanceArn
      `Prelude.seq` Prelude.rnf lastFailureMessage
      `Prelude.seq` Prelude.rnf endpointIdentifier
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf endpointArn
