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
-- Module      : Network.AWS.DMS.Types.Connection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.Connection where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Status of the connection between an endpoint and a replication instance,
-- including Amazon Resource Names (ARNs) and the last error message
-- issued.
--
-- /See:/ 'newConnection' smart constructor.
data Connection = Connection'
  { -- | The connection status. This parameter can return one of the following
    -- values:
    --
    -- -   @\"successful\"@
    --
    -- -   @\"testing\"@
    --
    -- -   @\"failed\"@
    --
    -- -   @\"deleting\"@
    status :: Core.Maybe Core.Text,
    -- | The error message when the connection last failed.
    lastFailureMessage :: Core.Maybe Core.Text,
    -- | The ARN string that uniquely identifies the endpoint.
    endpointArn :: Core.Maybe Core.Text,
    -- | The replication instance identifier. This parameter is stored as a
    -- lowercase string.
    replicationInstanceIdentifier :: Core.Maybe Core.Text,
    -- | The ARN of the replication instance.
    replicationInstanceArn :: Core.Maybe Core.Text,
    -- | The identifier of the endpoint. Identifiers must begin with a letter and
    -- must contain only ASCII letters, digits, and hyphens. They can\'t end
    -- with a hyphen or contain two consecutive hyphens.
    endpointIdentifier :: Core.Maybe Core.Text
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
-- 'lastFailureMessage', 'connection_lastFailureMessage' - The error message when the connection last failed.
--
-- 'endpointArn', 'connection_endpointArn' - The ARN string that uniquely identifies the endpoint.
--
-- 'replicationInstanceIdentifier', 'connection_replicationInstanceIdentifier' - The replication instance identifier. This parameter is stored as a
-- lowercase string.
--
-- 'replicationInstanceArn', 'connection_replicationInstanceArn' - The ARN of the replication instance.
--
-- 'endpointIdentifier', 'connection_endpointIdentifier' - The identifier of the endpoint. Identifiers must begin with a letter and
-- must contain only ASCII letters, digits, and hyphens. They can\'t end
-- with a hyphen or contain two consecutive hyphens.
newConnection ::
  Connection
newConnection =
  Connection'
    { status = Core.Nothing,
      lastFailureMessage = Core.Nothing,
      endpointArn = Core.Nothing,
      replicationInstanceIdentifier = Core.Nothing,
      replicationInstanceArn = Core.Nothing,
      endpointIdentifier = Core.Nothing
    }

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
connection_status :: Lens.Lens' Connection (Core.Maybe Core.Text)
connection_status = Lens.lens (\Connection' {status} -> status) (\s@Connection' {} a -> s {status = a} :: Connection)

-- | The error message when the connection last failed.
connection_lastFailureMessage :: Lens.Lens' Connection (Core.Maybe Core.Text)
connection_lastFailureMessage = Lens.lens (\Connection' {lastFailureMessage} -> lastFailureMessage) (\s@Connection' {} a -> s {lastFailureMessage = a} :: Connection)

-- | The ARN string that uniquely identifies the endpoint.
connection_endpointArn :: Lens.Lens' Connection (Core.Maybe Core.Text)
connection_endpointArn = Lens.lens (\Connection' {endpointArn} -> endpointArn) (\s@Connection' {} a -> s {endpointArn = a} :: Connection)

-- | The replication instance identifier. This parameter is stored as a
-- lowercase string.
connection_replicationInstanceIdentifier :: Lens.Lens' Connection (Core.Maybe Core.Text)
connection_replicationInstanceIdentifier = Lens.lens (\Connection' {replicationInstanceIdentifier} -> replicationInstanceIdentifier) (\s@Connection' {} a -> s {replicationInstanceIdentifier = a} :: Connection)

-- | The ARN of the replication instance.
connection_replicationInstanceArn :: Lens.Lens' Connection (Core.Maybe Core.Text)
connection_replicationInstanceArn = Lens.lens (\Connection' {replicationInstanceArn} -> replicationInstanceArn) (\s@Connection' {} a -> s {replicationInstanceArn = a} :: Connection)

-- | The identifier of the endpoint. Identifiers must begin with a letter and
-- must contain only ASCII letters, digits, and hyphens. They can\'t end
-- with a hyphen or contain two consecutive hyphens.
connection_endpointIdentifier :: Lens.Lens' Connection (Core.Maybe Core.Text)
connection_endpointIdentifier = Lens.lens (\Connection' {endpointIdentifier} -> endpointIdentifier) (\s@Connection' {} a -> s {endpointIdentifier = a} :: Connection)

instance Core.FromJSON Connection where
  parseJSON =
    Core.withObject
      "Connection"
      ( \x ->
          Connection'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "LastFailureMessage")
            Core.<*> (x Core..:? "EndpointArn")
            Core.<*> (x Core..:? "ReplicationInstanceIdentifier")
            Core.<*> (x Core..:? "ReplicationInstanceArn")
            Core.<*> (x Core..:? "EndpointIdentifier")
      )

instance Core.Hashable Connection

instance Core.NFData Connection
