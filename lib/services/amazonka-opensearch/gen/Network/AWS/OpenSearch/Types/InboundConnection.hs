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
-- Module      : Network.AWS.OpenSearch.Types.InboundConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpenSearch.Types.InboundConnection where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpenSearch.Types.DomainInformationContainer
import Network.AWS.OpenSearch.Types.InboundConnectionStatus
import qualified Network.AWS.Prelude as Prelude

-- | Details of an inbound connection.
--
-- /See:/ 'newInboundConnection' smart constructor.
data InboundConnection = InboundConnection'
  { -- | The @ AWSDomainInformation @ for the remote OpenSearch domain.
    remoteDomainInfo :: Prelude.Maybe DomainInformationContainer,
    -- | The @ AWSDomainInformation @ for the local OpenSearch domain.
    localDomainInfo :: Prelude.Maybe DomainInformationContainer,
    -- | The connection ID for the inbound cross-cluster connection.
    connectionId :: Prelude.Maybe Prelude.Text,
    -- | The @ InboundConnectionStatus @ for the outbound connection.
    connectionStatus :: Prelude.Maybe InboundConnectionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InboundConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'remoteDomainInfo', 'inboundConnection_remoteDomainInfo' - The @ AWSDomainInformation @ for the remote OpenSearch domain.
--
-- 'localDomainInfo', 'inboundConnection_localDomainInfo' - The @ AWSDomainInformation @ for the local OpenSearch domain.
--
-- 'connectionId', 'inboundConnection_connectionId' - The connection ID for the inbound cross-cluster connection.
--
-- 'connectionStatus', 'inboundConnection_connectionStatus' - The @ InboundConnectionStatus @ for the outbound connection.
newInboundConnection ::
  InboundConnection
newInboundConnection =
  InboundConnection'
    { remoteDomainInfo =
        Prelude.Nothing,
      localDomainInfo = Prelude.Nothing,
      connectionId = Prelude.Nothing,
      connectionStatus = Prelude.Nothing
    }

-- | The @ AWSDomainInformation @ for the remote OpenSearch domain.
inboundConnection_remoteDomainInfo :: Lens.Lens' InboundConnection (Prelude.Maybe DomainInformationContainer)
inboundConnection_remoteDomainInfo = Lens.lens (\InboundConnection' {remoteDomainInfo} -> remoteDomainInfo) (\s@InboundConnection' {} a -> s {remoteDomainInfo = a} :: InboundConnection)

-- | The @ AWSDomainInformation @ for the local OpenSearch domain.
inboundConnection_localDomainInfo :: Lens.Lens' InboundConnection (Prelude.Maybe DomainInformationContainer)
inboundConnection_localDomainInfo = Lens.lens (\InboundConnection' {localDomainInfo} -> localDomainInfo) (\s@InboundConnection' {} a -> s {localDomainInfo = a} :: InboundConnection)

-- | The connection ID for the inbound cross-cluster connection.
inboundConnection_connectionId :: Lens.Lens' InboundConnection (Prelude.Maybe Prelude.Text)
inboundConnection_connectionId = Lens.lens (\InboundConnection' {connectionId} -> connectionId) (\s@InboundConnection' {} a -> s {connectionId = a} :: InboundConnection)

-- | The @ InboundConnectionStatus @ for the outbound connection.
inboundConnection_connectionStatus :: Lens.Lens' InboundConnection (Prelude.Maybe InboundConnectionStatus)
inboundConnection_connectionStatus = Lens.lens (\InboundConnection' {connectionStatus} -> connectionStatus) (\s@InboundConnection' {} a -> s {connectionStatus = a} :: InboundConnection)

instance Core.FromJSON InboundConnection where
  parseJSON =
    Core.withObject
      "InboundConnection"
      ( \x ->
          InboundConnection'
            Prelude.<$> (x Core..:? "RemoteDomainInfo")
            Prelude.<*> (x Core..:? "LocalDomainInfo")
            Prelude.<*> (x Core..:? "ConnectionId")
            Prelude.<*> (x Core..:? "ConnectionStatus")
      )

instance Prelude.Hashable InboundConnection

instance Prelude.NFData InboundConnection
