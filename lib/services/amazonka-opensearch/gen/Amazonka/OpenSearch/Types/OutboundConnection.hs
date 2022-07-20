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
-- Module      : Amazonka.OpenSearch.Types.OutboundConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.OutboundConnection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.OpenSearch.Types.DomainInformationContainer
import Amazonka.OpenSearch.Types.OutboundConnectionStatus
import qualified Amazonka.Prelude as Prelude

-- | Specifies details about an outbound connection.
--
-- /See:/ 'newOutboundConnection' smart constructor.
data OutboundConnection = OutboundConnection'
  { -- | The connection alias for the outbound cross-cluster connection.
    connectionAlias :: Prelude.Maybe Prelude.Text,
    -- | The @ DomainInformation @ for the remote OpenSearch domain.
    remoteDomainInfo :: Prelude.Maybe DomainInformationContainer,
    -- | The connection ID for the outbound cross-cluster connection.
    connectionId :: Prelude.Maybe Prelude.Text,
    -- | The @ DomainInformation @ for the local OpenSearch domain.
    localDomainInfo :: Prelude.Maybe DomainInformationContainer,
    -- | The @ OutboundConnectionStatus @ for the outbound connection.
    connectionStatus :: Prelude.Maybe OutboundConnectionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutboundConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionAlias', 'outboundConnection_connectionAlias' - The connection alias for the outbound cross-cluster connection.
--
-- 'remoteDomainInfo', 'outboundConnection_remoteDomainInfo' - The @ DomainInformation @ for the remote OpenSearch domain.
--
-- 'connectionId', 'outboundConnection_connectionId' - The connection ID for the outbound cross-cluster connection.
--
-- 'localDomainInfo', 'outboundConnection_localDomainInfo' - The @ DomainInformation @ for the local OpenSearch domain.
--
-- 'connectionStatus', 'outboundConnection_connectionStatus' - The @ OutboundConnectionStatus @ for the outbound connection.
newOutboundConnection ::
  OutboundConnection
newOutboundConnection =
  OutboundConnection'
    { connectionAlias =
        Prelude.Nothing,
      remoteDomainInfo = Prelude.Nothing,
      connectionId = Prelude.Nothing,
      localDomainInfo = Prelude.Nothing,
      connectionStatus = Prelude.Nothing
    }

-- | The connection alias for the outbound cross-cluster connection.
outboundConnection_connectionAlias :: Lens.Lens' OutboundConnection (Prelude.Maybe Prelude.Text)
outboundConnection_connectionAlias = Lens.lens (\OutboundConnection' {connectionAlias} -> connectionAlias) (\s@OutboundConnection' {} a -> s {connectionAlias = a} :: OutboundConnection)

-- | The @ DomainInformation @ for the remote OpenSearch domain.
outboundConnection_remoteDomainInfo :: Lens.Lens' OutboundConnection (Prelude.Maybe DomainInformationContainer)
outboundConnection_remoteDomainInfo = Lens.lens (\OutboundConnection' {remoteDomainInfo} -> remoteDomainInfo) (\s@OutboundConnection' {} a -> s {remoteDomainInfo = a} :: OutboundConnection)

-- | The connection ID for the outbound cross-cluster connection.
outboundConnection_connectionId :: Lens.Lens' OutboundConnection (Prelude.Maybe Prelude.Text)
outboundConnection_connectionId = Lens.lens (\OutboundConnection' {connectionId} -> connectionId) (\s@OutboundConnection' {} a -> s {connectionId = a} :: OutboundConnection)

-- | The @ DomainInformation @ for the local OpenSearch domain.
outboundConnection_localDomainInfo :: Lens.Lens' OutboundConnection (Prelude.Maybe DomainInformationContainer)
outboundConnection_localDomainInfo = Lens.lens (\OutboundConnection' {localDomainInfo} -> localDomainInfo) (\s@OutboundConnection' {} a -> s {localDomainInfo = a} :: OutboundConnection)

-- | The @ OutboundConnectionStatus @ for the outbound connection.
outboundConnection_connectionStatus :: Lens.Lens' OutboundConnection (Prelude.Maybe OutboundConnectionStatus)
outboundConnection_connectionStatus = Lens.lens (\OutboundConnection' {connectionStatus} -> connectionStatus) (\s@OutboundConnection' {} a -> s {connectionStatus = a} :: OutboundConnection)

instance Core.FromJSON OutboundConnection where
  parseJSON =
    Core.withObject
      "OutboundConnection"
      ( \x ->
          OutboundConnection'
            Prelude.<$> (x Core..:? "ConnectionAlias")
            Prelude.<*> (x Core..:? "RemoteDomainInfo")
            Prelude.<*> (x Core..:? "ConnectionId")
            Prelude.<*> (x Core..:? "LocalDomainInfo")
            Prelude.<*> (x Core..:? "ConnectionStatus")
      )

instance Prelude.Hashable OutboundConnection where
  hashWithSalt _salt OutboundConnection' {..} =
    _salt `Prelude.hashWithSalt` connectionAlias
      `Prelude.hashWithSalt` remoteDomainInfo
      `Prelude.hashWithSalt` connectionId
      `Prelude.hashWithSalt` localDomainInfo
      `Prelude.hashWithSalt` connectionStatus

instance Prelude.NFData OutboundConnection where
  rnf OutboundConnection' {..} =
    Prelude.rnf connectionAlias
      `Prelude.seq` Prelude.rnf remoteDomainInfo
      `Prelude.seq` Prelude.rnf connectionId
      `Prelude.seq` Prelude.rnf localDomainInfo
      `Prelude.seq` Prelude.rnf connectionStatus
