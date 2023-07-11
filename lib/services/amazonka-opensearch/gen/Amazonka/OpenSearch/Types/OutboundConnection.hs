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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.OutboundConnection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.DomainInformationContainer
import Amazonka.OpenSearch.Types.OutboundConnectionStatus
import qualified Amazonka.Prelude as Prelude

-- | Specifies details about an outbound cross-cluster connection.
--
-- /See:/ 'newOutboundConnection' smart constructor.
data OutboundConnection = OutboundConnection'
  { -- | Name of the connection.
    connectionAlias :: Prelude.Maybe Prelude.Text,
    -- | Unique identifier of the connection.
    connectionId :: Prelude.Maybe Prelude.Text,
    -- | Status of the connection.
    connectionStatus :: Prelude.Maybe OutboundConnectionStatus,
    -- | Information about the source (local) domain.
    localDomainInfo :: Prelude.Maybe DomainInformationContainer,
    -- | Information about the destination (remote) domain.
    remoteDomainInfo :: Prelude.Maybe DomainInformationContainer
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
-- 'connectionAlias', 'outboundConnection_connectionAlias' - Name of the connection.
--
-- 'connectionId', 'outboundConnection_connectionId' - Unique identifier of the connection.
--
-- 'connectionStatus', 'outboundConnection_connectionStatus' - Status of the connection.
--
-- 'localDomainInfo', 'outboundConnection_localDomainInfo' - Information about the source (local) domain.
--
-- 'remoteDomainInfo', 'outboundConnection_remoteDomainInfo' - Information about the destination (remote) domain.
newOutboundConnection ::
  OutboundConnection
newOutboundConnection =
  OutboundConnection'
    { connectionAlias =
        Prelude.Nothing,
      connectionId = Prelude.Nothing,
      connectionStatus = Prelude.Nothing,
      localDomainInfo = Prelude.Nothing,
      remoteDomainInfo = Prelude.Nothing
    }

-- | Name of the connection.
outboundConnection_connectionAlias :: Lens.Lens' OutboundConnection (Prelude.Maybe Prelude.Text)
outboundConnection_connectionAlias = Lens.lens (\OutboundConnection' {connectionAlias} -> connectionAlias) (\s@OutboundConnection' {} a -> s {connectionAlias = a} :: OutboundConnection)

-- | Unique identifier of the connection.
outboundConnection_connectionId :: Lens.Lens' OutboundConnection (Prelude.Maybe Prelude.Text)
outboundConnection_connectionId = Lens.lens (\OutboundConnection' {connectionId} -> connectionId) (\s@OutboundConnection' {} a -> s {connectionId = a} :: OutboundConnection)

-- | Status of the connection.
outboundConnection_connectionStatus :: Lens.Lens' OutboundConnection (Prelude.Maybe OutboundConnectionStatus)
outboundConnection_connectionStatus = Lens.lens (\OutboundConnection' {connectionStatus} -> connectionStatus) (\s@OutboundConnection' {} a -> s {connectionStatus = a} :: OutboundConnection)

-- | Information about the source (local) domain.
outboundConnection_localDomainInfo :: Lens.Lens' OutboundConnection (Prelude.Maybe DomainInformationContainer)
outboundConnection_localDomainInfo = Lens.lens (\OutboundConnection' {localDomainInfo} -> localDomainInfo) (\s@OutboundConnection' {} a -> s {localDomainInfo = a} :: OutboundConnection)

-- | Information about the destination (remote) domain.
outboundConnection_remoteDomainInfo :: Lens.Lens' OutboundConnection (Prelude.Maybe DomainInformationContainer)
outboundConnection_remoteDomainInfo = Lens.lens (\OutboundConnection' {remoteDomainInfo} -> remoteDomainInfo) (\s@OutboundConnection' {} a -> s {remoteDomainInfo = a} :: OutboundConnection)

instance Data.FromJSON OutboundConnection where
  parseJSON =
    Data.withObject
      "OutboundConnection"
      ( \x ->
          OutboundConnection'
            Prelude.<$> (x Data..:? "ConnectionAlias")
            Prelude.<*> (x Data..:? "ConnectionId")
            Prelude.<*> (x Data..:? "ConnectionStatus")
            Prelude.<*> (x Data..:? "LocalDomainInfo")
            Prelude.<*> (x Data..:? "RemoteDomainInfo")
      )

instance Prelude.Hashable OutboundConnection where
  hashWithSalt _salt OutboundConnection' {..} =
    _salt
      `Prelude.hashWithSalt` connectionAlias
      `Prelude.hashWithSalt` connectionId
      `Prelude.hashWithSalt` connectionStatus
      `Prelude.hashWithSalt` localDomainInfo
      `Prelude.hashWithSalt` remoteDomainInfo

instance Prelude.NFData OutboundConnection where
  rnf OutboundConnection' {..} =
    Prelude.rnf connectionAlias
      `Prelude.seq` Prelude.rnf connectionId
      `Prelude.seq` Prelude.rnf connectionStatus
      `Prelude.seq` Prelude.rnf localDomainInfo
      `Prelude.seq` Prelude.rnf remoteDomainInfo
