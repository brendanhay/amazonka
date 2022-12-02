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
-- Module      : Amazonka.OpenSearch.Types.InboundConnection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.InboundConnection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.DomainInformationContainer
import Amazonka.OpenSearch.Types.InboundConnectionStatus
import qualified Amazonka.Prelude as Prelude

-- | Describes an inbound cross-cluster connection for Amazon OpenSearch
-- Service. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/cross-cluster-search.html Cross-cluster search for Amazon OpenSearch Service>.
--
-- /See:/ 'newInboundConnection' smart constructor.
data InboundConnection = InboundConnection'
  { -- | Information about the destination (remote) domain.
    remoteDomainInfo :: Prelude.Maybe DomainInformationContainer,
    -- | The unique identifier of the connection.
    connectionId :: Prelude.Maybe Prelude.Text,
    -- | Information about the source (local) domain.
    localDomainInfo :: Prelude.Maybe DomainInformationContainer,
    -- | The current status of the connection.
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
-- 'remoteDomainInfo', 'inboundConnection_remoteDomainInfo' - Information about the destination (remote) domain.
--
-- 'connectionId', 'inboundConnection_connectionId' - The unique identifier of the connection.
--
-- 'localDomainInfo', 'inboundConnection_localDomainInfo' - Information about the source (local) domain.
--
-- 'connectionStatus', 'inboundConnection_connectionStatus' - The current status of the connection.
newInboundConnection ::
  InboundConnection
newInboundConnection =
  InboundConnection'
    { remoteDomainInfo =
        Prelude.Nothing,
      connectionId = Prelude.Nothing,
      localDomainInfo = Prelude.Nothing,
      connectionStatus = Prelude.Nothing
    }

-- | Information about the destination (remote) domain.
inboundConnection_remoteDomainInfo :: Lens.Lens' InboundConnection (Prelude.Maybe DomainInformationContainer)
inboundConnection_remoteDomainInfo = Lens.lens (\InboundConnection' {remoteDomainInfo} -> remoteDomainInfo) (\s@InboundConnection' {} a -> s {remoteDomainInfo = a} :: InboundConnection)

-- | The unique identifier of the connection.
inboundConnection_connectionId :: Lens.Lens' InboundConnection (Prelude.Maybe Prelude.Text)
inboundConnection_connectionId = Lens.lens (\InboundConnection' {connectionId} -> connectionId) (\s@InboundConnection' {} a -> s {connectionId = a} :: InboundConnection)

-- | Information about the source (local) domain.
inboundConnection_localDomainInfo :: Lens.Lens' InboundConnection (Prelude.Maybe DomainInformationContainer)
inboundConnection_localDomainInfo = Lens.lens (\InboundConnection' {localDomainInfo} -> localDomainInfo) (\s@InboundConnection' {} a -> s {localDomainInfo = a} :: InboundConnection)

-- | The current status of the connection.
inboundConnection_connectionStatus :: Lens.Lens' InboundConnection (Prelude.Maybe InboundConnectionStatus)
inboundConnection_connectionStatus = Lens.lens (\InboundConnection' {connectionStatus} -> connectionStatus) (\s@InboundConnection' {} a -> s {connectionStatus = a} :: InboundConnection)

instance Data.FromJSON InboundConnection where
  parseJSON =
    Data.withObject
      "InboundConnection"
      ( \x ->
          InboundConnection'
            Prelude.<$> (x Data..:? "RemoteDomainInfo")
            Prelude.<*> (x Data..:? "ConnectionId")
            Prelude.<*> (x Data..:? "LocalDomainInfo")
            Prelude.<*> (x Data..:? "ConnectionStatus")
      )

instance Prelude.Hashable InboundConnection where
  hashWithSalt _salt InboundConnection' {..} =
    _salt `Prelude.hashWithSalt` remoteDomainInfo
      `Prelude.hashWithSalt` connectionId
      `Prelude.hashWithSalt` localDomainInfo
      `Prelude.hashWithSalt` connectionStatus

instance Prelude.NFData InboundConnection where
  rnf InboundConnection' {..} =
    Prelude.rnf remoteDomainInfo
      `Prelude.seq` Prelude.rnf connectionId
      `Prelude.seq` Prelude.rnf localDomainInfo
      `Prelude.seq` Prelude.rnf connectionStatus
