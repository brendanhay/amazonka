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
-- Module      : Amazonka.ElasticSearch.Types.OutboundCrossClusterSearchConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.OutboundCrossClusterSearchConnection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types.DomainInformation
import Amazonka.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatus
import qualified Amazonka.Prelude as Prelude

-- | Specifies details of an outbound connection.
--
-- /See:/ 'newOutboundCrossClusterSearchConnection' smart constructor.
data OutboundCrossClusterSearchConnection = OutboundCrossClusterSearchConnection'
  { -- | Specifies the connection alias for the outbound cross-cluster search
    -- connection.
    connectionAlias :: Prelude.Maybe Prelude.Text,
    -- | Specifies the @OutboundCrossClusterSearchConnectionStatus@ for the
    -- outbound connection.
    connectionStatus :: Prelude.Maybe OutboundCrossClusterSearchConnectionStatus,
    -- | Specifies the connection id for the outbound cross-cluster search
    -- connection.
    crossClusterSearchConnectionId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the @DomainInformation@ for the destination Elasticsearch
    -- domain.
    destinationDomainInfo :: Prelude.Maybe DomainInformation,
    -- | Specifies the @DomainInformation@ for the source Elasticsearch domain.
    sourceDomainInfo :: Prelude.Maybe DomainInformation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutboundCrossClusterSearchConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionAlias', 'outboundCrossClusterSearchConnection_connectionAlias' - Specifies the connection alias for the outbound cross-cluster search
-- connection.
--
-- 'connectionStatus', 'outboundCrossClusterSearchConnection_connectionStatus' - Specifies the @OutboundCrossClusterSearchConnectionStatus@ for the
-- outbound connection.
--
-- 'crossClusterSearchConnectionId', 'outboundCrossClusterSearchConnection_crossClusterSearchConnectionId' - Specifies the connection id for the outbound cross-cluster search
-- connection.
--
-- 'destinationDomainInfo', 'outboundCrossClusterSearchConnection_destinationDomainInfo' - Specifies the @DomainInformation@ for the destination Elasticsearch
-- domain.
--
-- 'sourceDomainInfo', 'outboundCrossClusterSearchConnection_sourceDomainInfo' - Specifies the @DomainInformation@ for the source Elasticsearch domain.
newOutboundCrossClusterSearchConnection ::
  OutboundCrossClusterSearchConnection
newOutboundCrossClusterSearchConnection =
  OutboundCrossClusterSearchConnection'
    { connectionAlias =
        Prelude.Nothing,
      connectionStatus = Prelude.Nothing,
      crossClusterSearchConnectionId =
        Prelude.Nothing,
      destinationDomainInfo =
        Prelude.Nothing,
      sourceDomainInfo = Prelude.Nothing
    }

-- | Specifies the connection alias for the outbound cross-cluster search
-- connection.
outboundCrossClusterSearchConnection_connectionAlias :: Lens.Lens' OutboundCrossClusterSearchConnection (Prelude.Maybe Prelude.Text)
outboundCrossClusterSearchConnection_connectionAlias = Lens.lens (\OutboundCrossClusterSearchConnection' {connectionAlias} -> connectionAlias) (\s@OutboundCrossClusterSearchConnection' {} a -> s {connectionAlias = a} :: OutboundCrossClusterSearchConnection)

-- | Specifies the @OutboundCrossClusterSearchConnectionStatus@ for the
-- outbound connection.
outboundCrossClusterSearchConnection_connectionStatus :: Lens.Lens' OutboundCrossClusterSearchConnection (Prelude.Maybe OutboundCrossClusterSearchConnectionStatus)
outboundCrossClusterSearchConnection_connectionStatus = Lens.lens (\OutboundCrossClusterSearchConnection' {connectionStatus} -> connectionStatus) (\s@OutboundCrossClusterSearchConnection' {} a -> s {connectionStatus = a} :: OutboundCrossClusterSearchConnection)

-- | Specifies the connection id for the outbound cross-cluster search
-- connection.
outboundCrossClusterSearchConnection_crossClusterSearchConnectionId :: Lens.Lens' OutboundCrossClusterSearchConnection (Prelude.Maybe Prelude.Text)
outboundCrossClusterSearchConnection_crossClusterSearchConnectionId = Lens.lens (\OutboundCrossClusterSearchConnection' {crossClusterSearchConnectionId} -> crossClusterSearchConnectionId) (\s@OutboundCrossClusterSearchConnection' {} a -> s {crossClusterSearchConnectionId = a} :: OutboundCrossClusterSearchConnection)

-- | Specifies the @DomainInformation@ for the destination Elasticsearch
-- domain.
outboundCrossClusterSearchConnection_destinationDomainInfo :: Lens.Lens' OutboundCrossClusterSearchConnection (Prelude.Maybe DomainInformation)
outboundCrossClusterSearchConnection_destinationDomainInfo = Lens.lens (\OutboundCrossClusterSearchConnection' {destinationDomainInfo} -> destinationDomainInfo) (\s@OutboundCrossClusterSearchConnection' {} a -> s {destinationDomainInfo = a} :: OutboundCrossClusterSearchConnection)

-- | Specifies the @DomainInformation@ for the source Elasticsearch domain.
outboundCrossClusterSearchConnection_sourceDomainInfo :: Lens.Lens' OutboundCrossClusterSearchConnection (Prelude.Maybe DomainInformation)
outboundCrossClusterSearchConnection_sourceDomainInfo = Lens.lens (\OutboundCrossClusterSearchConnection' {sourceDomainInfo} -> sourceDomainInfo) (\s@OutboundCrossClusterSearchConnection' {} a -> s {sourceDomainInfo = a} :: OutboundCrossClusterSearchConnection)

instance
  Data.FromJSON
    OutboundCrossClusterSearchConnection
  where
  parseJSON =
    Data.withObject
      "OutboundCrossClusterSearchConnection"
      ( \x ->
          OutboundCrossClusterSearchConnection'
            Prelude.<$> (x Data..:? "ConnectionAlias")
            Prelude.<*> (x Data..:? "ConnectionStatus")
            Prelude.<*> (x Data..:? "CrossClusterSearchConnectionId")
            Prelude.<*> (x Data..:? "DestinationDomainInfo")
            Prelude.<*> (x Data..:? "SourceDomainInfo")
      )

instance
  Prelude.Hashable
    OutboundCrossClusterSearchConnection
  where
  hashWithSalt
    _salt
    OutboundCrossClusterSearchConnection' {..} =
      _salt `Prelude.hashWithSalt` connectionAlias
        `Prelude.hashWithSalt` connectionStatus
        `Prelude.hashWithSalt` crossClusterSearchConnectionId
        `Prelude.hashWithSalt` destinationDomainInfo
        `Prelude.hashWithSalt` sourceDomainInfo

instance
  Prelude.NFData
    OutboundCrossClusterSearchConnection
  where
  rnf OutboundCrossClusterSearchConnection' {..} =
    Prelude.rnf connectionAlias
      `Prelude.seq` Prelude.rnf connectionStatus
      `Prelude.seq` Prelude.rnf crossClusterSearchConnectionId
      `Prelude.seq` Prelude.rnf destinationDomainInfo
      `Prelude.seq` Prelude.rnf sourceDomainInfo
