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
-- Module      : Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnection where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types.DomainInformation
import Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatus
import qualified Network.AWS.Lens as Lens

-- | Specifies details of an outbound connection.
--
-- /See:/ 'newOutboundCrossClusterSearchConnection' smart constructor.
data OutboundCrossClusterSearchConnection = OutboundCrossClusterSearchConnection'
  { -- | Specifies the connection id for the outbound cross-cluster search
    -- connection.
    crossClusterSearchConnectionId :: Core.Maybe Core.Text,
    -- | Specifies the @DomainInformation@ for the source Elasticsearch domain.
    sourceDomainInfo :: Core.Maybe DomainInformation,
    -- | Specifies the connection alias for the outbound cross-cluster search
    -- connection.
    connectionAlias :: Core.Maybe Core.Text,
    -- | Specifies the @DomainInformation@ for the destination Elasticsearch
    -- domain.
    destinationDomainInfo :: Core.Maybe DomainInformation,
    -- | Specifies the @OutboundCrossClusterSearchConnectionStatus@ for the
    -- outbound connection.
    connectionStatus :: Core.Maybe OutboundCrossClusterSearchConnectionStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OutboundCrossClusterSearchConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crossClusterSearchConnectionId', 'outboundCrossClusterSearchConnection_crossClusterSearchConnectionId' - Specifies the connection id for the outbound cross-cluster search
-- connection.
--
-- 'sourceDomainInfo', 'outboundCrossClusterSearchConnection_sourceDomainInfo' - Specifies the @DomainInformation@ for the source Elasticsearch domain.
--
-- 'connectionAlias', 'outboundCrossClusterSearchConnection_connectionAlias' - Specifies the connection alias for the outbound cross-cluster search
-- connection.
--
-- 'destinationDomainInfo', 'outboundCrossClusterSearchConnection_destinationDomainInfo' - Specifies the @DomainInformation@ for the destination Elasticsearch
-- domain.
--
-- 'connectionStatus', 'outboundCrossClusterSearchConnection_connectionStatus' - Specifies the @OutboundCrossClusterSearchConnectionStatus@ for the
-- outbound connection.
newOutboundCrossClusterSearchConnection ::
  OutboundCrossClusterSearchConnection
newOutboundCrossClusterSearchConnection =
  OutboundCrossClusterSearchConnection'
    { crossClusterSearchConnectionId =
        Core.Nothing,
      sourceDomainInfo = Core.Nothing,
      connectionAlias = Core.Nothing,
      destinationDomainInfo = Core.Nothing,
      connectionStatus = Core.Nothing
    }

-- | Specifies the connection id for the outbound cross-cluster search
-- connection.
outboundCrossClusterSearchConnection_crossClusterSearchConnectionId :: Lens.Lens' OutboundCrossClusterSearchConnection (Core.Maybe Core.Text)
outboundCrossClusterSearchConnection_crossClusterSearchConnectionId = Lens.lens (\OutboundCrossClusterSearchConnection' {crossClusterSearchConnectionId} -> crossClusterSearchConnectionId) (\s@OutboundCrossClusterSearchConnection' {} a -> s {crossClusterSearchConnectionId = a} :: OutboundCrossClusterSearchConnection)

-- | Specifies the @DomainInformation@ for the source Elasticsearch domain.
outboundCrossClusterSearchConnection_sourceDomainInfo :: Lens.Lens' OutboundCrossClusterSearchConnection (Core.Maybe DomainInformation)
outboundCrossClusterSearchConnection_sourceDomainInfo = Lens.lens (\OutboundCrossClusterSearchConnection' {sourceDomainInfo} -> sourceDomainInfo) (\s@OutboundCrossClusterSearchConnection' {} a -> s {sourceDomainInfo = a} :: OutboundCrossClusterSearchConnection)

-- | Specifies the connection alias for the outbound cross-cluster search
-- connection.
outboundCrossClusterSearchConnection_connectionAlias :: Lens.Lens' OutboundCrossClusterSearchConnection (Core.Maybe Core.Text)
outboundCrossClusterSearchConnection_connectionAlias = Lens.lens (\OutboundCrossClusterSearchConnection' {connectionAlias} -> connectionAlias) (\s@OutboundCrossClusterSearchConnection' {} a -> s {connectionAlias = a} :: OutboundCrossClusterSearchConnection)

-- | Specifies the @DomainInformation@ for the destination Elasticsearch
-- domain.
outboundCrossClusterSearchConnection_destinationDomainInfo :: Lens.Lens' OutboundCrossClusterSearchConnection (Core.Maybe DomainInformation)
outboundCrossClusterSearchConnection_destinationDomainInfo = Lens.lens (\OutboundCrossClusterSearchConnection' {destinationDomainInfo} -> destinationDomainInfo) (\s@OutboundCrossClusterSearchConnection' {} a -> s {destinationDomainInfo = a} :: OutboundCrossClusterSearchConnection)

-- | Specifies the @OutboundCrossClusterSearchConnectionStatus@ for the
-- outbound connection.
outboundCrossClusterSearchConnection_connectionStatus :: Lens.Lens' OutboundCrossClusterSearchConnection (Core.Maybe OutboundCrossClusterSearchConnectionStatus)
outboundCrossClusterSearchConnection_connectionStatus = Lens.lens (\OutboundCrossClusterSearchConnection' {connectionStatus} -> connectionStatus) (\s@OutboundCrossClusterSearchConnection' {} a -> s {connectionStatus = a} :: OutboundCrossClusterSearchConnection)

instance
  Core.FromJSON
    OutboundCrossClusterSearchConnection
  where
  parseJSON =
    Core.withObject
      "OutboundCrossClusterSearchConnection"
      ( \x ->
          OutboundCrossClusterSearchConnection'
            Core.<$> (x Core..:? "CrossClusterSearchConnectionId")
            Core.<*> (x Core..:? "SourceDomainInfo")
            Core.<*> (x Core..:? "ConnectionAlias")
            Core.<*> (x Core..:? "DestinationDomainInfo")
            Core.<*> (x Core..:? "ConnectionStatus")
      )

instance
  Core.Hashable
    OutboundCrossClusterSearchConnection

instance
  Core.NFData
    OutboundCrossClusterSearchConnection
