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
-- Module      : Amazonka.ElasticSearch.Types.InboundCrossClusterSearchConnection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.InboundCrossClusterSearchConnection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types.DomainInformation
import Amazonka.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatus
import qualified Amazonka.Prelude as Prelude

-- | Specifies details of an inbound connection.
--
-- /See:/ 'newInboundCrossClusterSearchConnection' smart constructor.
data InboundCrossClusterSearchConnection = InboundCrossClusterSearchConnection'
  { -- | Specifies the connection id for the inbound cross-cluster search
    -- connection.
    crossClusterSearchConnectionId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the @DomainInformation@ for the source Elasticsearch domain.
    sourceDomainInfo :: Prelude.Maybe DomainInformation,
    -- | Specifies the @InboundCrossClusterSearchConnectionStatus@ for the
    -- outbound connection.
    connectionStatus :: Prelude.Maybe InboundCrossClusterSearchConnectionStatus,
    -- | Specifies the @DomainInformation@ for the destination Elasticsearch
    -- domain.
    destinationDomainInfo :: Prelude.Maybe DomainInformation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InboundCrossClusterSearchConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crossClusterSearchConnectionId', 'inboundCrossClusterSearchConnection_crossClusterSearchConnectionId' - Specifies the connection id for the inbound cross-cluster search
-- connection.
--
-- 'sourceDomainInfo', 'inboundCrossClusterSearchConnection_sourceDomainInfo' - Specifies the @DomainInformation@ for the source Elasticsearch domain.
--
-- 'connectionStatus', 'inboundCrossClusterSearchConnection_connectionStatus' - Specifies the @InboundCrossClusterSearchConnectionStatus@ for the
-- outbound connection.
--
-- 'destinationDomainInfo', 'inboundCrossClusterSearchConnection_destinationDomainInfo' - Specifies the @DomainInformation@ for the destination Elasticsearch
-- domain.
newInboundCrossClusterSearchConnection ::
  InboundCrossClusterSearchConnection
newInboundCrossClusterSearchConnection =
  InboundCrossClusterSearchConnection'
    { crossClusterSearchConnectionId =
        Prelude.Nothing,
      sourceDomainInfo = Prelude.Nothing,
      connectionStatus = Prelude.Nothing,
      destinationDomainInfo =
        Prelude.Nothing
    }

-- | Specifies the connection id for the inbound cross-cluster search
-- connection.
inboundCrossClusterSearchConnection_crossClusterSearchConnectionId :: Lens.Lens' InboundCrossClusterSearchConnection (Prelude.Maybe Prelude.Text)
inboundCrossClusterSearchConnection_crossClusterSearchConnectionId = Lens.lens (\InboundCrossClusterSearchConnection' {crossClusterSearchConnectionId} -> crossClusterSearchConnectionId) (\s@InboundCrossClusterSearchConnection' {} a -> s {crossClusterSearchConnectionId = a} :: InboundCrossClusterSearchConnection)

-- | Specifies the @DomainInformation@ for the source Elasticsearch domain.
inboundCrossClusterSearchConnection_sourceDomainInfo :: Lens.Lens' InboundCrossClusterSearchConnection (Prelude.Maybe DomainInformation)
inboundCrossClusterSearchConnection_sourceDomainInfo = Lens.lens (\InboundCrossClusterSearchConnection' {sourceDomainInfo} -> sourceDomainInfo) (\s@InboundCrossClusterSearchConnection' {} a -> s {sourceDomainInfo = a} :: InboundCrossClusterSearchConnection)

-- | Specifies the @InboundCrossClusterSearchConnectionStatus@ for the
-- outbound connection.
inboundCrossClusterSearchConnection_connectionStatus :: Lens.Lens' InboundCrossClusterSearchConnection (Prelude.Maybe InboundCrossClusterSearchConnectionStatus)
inboundCrossClusterSearchConnection_connectionStatus = Lens.lens (\InboundCrossClusterSearchConnection' {connectionStatus} -> connectionStatus) (\s@InboundCrossClusterSearchConnection' {} a -> s {connectionStatus = a} :: InboundCrossClusterSearchConnection)

-- | Specifies the @DomainInformation@ for the destination Elasticsearch
-- domain.
inboundCrossClusterSearchConnection_destinationDomainInfo :: Lens.Lens' InboundCrossClusterSearchConnection (Prelude.Maybe DomainInformation)
inboundCrossClusterSearchConnection_destinationDomainInfo = Lens.lens (\InboundCrossClusterSearchConnection' {destinationDomainInfo} -> destinationDomainInfo) (\s@InboundCrossClusterSearchConnection' {} a -> s {destinationDomainInfo = a} :: InboundCrossClusterSearchConnection)

instance
  Data.FromJSON
    InboundCrossClusterSearchConnection
  where
  parseJSON =
    Data.withObject
      "InboundCrossClusterSearchConnection"
      ( \x ->
          InboundCrossClusterSearchConnection'
            Prelude.<$> (x Data..:? "CrossClusterSearchConnectionId")
            Prelude.<*> (x Data..:? "SourceDomainInfo")
            Prelude.<*> (x Data..:? "ConnectionStatus")
            Prelude.<*> (x Data..:? "DestinationDomainInfo")
      )

instance
  Prelude.Hashable
    InboundCrossClusterSearchConnection
  where
  hashWithSalt
    _salt
    InboundCrossClusterSearchConnection' {..} =
      _salt
        `Prelude.hashWithSalt` crossClusterSearchConnectionId
        `Prelude.hashWithSalt` sourceDomainInfo
        `Prelude.hashWithSalt` connectionStatus
        `Prelude.hashWithSalt` destinationDomainInfo

instance
  Prelude.NFData
    InboundCrossClusterSearchConnection
  where
  rnf InboundCrossClusterSearchConnection' {..} =
    Prelude.rnf crossClusterSearchConnectionId
      `Prelude.seq` Prelude.rnf sourceDomainInfo
      `Prelude.seq` Prelude.rnf connectionStatus
      `Prelude.seq` Prelude.rnf destinationDomainInfo
