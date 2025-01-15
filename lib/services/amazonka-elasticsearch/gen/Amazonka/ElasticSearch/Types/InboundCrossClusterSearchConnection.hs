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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
  { -- | Specifies the @InboundCrossClusterSearchConnectionStatus@ for the
    -- outbound connection.
    connectionStatus :: Prelude.Maybe InboundCrossClusterSearchConnectionStatus,
    -- | Specifies the connection id for the inbound cross-cluster search
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
-- Create a value of 'InboundCrossClusterSearchConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionStatus', 'inboundCrossClusterSearchConnection_connectionStatus' - Specifies the @InboundCrossClusterSearchConnectionStatus@ for the
-- outbound connection.
--
-- 'crossClusterSearchConnectionId', 'inboundCrossClusterSearchConnection_crossClusterSearchConnectionId' - Specifies the connection id for the inbound cross-cluster search
-- connection.
--
-- 'destinationDomainInfo', 'inboundCrossClusterSearchConnection_destinationDomainInfo' - Specifies the @DomainInformation@ for the destination Elasticsearch
-- domain.
--
-- 'sourceDomainInfo', 'inboundCrossClusterSearchConnection_sourceDomainInfo' - Specifies the @DomainInformation@ for the source Elasticsearch domain.
newInboundCrossClusterSearchConnection ::
  InboundCrossClusterSearchConnection
newInboundCrossClusterSearchConnection =
  InboundCrossClusterSearchConnection'
    { connectionStatus =
        Prelude.Nothing,
      crossClusterSearchConnectionId =
        Prelude.Nothing,
      destinationDomainInfo =
        Prelude.Nothing,
      sourceDomainInfo = Prelude.Nothing
    }

-- | Specifies the @InboundCrossClusterSearchConnectionStatus@ for the
-- outbound connection.
inboundCrossClusterSearchConnection_connectionStatus :: Lens.Lens' InboundCrossClusterSearchConnection (Prelude.Maybe InboundCrossClusterSearchConnectionStatus)
inboundCrossClusterSearchConnection_connectionStatus = Lens.lens (\InboundCrossClusterSearchConnection' {connectionStatus} -> connectionStatus) (\s@InboundCrossClusterSearchConnection' {} a -> s {connectionStatus = a} :: InboundCrossClusterSearchConnection)

-- | Specifies the connection id for the inbound cross-cluster search
-- connection.
inboundCrossClusterSearchConnection_crossClusterSearchConnectionId :: Lens.Lens' InboundCrossClusterSearchConnection (Prelude.Maybe Prelude.Text)
inboundCrossClusterSearchConnection_crossClusterSearchConnectionId = Lens.lens (\InboundCrossClusterSearchConnection' {crossClusterSearchConnectionId} -> crossClusterSearchConnectionId) (\s@InboundCrossClusterSearchConnection' {} a -> s {crossClusterSearchConnectionId = a} :: InboundCrossClusterSearchConnection)

-- | Specifies the @DomainInformation@ for the destination Elasticsearch
-- domain.
inboundCrossClusterSearchConnection_destinationDomainInfo :: Lens.Lens' InboundCrossClusterSearchConnection (Prelude.Maybe DomainInformation)
inboundCrossClusterSearchConnection_destinationDomainInfo = Lens.lens (\InboundCrossClusterSearchConnection' {destinationDomainInfo} -> destinationDomainInfo) (\s@InboundCrossClusterSearchConnection' {} a -> s {destinationDomainInfo = a} :: InboundCrossClusterSearchConnection)

-- | Specifies the @DomainInformation@ for the source Elasticsearch domain.
inboundCrossClusterSearchConnection_sourceDomainInfo :: Lens.Lens' InboundCrossClusterSearchConnection (Prelude.Maybe DomainInformation)
inboundCrossClusterSearchConnection_sourceDomainInfo = Lens.lens (\InboundCrossClusterSearchConnection' {sourceDomainInfo} -> sourceDomainInfo) (\s@InboundCrossClusterSearchConnection' {} a -> s {sourceDomainInfo = a} :: InboundCrossClusterSearchConnection)

instance
  Data.FromJSON
    InboundCrossClusterSearchConnection
  where
  parseJSON =
    Data.withObject
      "InboundCrossClusterSearchConnection"
      ( \x ->
          InboundCrossClusterSearchConnection'
            Prelude.<$> (x Data..:? "ConnectionStatus")
            Prelude.<*> (x Data..:? "CrossClusterSearchConnectionId")
            Prelude.<*> (x Data..:? "DestinationDomainInfo")
            Prelude.<*> (x Data..:? "SourceDomainInfo")
      )

instance
  Prelude.Hashable
    InboundCrossClusterSearchConnection
  where
  hashWithSalt
    _salt
    InboundCrossClusterSearchConnection' {..} =
      _salt
        `Prelude.hashWithSalt` connectionStatus
        `Prelude.hashWithSalt` crossClusterSearchConnectionId
        `Prelude.hashWithSalt` destinationDomainInfo
        `Prelude.hashWithSalt` sourceDomainInfo

instance
  Prelude.NFData
    InboundCrossClusterSearchConnection
  where
  rnf InboundCrossClusterSearchConnection' {..} =
    Prelude.rnf connectionStatus `Prelude.seq`
      Prelude.rnf crossClusterSearchConnectionId `Prelude.seq`
        Prelude.rnf destinationDomainInfo `Prelude.seq`
          Prelude.rnf sourceDomainInfo
