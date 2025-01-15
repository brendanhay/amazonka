{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ElasticSearch.CreateOutboundCrossClusterSearchConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new cross-cluster search connection from a source domain to a
-- destination domain.
module Amazonka.ElasticSearch.CreateOutboundCrossClusterSearchConnection
  ( -- * Creating a Request
    CreateOutboundCrossClusterSearchConnection (..),
    newCreateOutboundCrossClusterSearchConnection,

    -- * Request Lenses
    createOutboundCrossClusterSearchConnection_sourceDomainInfo,
    createOutboundCrossClusterSearchConnection_destinationDomainInfo,
    createOutboundCrossClusterSearchConnection_connectionAlias,

    -- * Destructuring the Response
    CreateOutboundCrossClusterSearchConnectionResponse (..),
    newCreateOutboundCrossClusterSearchConnectionResponse,

    -- * Response Lenses
    createOutboundCrossClusterSearchConnectionResponse_connectionAlias,
    createOutboundCrossClusterSearchConnectionResponse_connectionStatus,
    createOutboundCrossClusterSearchConnectionResponse_crossClusterSearchConnectionId,
    createOutboundCrossClusterSearchConnectionResponse_destinationDomainInfo,
    createOutboundCrossClusterSearchConnectionResponse_sourceDomainInfo,
    createOutboundCrossClusterSearchConnectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the
-- @CreateOutboundCrossClusterSearchConnection@ operation.
--
-- /See:/ 'newCreateOutboundCrossClusterSearchConnection' smart constructor.
data CreateOutboundCrossClusterSearchConnection = CreateOutboundCrossClusterSearchConnection'
  { -- | Specifies the @DomainInformation@ for the source Elasticsearch domain.
    sourceDomainInfo :: DomainInformation,
    -- | Specifies the @DomainInformation@ for the destination Elasticsearch
    -- domain.
    destinationDomainInfo :: DomainInformation,
    -- | Specifies the connection alias that will be used by the customer for
    -- this connection.
    connectionAlias :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateOutboundCrossClusterSearchConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceDomainInfo', 'createOutboundCrossClusterSearchConnection_sourceDomainInfo' - Specifies the @DomainInformation@ for the source Elasticsearch domain.
--
-- 'destinationDomainInfo', 'createOutboundCrossClusterSearchConnection_destinationDomainInfo' - Specifies the @DomainInformation@ for the destination Elasticsearch
-- domain.
--
-- 'connectionAlias', 'createOutboundCrossClusterSearchConnection_connectionAlias' - Specifies the connection alias that will be used by the customer for
-- this connection.
newCreateOutboundCrossClusterSearchConnection ::
  -- | 'sourceDomainInfo'
  DomainInformation ->
  -- | 'destinationDomainInfo'
  DomainInformation ->
  -- | 'connectionAlias'
  Prelude.Text ->
  CreateOutboundCrossClusterSearchConnection
newCreateOutboundCrossClusterSearchConnection
  pSourceDomainInfo_
  pDestinationDomainInfo_
  pConnectionAlias_ =
    CreateOutboundCrossClusterSearchConnection'
      { sourceDomainInfo =
          pSourceDomainInfo_,
        destinationDomainInfo =
          pDestinationDomainInfo_,
        connectionAlias =
          pConnectionAlias_
      }

-- | Specifies the @DomainInformation@ for the source Elasticsearch domain.
createOutboundCrossClusterSearchConnection_sourceDomainInfo :: Lens.Lens' CreateOutboundCrossClusterSearchConnection DomainInformation
createOutboundCrossClusterSearchConnection_sourceDomainInfo = Lens.lens (\CreateOutboundCrossClusterSearchConnection' {sourceDomainInfo} -> sourceDomainInfo) (\s@CreateOutboundCrossClusterSearchConnection' {} a -> s {sourceDomainInfo = a} :: CreateOutboundCrossClusterSearchConnection)

-- | Specifies the @DomainInformation@ for the destination Elasticsearch
-- domain.
createOutboundCrossClusterSearchConnection_destinationDomainInfo :: Lens.Lens' CreateOutboundCrossClusterSearchConnection DomainInformation
createOutboundCrossClusterSearchConnection_destinationDomainInfo = Lens.lens (\CreateOutboundCrossClusterSearchConnection' {destinationDomainInfo} -> destinationDomainInfo) (\s@CreateOutboundCrossClusterSearchConnection' {} a -> s {destinationDomainInfo = a} :: CreateOutboundCrossClusterSearchConnection)

-- | Specifies the connection alias that will be used by the customer for
-- this connection.
createOutboundCrossClusterSearchConnection_connectionAlias :: Lens.Lens' CreateOutboundCrossClusterSearchConnection Prelude.Text
createOutboundCrossClusterSearchConnection_connectionAlias = Lens.lens (\CreateOutboundCrossClusterSearchConnection' {connectionAlias} -> connectionAlias) (\s@CreateOutboundCrossClusterSearchConnection' {} a -> s {connectionAlias = a} :: CreateOutboundCrossClusterSearchConnection)

instance
  Core.AWSRequest
    CreateOutboundCrossClusterSearchConnection
  where
  type
    AWSResponse
      CreateOutboundCrossClusterSearchConnection =
      CreateOutboundCrossClusterSearchConnectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateOutboundCrossClusterSearchConnectionResponse'
            Prelude.<$> (x Data..?> "ConnectionAlias")
            Prelude.<*> (x Data..?> "ConnectionStatus")
            Prelude.<*> (x Data..?> "CrossClusterSearchConnectionId")
            Prelude.<*> (x Data..?> "DestinationDomainInfo")
            Prelude.<*> (x Data..?> "SourceDomainInfo")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateOutboundCrossClusterSearchConnection
  where
  hashWithSalt
    _salt
    CreateOutboundCrossClusterSearchConnection' {..} =
      _salt
        `Prelude.hashWithSalt` sourceDomainInfo
        `Prelude.hashWithSalt` destinationDomainInfo
        `Prelude.hashWithSalt` connectionAlias

instance
  Prelude.NFData
    CreateOutboundCrossClusterSearchConnection
  where
  rnf CreateOutboundCrossClusterSearchConnection' {..} =
    Prelude.rnf sourceDomainInfo `Prelude.seq`
      Prelude.rnf destinationDomainInfo `Prelude.seq`
        Prelude.rnf connectionAlias

instance
  Data.ToHeaders
    CreateOutboundCrossClusterSearchConnection
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    CreateOutboundCrossClusterSearchConnection
  where
  toJSON
    CreateOutboundCrossClusterSearchConnection' {..} =
      Data.object
        ( Prelude.catMaybes
            [ Prelude.Just
                ("SourceDomainInfo" Data..= sourceDomainInfo),
              Prelude.Just
                ( "DestinationDomainInfo"
                    Data..= destinationDomainInfo
                ),
              Prelude.Just
                ("ConnectionAlias" Data..= connectionAlias)
            ]
        )

instance
  Data.ToPath
    CreateOutboundCrossClusterSearchConnection
  where
  toPath =
    Prelude.const
      "/2015-01-01/es/ccs/outboundConnection"

instance
  Data.ToQuery
    CreateOutboundCrossClusterSearchConnection
  where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a @CreateOutboundCrossClusterSearchConnection@ request.
-- Contains the details of the newly created cross-cluster search
-- connection.
--
-- /See:/ 'newCreateOutboundCrossClusterSearchConnectionResponse' smart constructor.
data CreateOutboundCrossClusterSearchConnectionResponse = CreateOutboundCrossClusterSearchConnectionResponse'
  { -- | Specifies the connection alias provided during the create connection
    -- request.
    connectionAlias :: Prelude.Maybe Prelude.Text,
    -- | Specifies the @OutboundCrossClusterSearchConnectionStatus@ for the newly
    -- created connection.
    connectionStatus :: Prelude.Maybe OutboundCrossClusterSearchConnectionStatus,
    -- | Unique id for the created outbound connection, which is used for
    -- subsequent operations on connection.
    crossClusterSearchConnectionId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the @DomainInformation@ for the destination Elasticsearch
    -- domain.
    destinationDomainInfo :: Prelude.Maybe DomainInformation,
    -- | Specifies the @DomainInformation@ for the source Elasticsearch domain.
    sourceDomainInfo :: Prelude.Maybe DomainInformation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateOutboundCrossClusterSearchConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionAlias', 'createOutboundCrossClusterSearchConnectionResponse_connectionAlias' - Specifies the connection alias provided during the create connection
-- request.
--
-- 'connectionStatus', 'createOutboundCrossClusterSearchConnectionResponse_connectionStatus' - Specifies the @OutboundCrossClusterSearchConnectionStatus@ for the newly
-- created connection.
--
-- 'crossClusterSearchConnectionId', 'createOutboundCrossClusterSearchConnectionResponse_crossClusterSearchConnectionId' - Unique id for the created outbound connection, which is used for
-- subsequent operations on connection.
--
-- 'destinationDomainInfo', 'createOutboundCrossClusterSearchConnectionResponse_destinationDomainInfo' - Specifies the @DomainInformation@ for the destination Elasticsearch
-- domain.
--
-- 'sourceDomainInfo', 'createOutboundCrossClusterSearchConnectionResponse_sourceDomainInfo' - Specifies the @DomainInformation@ for the source Elasticsearch domain.
--
-- 'httpStatus', 'createOutboundCrossClusterSearchConnectionResponse_httpStatus' - The response's http status code.
newCreateOutboundCrossClusterSearchConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateOutboundCrossClusterSearchConnectionResponse
newCreateOutboundCrossClusterSearchConnectionResponse
  pHttpStatus_ =
    CreateOutboundCrossClusterSearchConnectionResponse'
      { connectionAlias =
          Prelude.Nothing,
        connectionStatus =
          Prelude.Nothing,
        crossClusterSearchConnectionId =
          Prelude.Nothing,
        destinationDomainInfo =
          Prelude.Nothing,
        sourceDomainInfo =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Specifies the connection alias provided during the create connection
-- request.
createOutboundCrossClusterSearchConnectionResponse_connectionAlias :: Lens.Lens' CreateOutboundCrossClusterSearchConnectionResponse (Prelude.Maybe Prelude.Text)
createOutboundCrossClusterSearchConnectionResponse_connectionAlias = Lens.lens (\CreateOutboundCrossClusterSearchConnectionResponse' {connectionAlias} -> connectionAlias) (\s@CreateOutboundCrossClusterSearchConnectionResponse' {} a -> s {connectionAlias = a} :: CreateOutboundCrossClusterSearchConnectionResponse)

-- | Specifies the @OutboundCrossClusterSearchConnectionStatus@ for the newly
-- created connection.
createOutboundCrossClusterSearchConnectionResponse_connectionStatus :: Lens.Lens' CreateOutboundCrossClusterSearchConnectionResponse (Prelude.Maybe OutboundCrossClusterSearchConnectionStatus)
createOutboundCrossClusterSearchConnectionResponse_connectionStatus = Lens.lens (\CreateOutboundCrossClusterSearchConnectionResponse' {connectionStatus} -> connectionStatus) (\s@CreateOutboundCrossClusterSearchConnectionResponse' {} a -> s {connectionStatus = a} :: CreateOutboundCrossClusterSearchConnectionResponse)

-- | Unique id for the created outbound connection, which is used for
-- subsequent operations on connection.
createOutboundCrossClusterSearchConnectionResponse_crossClusterSearchConnectionId :: Lens.Lens' CreateOutboundCrossClusterSearchConnectionResponse (Prelude.Maybe Prelude.Text)
createOutboundCrossClusterSearchConnectionResponse_crossClusterSearchConnectionId = Lens.lens (\CreateOutboundCrossClusterSearchConnectionResponse' {crossClusterSearchConnectionId} -> crossClusterSearchConnectionId) (\s@CreateOutboundCrossClusterSearchConnectionResponse' {} a -> s {crossClusterSearchConnectionId = a} :: CreateOutboundCrossClusterSearchConnectionResponse)

-- | Specifies the @DomainInformation@ for the destination Elasticsearch
-- domain.
createOutboundCrossClusterSearchConnectionResponse_destinationDomainInfo :: Lens.Lens' CreateOutboundCrossClusterSearchConnectionResponse (Prelude.Maybe DomainInformation)
createOutboundCrossClusterSearchConnectionResponse_destinationDomainInfo = Lens.lens (\CreateOutboundCrossClusterSearchConnectionResponse' {destinationDomainInfo} -> destinationDomainInfo) (\s@CreateOutboundCrossClusterSearchConnectionResponse' {} a -> s {destinationDomainInfo = a} :: CreateOutboundCrossClusterSearchConnectionResponse)

-- | Specifies the @DomainInformation@ for the source Elasticsearch domain.
createOutboundCrossClusterSearchConnectionResponse_sourceDomainInfo :: Lens.Lens' CreateOutboundCrossClusterSearchConnectionResponse (Prelude.Maybe DomainInformation)
createOutboundCrossClusterSearchConnectionResponse_sourceDomainInfo = Lens.lens (\CreateOutboundCrossClusterSearchConnectionResponse' {sourceDomainInfo} -> sourceDomainInfo) (\s@CreateOutboundCrossClusterSearchConnectionResponse' {} a -> s {sourceDomainInfo = a} :: CreateOutboundCrossClusterSearchConnectionResponse)

-- | The response's http status code.
createOutboundCrossClusterSearchConnectionResponse_httpStatus :: Lens.Lens' CreateOutboundCrossClusterSearchConnectionResponse Prelude.Int
createOutboundCrossClusterSearchConnectionResponse_httpStatus = Lens.lens (\CreateOutboundCrossClusterSearchConnectionResponse' {httpStatus} -> httpStatus) (\s@CreateOutboundCrossClusterSearchConnectionResponse' {} a -> s {httpStatus = a} :: CreateOutboundCrossClusterSearchConnectionResponse)

instance
  Prelude.NFData
    CreateOutboundCrossClusterSearchConnectionResponse
  where
  rnf
    CreateOutboundCrossClusterSearchConnectionResponse' {..} =
      Prelude.rnf connectionAlias `Prelude.seq`
        Prelude.rnf connectionStatus `Prelude.seq`
          Prelude.rnf crossClusterSearchConnectionId `Prelude.seq`
            Prelude.rnf destinationDomainInfo `Prelude.seq`
              Prelude.rnf sourceDomainInfo `Prelude.seq`
                Prelude.rnf httpStatus
