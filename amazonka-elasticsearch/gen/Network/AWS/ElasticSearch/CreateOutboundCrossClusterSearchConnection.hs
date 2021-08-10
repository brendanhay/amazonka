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
-- Module      : Network.AWS.ElasticSearch.CreateOutboundCrossClusterSearchConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new cross-cluster search connection from a source domain to a
-- destination domain.
module Network.AWS.ElasticSearch.CreateOutboundCrossClusterSearchConnection
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
    createOutboundCrossClusterSearchConnectionResponse_crossClusterSearchConnectionId,
    createOutboundCrossClusterSearchConnectionResponse_sourceDomainInfo,
    createOutboundCrossClusterSearchConnectionResponse_connectionAlias,
    createOutboundCrossClusterSearchConnectionResponse_destinationDomainInfo,
    createOutboundCrossClusterSearchConnectionResponse_connectionStatus,
    createOutboundCrossClusterSearchConnectionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateOutboundCrossClusterSearchConnectionResponse'
            Prelude.<$> (x Core..?> "CrossClusterSearchConnectionId")
              Prelude.<*> (x Core..?> "SourceDomainInfo")
              Prelude.<*> (x Core..?> "ConnectionAlias")
              Prelude.<*> (x Core..?> "DestinationDomainInfo")
              Prelude.<*> (x Core..?> "ConnectionStatus")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateOutboundCrossClusterSearchConnection

instance
  Prelude.NFData
    CreateOutboundCrossClusterSearchConnection

instance
  Core.ToHeaders
    CreateOutboundCrossClusterSearchConnection
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToJSON
    CreateOutboundCrossClusterSearchConnection
  where
  toJSON
    CreateOutboundCrossClusterSearchConnection' {..} =
      Core.object
        ( Prelude.catMaybes
            [ Prelude.Just
                ("SourceDomainInfo" Core..= sourceDomainInfo),
              Prelude.Just
                ( "DestinationDomainInfo"
                    Core..= destinationDomainInfo
                ),
              Prelude.Just
                ("ConnectionAlias" Core..= connectionAlias)
            ]
        )

instance
  Core.ToPath
    CreateOutboundCrossClusterSearchConnection
  where
  toPath =
    Prelude.const
      "/2015-01-01/es/ccs/outboundConnection"

instance
  Core.ToQuery
    CreateOutboundCrossClusterSearchConnection
  where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a @CreateOutboundCrossClusterSearchConnection@ request.
-- Contains the details of the newly created cross-cluster search
-- connection.
--
-- /See:/ 'newCreateOutboundCrossClusterSearchConnectionResponse' smart constructor.
data CreateOutboundCrossClusterSearchConnectionResponse = CreateOutboundCrossClusterSearchConnectionResponse'
  { -- | Unique id for the created outbound connection, which is used for
    -- subsequent operations on connection.
    crossClusterSearchConnectionId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the @DomainInformation@ for the source Elasticsearch domain.
    sourceDomainInfo :: Prelude.Maybe DomainInformation,
    -- | Specifies the connection alias provided during the create connection
    -- request.
    connectionAlias :: Prelude.Maybe Prelude.Text,
    -- | Specifies the @DomainInformation@ for the destination Elasticsearch
    -- domain.
    destinationDomainInfo :: Prelude.Maybe DomainInformation,
    -- | Specifies the @OutboundCrossClusterSearchConnectionStatus@ for the newly
    -- created connection.
    connectionStatus :: Prelude.Maybe OutboundCrossClusterSearchConnectionStatus,
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
-- 'crossClusterSearchConnectionId', 'createOutboundCrossClusterSearchConnectionResponse_crossClusterSearchConnectionId' - Unique id for the created outbound connection, which is used for
-- subsequent operations on connection.
--
-- 'sourceDomainInfo', 'createOutboundCrossClusterSearchConnectionResponse_sourceDomainInfo' - Specifies the @DomainInformation@ for the source Elasticsearch domain.
--
-- 'connectionAlias', 'createOutboundCrossClusterSearchConnectionResponse_connectionAlias' - Specifies the connection alias provided during the create connection
-- request.
--
-- 'destinationDomainInfo', 'createOutboundCrossClusterSearchConnectionResponse_destinationDomainInfo' - Specifies the @DomainInformation@ for the destination Elasticsearch
-- domain.
--
-- 'connectionStatus', 'createOutboundCrossClusterSearchConnectionResponse_connectionStatus' - Specifies the @OutboundCrossClusterSearchConnectionStatus@ for the newly
-- created connection.
--
-- 'httpStatus', 'createOutboundCrossClusterSearchConnectionResponse_httpStatus' - The response's http status code.
newCreateOutboundCrossClusterSearchConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateOutboundCrossClusterSearchConnectionResponse
newCreateOutboundCrossClusterSearchConnectionResponse
  pHttpStatus_ =
    CreateOutboundCrossClusterSearchConnectionResponse'
      { crossClusterSearchConnectionId =
          Prelude.Nothing,
        sourceDomainInfo =
          Prelude.Nothing,
        connectionAlias =
          Prelude.Nothing,
        destinationDomainInfo =
          Prelude.Nothing,
        connectionStatus =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Unique id for the created outbound connection, which is used for
-- subsequent operations on connection.
createOutboundCrossClusterSearchConnectionResponse_crossClusterSearchConnectionId :: Lens.Lens' CreateOutboundCrossClusterSearchConnectionResponse (Prelude.Maybe Prelude.Text)
createOutboundCrossClusterSearchConnectionResponse_crossClusterSearchConnectionId = Lens.lens (\CreateOutboundCrossClusterSearchConnectionResponse' {crossClusterSearchConnectionId} -> crossClusterSearchConnectionId) (\s@CreateOutboundCrossClusterSearchConnectionResponse' {} a -> s {crossClusterSearchConnectionId = a} :: CreateOutboundCrossClusterSearchConnectionResponse)

-- | Specifies the @DomainInformation@ for the source Elasticsearch domain.
createOutboundCrossClusterSearchConnectionResponse_sourceDomainInfo :: Lens.Lens' CreateOutboundCrossClusterSearchConnectionResponse (Prelude.Maybe DomainInformation)
createOutboundCrossClusterSearchConnectionResponse_sourceDomainInfo = Lens.lens (\CreateOutboundCrossClusterSearchConnectionResponse' {sourceDomainInfo} -> sourceDomainInfo) (\s@CreateOutboundCrossClusterSearchConnectionResponse' {} a -> s {sourceDomainInfo = a} :: CreateOutboundCrossClusterSearchConnectionResponse)

-- | Specifies the connection alias provided during the create connection
-- request.
createOutboundCrossClusterSearchConnectionResponse_connectionAlias :: Lens.Lens' CreateOutboundCrossClusterSearchConnectionResponse (Prelude.Maybe Prelude.Text)
createOutboundCrossClusterSearchConnectionResponse_connectionAlias = Lens.lens (\CreateOutboundCrossClusterSearchConnectionResponse' {connectionAlias} -> connectionAlias) (\s@CreateOutboundCrossClusterSearchConnectionResponse' {} a -> s {connectionAlias = a} :: CreateOutboundCrossClusterSearchConnectionResponse)

-- | Specifies the @DomainInformation@ for the destination Elasticsearch
-- domain.
createOutboundCrossClusterSearchConnectionResponse_destinationDomainInfo :: Lens.Lens' CreateOutboundCrossClusterSearchConnectionResponse (Prelude.Maybe DomainInformation)
createOutboundCrossClusterSearchConnectionResponse_destinationDomainInfo = Lens.lens (\CreateOutboundCrossClusterSearchConnectionResponse' {destinationDomainInfo} -> destinationDomainInfo) (\s@CreateOutboundCrossClusterSearchConnectionResponse' {} a -> s {destinationDomainInfo = a} :: CreateOutboundCrossClusterSearchConnectionResponse)

-- | Specifies the @OutboundCrossClusterSearchConnectionStatus@ for the newly
-- created connection.
createOutboundCrossClusterSearchConnectionResponse_connectionStatus :: Lens.Lens' CreateOutboundCrossClusterSearchConnectionResponse (Prelude.Maybe OutboundCrossClusterSearchConnectionStatus)
createOutboundCrossClusterSearchConnectionResponse_connectionStatus = Lens.lens (\CreateOutboundCrossClusterSearchConnectionResponse' {connectionStatus} -> connectionStatus) (\s@CreateOutboundCrossClusterSearchConnectionResponse' {} a -> s {connectionStatus = a} :: CreateOutboundCrossClusterSearchConnectionResponse)

-- | The response's http status code.
createOutboundCrossClusterSearchConnectionResponse_httpStatus :: Lens.Lens' CreateOutboundCrossClusterSearchConnectionResponse Prelude.Int
createOutboundCrossClusterSearchConnectionResponse_httpStatus = Lens.lens (\CreateOutboundCrossClusterSearchConnectionResponse' {httpStatus} -> httpStatus) (\s@CreateOutboundCrossClusterSearchConnectionResponse' {} a -> s {httpStatus = a} :: CreateOutboundCrossClusterSearchConnectionResponse)

instance
  Prelude.NFData
    CreateOutboundCrossClusterSearchConnectionResponse
