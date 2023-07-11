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
-- Module      : Amazonka.OpenSearch.CreateOutboundConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new cross-cluster search connection from a source Amazon
-- OpenSearch Service domain to a destination domain. For more information,
-- see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/cross-cluster-search.html Cross-cluster search for Amazon OpenSearch Service>.
module Amazonka.OpenSearch.CreateOutboundConnection
  ( -- * Creating a Request
    CreateOutboundConnection (..),
    newCreateOutboundConnection,

    -- * Request Lenses
    createOutboundConnection_localDomainInfo,
    createOutboundConnection_remoteDomainInfo,
    createOutboundConnection_connectionAlias,

    -- * Destructuring the Response
    CreateOutboundConnectionResponse (..),
    newCreateOutboundConnectionResponse,

    -- * Response Lenses
    createOutboundConnectionResponse_connectionAlias,
    createOutboundConnectionResponse_connectionId,
    createOutboundConnectionResponse_connectionStatus,
    createOutboundConnectionResponse_localDomainInfo,
    createOutboundConnectionResponse_remoteDomainInfo,
    createOutboundConnectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @CreateOutboundConnection@
-- operation.
--
-- /See:/ 'newCreateOutboundConnection' smart constructor.
data CreateOutboundConnection = CreateOutboundConnection'
  { -- | Name and Region of the source (local) domain.
    localDomainInfo :: DomainInformationContainer,
    -- | Name and Region of the destination (remote) domain.
    remoteDomainInfo :: DomainInformationContainer,
    -- | Name of the connection.
    connectionAlias :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateOutboundConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'localDomainInfo', 'createOutboundConnection_localDomainInfo' - Name and Region of the source (local) domain.
--
-- 'remoteDomainInfo', 'createOutboundConnection_remoteDomainInfo' - Name and Region of the destination (remote) domain.
--
-- 'connectionAlias', 'createOutboundConnection_connectionAlias' - Name of the connection.
newCreateOutboundConnection ::
  -- | 'localDomainInfo'
  DomainInformationContainer ->
  -- | 'remoteDomainInfo'
  DomainInformationContainer ->
  -- | 'connectionAlias'
  Prelude.Text ->
  CreateOutboundConnection
newCreateOutboundConnection
  pLocalDomainInfo_
  pRemoteDomainInfo_
  pConnectionAlias_ =
    CreateOutboundConnection'
      { localDomainInfo =
          pLocalDomainInfo_,
        remoteDomainInfo = pRemoteDomainInfo_,
        connectionAlias = pConnectionAlias_
      }

-- | Name and Region of the source (local) domain.
createOutboundConnection_localDomainInfo :: Lens.Lens' CreateOutboundConnection DomainInformationContainer
createOutboundConnection_localDomainInfo = Lens.lens (\CreateOutboundConnection' {localDomainInfo} -> localDomainInfo) (\s@CreateOutboundConnection' {} a -> s {localDomainInfo = a} :: CreateOutboundConnection)

-- | Name and Region of the destination (remote) domain.
createOutboundConnection_remoteDomainInfo :: Lens.Lens' CreateOutboundConnection DomainInformationContainer
createOutboundConnection_remoteDomainInfo = Lens.lens (\CreateOutboundConnection' {remoteDomainInfo} -> remoteDomainInfo) (\s@CreateOutboundConnection' {} a -> s {remoteDomainInfo = a} :: CreateOutboundConnection)

-- | Name of the connection.
createOutboundConnection_connectionAlias :: Lens.Lens' CreateOutboundConnection Prelude.Text
createOutboundConnection_connectionAlias = Lens.lens (\CreateOutboundConnection' {connectionAlias} -> connectionAlias) (\s@CreateOutboundConnection' {} a -> s {connectionAlias = a} :: CreateOutboundConnection)

instance Core.AWSRequest CreateOutboundConnection where
  type
    AWSResponse CreateOutboundConnection =
      CreateOutboundConnectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateOutboundConnectionResponse'
            Prelude.<$> (x Data..?> "ConnectionAlias")
            Prelude.<*> (x Data..?> "ConnectionId")
            Prelude.<*> (x Data..?> "ConnectionStatus")
            Prelude.<*> (x Data..?> "LocalDomainInfo")
            Prelude.<*> (x Data..?> "RemoteDomainInfo")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateOutboundConnection where
  hashWithSalt _salt CreateOutboundConnection' {..} =
    _salt
      `Prelude.hashWithSalt` localDomainInfo
      `Prelude.hashWithSalt` remoteDomainInfo
      `Prelude.hashWithSalt` connectionAlias

instance Prelude.NFData CreateOutboundConnection where
  rnf CreateOutboundConnection' {..} =
    Prelude.rnf localDomainInfo
      `Prelude.seq` Prelude.rnf remoteDomainInfo
      `Prelude.seq` Prelude.rnf connectionAlias

instance Data.ToHeaders CreateOutboundConnection where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateOutboundConnection where
  toJSON CreateOutboundConnection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("LocalDomainInfo" Data..= localDomainInfo),
            Prelude.Just
              ("RemoteDomainInfo" Data..= remoteDomainInfo),
            Prelude.Just
              ("ConnectionAlias" Data..= connectionAlias)
          ]
      )

instance Data.ToPath CreateOutboundConnection where
  toPath =
    Prelude.const
      "/2021-01-01/opensearch/cc/outboundConnection"

instance Data.ToQuery CreateOutboundConnection where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a @CreateOutboundConnection@ request. Contains details
-- about the newly created cross-cluster connection.
--
-- /See:/ 'newCreateOutboundConnectionResponse' smart constructor.
data CreateOutboundConnectionResponse = CreateOutboundConnectionResponse'
  { -- | Name of the connection.
    connectionAlias :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the created outbound connection, which is used
    -- for subsequent operations on the connection.
    connectionId :: Prelude.Maybe Prelude.Text,
    -- | The status of the connection.
    connectionStatus :: Prelude.Maybe OutboundConnectionStatus,
    -- | Information about the source (local) domain.
    localDomainInfo :: Prelude.Maybe DomainInformationContainer,
    -- | Information about the destination (remote) domain.
    remoteDomainInfo :: Prelude.Maybe DomainInformationContainer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateOutboundConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionAlias', 'createOutboundConnectionResponse_connectionAlias' - Name of the connection.
--
-- 'connectionId', 'createOutboundConnectionResponse_connectionId' - The unique identifier for the created outbound connection, which is used
-- for subsequent operations on the connection.
--
-- 'connectionStatus', 'createOutboundConnectionResponse_connectionStatus' - The status of the connection.
--
-- 'localDomainInfo', 'createOutboundConnectionResponse_localDomainInfo' - Information about the source (local) domain.
--
-- 'remoteDomainInfo', 'createOutboundConnectionResponse_remoteDomainInfo' - Information about the destination (remote) domain.
--
-- 'httpStatus', 'createOutboundConnectionResponse_httpStatus' - The response's http status code.
newCreateOutboundConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateOutboundConnectionResponse
newCreateOutboundConnectionResponse pHttpStatus_ =
  CreateOutboundConnectionResponse'
    { connectionAlias =
        Prelude.Nothing,
      connectionId = Prelude.Nothing,
      connectionStatus = Prelude.Nothing,
      localDomainInfo = Prelude.Nothing,
      remoteDomainInfo = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Name of the connection.
createOutboundConnectionResponse_connectionAlias :: Lens.Lens' CreateOutboundConnectionResponse (Prelude.Maybe Prelude.Text)
createOutboundConnectionResponse_connectionAlias = Lens.lens (\CreateOutboundConnectionResponse' {connectionAlias} -> connectionAlias) (\s@CreateOutboundConnectionResponse' {} a -> s {connectionAlias = a} :: CreateOutboundConnectionResponse)

-- | The unique identifier for the created outbound connection, which is used
-- for subsequent operations on the connection.
createOutboundConnectionResponse_connectionId :: Lens.Lens' CreateOutboundConnectionResponse (Prelude.Maybe Prelude.Text)
createOutboundConnectionResponse_connectionId = Lens.lens (\CreateOutboundConnectionResponse' {connectionId} -> connectionId) (\s@CreateOutboundConnectionResponse' {} a -> s {connectionId = a} :: CreateOutboundConnectionResponse)

-- | The status of the connection.
createOutboundConnectionResponse_connectionStatus :: Lens.Lens' CreateOutboundConnectionResponse (Prelude.Maybe OutboundConnectionStatus)
createOutboundConnectionResponse_connectionStatus = Lens.lens (\CreateOutboundConnectionResponse' {connectionStatus} -> connectionStatus) (\s@CreateOutboundConnectionResponse' {} a -> s {connectionStatus = a} :: CreateOutboundConnectionResponse)

-- | Information about the source (local) domain.
createOutboundConnectionResponse_localDomainInfo :: Lens.Lens' CreateOutboundConnectionResponse (Prelude.Maybe DomainInformationContainer)
createOutboundConnectionResponse_localDomainInfo = Lens.lens (\CreateOutboundConnectionResponse' {localDomainInfo} -> localDomainInfo) (\s@CreateOutboundConnectionResponse' {} a -> s {localDomainInfo = a} :: CreateOutboundConnectionResponse)

-- | Information about the destination (remote) domain.
createOutboundConnectionResponse_remoteDomainInfo :: Lens.Lens' CreateOutboundConnectionResponse (Prelude.Maybe DomainInformationContainer)
createOutboundConnectionResponse_remoteDomainInfo = Lens.lens (\CreateOutboundConnectionResponse' {remoteDomainInfo} -> remoteDomainInfo) (\s@CreateOutboundConnectionResponse' {} a -> s {remoteDomainInfo = a} :: CreateOutboundConnectionResponse)

-- | The response's http status code.
createOutboundConnectionResponse_httpStatus :: Lens.Lens' CreateOutboundConnectionResponse Prelude.Int
createOutboundConnectionResponse_httpStatus = Lens.lens (\CreateOutboundConnectionResponse' {httpStatus} -> httpStatus) (\s@CreateOutboundConnectionResponse' {} a -> s {httpStatus = a} :: CreateOutboundConnectionResponse)

instance
  Prelude.NFData
    CreateOutboundConnectionResponse
  where
  rnf CreateOutboundConnectionResponse' {..} =
    Prelude.rnf connectionAlias
      `Prelude.seq` Prelude.rnf connectionId
      `Prelude.seq` Prelude.rnf connectionStatus
      `Prelude.seq` Prelude.rnf localDomainInfo
      `Prelude.seq` Prelude.rnf remoteDomainInfo
      `Prelude.seq` Prelude.rnf httpStatus
