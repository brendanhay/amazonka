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
-- Module      : Amazonka.KafkaConnect.DeleteConnector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified connector.
module Amazonka.KafkaConnect.DeleteConnector
  ( -- * Creating a Request
    DeleteConnector (..),
    newDeleteConnector,

    -- * Request Lenses
    deleteConnector_currentVersion,
    deleteConnector_connectorArn,

    -- * Destructuring the Response
    DeleteConnectorResponse (..),
    newDeleteConnectorResponse,

    -- * Response Lenses
    deleteConnectorResponse_connectorArn,
    deleteConnectorResponse_connectorState,
    deleteConnectorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KafkaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteConnector' smart constructor.
data DeleteConnector = DeleteConnector'
  { -- | The current version of the connector that you want to delete.
    currentVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the connector that you want to delete.
    connectorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConnector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentVersion', 'deleteConnector_currentVersion' - The current version of the connector that you want to delete.
--
-- 'connectorArn', 'deleteConnector_connectorArn' - The Amazon Resource Name (ARN) of the connector that you want to delete.
newDeleteConnector ::
  -- | 'connectorArn'
  Prelude.Text ->
  DeleteConnector
newDeleteConnector pConnectorArn_ =
  DeleteConnector'
    { currentVersion = Prelude.Nothing,
      connectorArn = pConnectorArn_
    }

-- | The current version of the connector that you want to delete.
deleteConnector_currentVersion :: Lens.Lens' DeleteConnector (Prelude.Maybe Prelude.Text)
deleteConnector_currentVersion = Lens.lens (\DeleteConnector' {currentVersion} -> currentVersion) (\s@DeleteConnector' {} a -> s {currentVersion = a} :: DeleteConnector)

-- | The Amazon Resource Name (ARN) of the connector that you want to delete.
deleteConnector_connectorArn :: Lens.Lens' DeleteConnector Prelude.Text
deleteConnector_connectorArn = Lens.lens (\DeleteConnector' {connectorArn} -> connectorArn) (\s@DeleteConnector' {} a -> s {connectorArn = a} :: DeleteConnector)

instance Core.AWSRequest DeleteConnector where
  type
    AWSResponse DeleteConnector =
      DeleteConnectorResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteConnectorResponse'
            Prelude.<$> (x Data..?> "connectorArn")
            Prelude.<*> (x Data..?> "connectorState")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteConnector where
  hashWithSalt _salt DeleteConnector' {..} =
    _salt
      `Prelude.hashWithSalt` currentVersion
      `Prelude.hashWithSalt` connectorArn

instance Prelude.NFData DeleteConnector where
  rnf DeleteConnector' {..} =
    Prelude.rnf currentVersion
      `Prelude.seq` Prelude.rnf connectorArn

instance Data.ToHeaders DeleteConnector where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteConnector where
  toPath DeleteConnector' {..} =
    Prelude.mconcat
      ["/v1/connectors/", Data.toBS connectorArn]

instance Data.ToQuery DeleteConnector where
  toQuery DeleteConnector' {..} =
    Prelude.mconcat
      ["currentVersion" Data.=: currentVersion]

-- | /See:/ 'newDeleteConnectorResponse' smart constructor.
data DeleteConnectorResponse = DeleteConnectorResponse'
  { -- | The Amazon Resource Name (ARN) of the connector that you requested to
    -- delete.
    connectorArn :: Prelude.Maybe Prelude.Text,
    -- | The state of the connector that you requested to delete.
    connectorState :: Prelude.Maybe ConnectorState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConnectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectorArn', 'deleteConnectorResponse_connectorArn' - The Amazon Resource Name (ARN) of the connector that you requested to
-- delete.
--
-- 'connectorState', 'deleteConnectorResponse_connectorState' - The state of the connector that you requested to delete.
--
-- 'httpStatus', 'deleteConnectorResponse_httpStatus' - The response's http status code.
newDeleteConnectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteConnectorResponse
newDeleteConnectorResponse pHttpStatus_ =
  DeleteConnectorResponse'
    { connectorArn =
        Prelude.Nothing,
      connectorState = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the connector that you requested to
-- delete.
deleteConnectorResponse_connectorArn :: Lens.Lens' DeleteConnectorResponse (Prelude.Maybe Prelude.Text)
deleteConnectorResponse_connectorArn = Lens.lens (\DeleteConnectorResponse' {connectorArn} -> connectorArn) (\s@DeleteConnectorResponse' {} a -> s {connectorArn = a} :: DeleteConnectorResponse)

-- | The state of the connector that you requested to delete.
deleteConnectorResponse_connectorState :: Lens.Lens' DeleteConnectorResponse (Prelude.Maybe ConnectorState)
deleteConnectorResponse_connectorState = Lens.lens (\DeleteConnectorResponse' {connectorState} -> connectorState) (\s@DeleteConnectorResponse' {} a -> s {connectorState = a} :: DeleteConnectorResponse)

-- | The response's http status code.
deleteConnectorResponse_httpStatus :: Lens.Lens' DeleteConnectorResponse Prelude.Int
deleteConnectorResponse_httpStatus = Lens.lens (\DeleteConnectorResponse' {httpStatus} -> httpStatus) (\s@DeleteConnectorResponse' {} a -> s {httpStatus = a} :: DeleteConnectorResponse)

instance Prelude.NFData DeleteConnectorResponse where
  rnf DeleteConnectorResponse' {..} =
    Prelude.rnf connectorArn
      `Prelude.seq` Prelude.rnf connectorState
      `Prelude.seq` Prelude.rnf httpStatus
