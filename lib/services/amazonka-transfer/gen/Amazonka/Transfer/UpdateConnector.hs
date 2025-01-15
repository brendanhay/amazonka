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
-- Module      : Amazonka.Transfer.UpdateConnector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates some of the parameters for an existing connector. Provide the
-- @ConnectorId@ for the connector that you want to update, along with the
-- new values for the parameters to update.
module Amazonka.Transfer.UpdateConnector
  ( -- * Creating a Request
    UpdateConnector (..),
    newUpdateConnector,

    -- * Request Lenses
    updateConnector_accessRole,
    updateConnector_as2Config,
    updateConnector_loggingRole,
    updateConnector_url,
    updateConnector_connectorId,

    -- * Destructuring the Response
    UpdateConnectorResponse (..),
    newUpdateConnectorResponse,

    -- * Response Lenses
    updateConnectorResponse_httpStatus,
    updateConnectorResponse_connectorId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newUpdateConnector' smart constructor.
data UpdateConnector = UpdateConnector'
  { -- | With AS2, you can send files by calling @StartFileTransfer@ and
    -- specifying the file paths in the request parameter, @SendFilePaths@. We
    -- use the file’s parent directory (for example, for
    -- @--send-file-paths \/bucket\/dir\/file.txt@, parent directory is
    -- @\/bucket\/dir\/@) to temporarily store a processed AS2 message file,
    -- store the MDN when we receive them from the partner, and write a final
    -- JSON file containing relevant metadata of the transmission. So, the
    -- @AccessRole@ needs to provide read and write access to the parent
    -- directory of the file location used in the @StartFileTransfer@ request.
    -- Additionally, you need to provide read and write access to the parent
    -- directory of the files that you intend to send with @StartFileTransfer@.
    accessRole :: Prelude.Maybe Prelude.Text,
    -- | A structure that contains the parameters for a connector object.
    as2Config :: Prelude.Maybe As2ConnectorConfig,
    -- | The Amazon Resource Name (ARN) of the Identity and Access Management
    -- (IAM) role that allows a connector to turn on CloudWatch logging for
    -- Amazon S3 events. When set, you can view connector activity in your
    -- CloudWatch logs.
    loggingRole :: Prelude.Maybe Prelude.Text,
    -- | The URL of the partner\'s AS2 endpoint.
    url :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the connector.
    connectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConnector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessRole', 'updateConnector_accessRole' - With AS2, you can send files by calling @StartFileTransfer@ and
-- specifying the file paths in the request parameter, @SendFilePaths@. We
-- use the file’s parent directory (for example, for
-- @--send-file-paths \/bucket\/dir\/file.txt@, parent directory is
-- @\/bucket\/dir\/@) to temporarily store a processed AS2 message file,
-- store the MDN when we receive them from the partner, and write a final
-- JSON file containing relevant metadata of the transmission. So, the
-- @AccessRole@ needs to provide read and write access to the parent
-- directory of the file location used in the @StartFileTransfer@ request.
-- Additionally, you need to provide read and write access to the parent
-- directory of the files that you intend to send with @StartFileTransfer@.
--
-- 'as2Config', 'updateConnector_as2Config' - A structure that contains the parameters for a connector object.
--
-- 'loggingRole', 'updateConnector_loggingRole' - The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that allows a connector to turn on CloudWatch logging for
-- Amazon S3 events. When set, you can view connector activity in your
-- CloudWatch logs.
--
-- 'url', 'updateConnector_url' - The URL of the partner\'s AS2 endpoint.
--
-- 'connectorId', 'updateConnector_connectorId' - The unique identifier for the connector.
newUpdateConnector ::
  -- | 'connectorId'
  Prelude.Text ->
  UpdateConnector
newUpdateConnector pConnectorId_ =
  UpdateConnector'
    { accessRole = Prelude.Nothing,
      as2Config = Prelude.Nothing,
      loggingRole = Prelude.Nothing,
      url = Prelude.Nothing,
      connectorId = pConnectorId_
    }

-- | With AS2, you can send files by calling @StartFileTransfer@ and
-- specifying the file paths in the request parameter, @SendFilePaths@. We
-- use the file’s parent directory (for example, for
-- @--send-file-paths \/bucket\/dir\/file.txt@, parent directory is
-- @\/bucket\/dir\/@) to temporarily store a processed AS2 message file,
-- store the MDN when we receive them from the partner, and write a final
-- JSON file containing relevant metadata of the transmission. So, the
-- @AccessRole@ needs to provide read and write access to the parent
-- directory of the file location used in the @StartFileTransfer@ request.
-- Additionally, you need to provide read and write access to the parent
-- directory of the files that you intend to send with @StartFileTransfer@.
updateConnector_accessRole :: Lens.Lens' UpdateConnector (Prelude.Maybe Prelude.Text)
updateConnector_accessRole = Lens.lens (\UpdateConnector' {accessRole} -> accessRole) (\s@UpdateConnector' {} a -> s {accessRole = a} :: UpdateConnector)

-- | A structure that contains the parameters for a connector object.
updateConnector_as2Config :: Lens.Lens' UpdateConnector (Prelude.Maybe As2ConnectorConfig)
updateConnector_as2Config = Lens.lens (\UpdateConnector' {as2Config} -> as2Config) (\s@UpdateConnector' {} a -> s {as2Config = a} :: UpdateConnector)

-- | The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that allows a connector to turn on CloudWatch logging for
-- Amazon S3 events. When set, you can view connector activity in your
-- CloudWatch logs.
updateConnector_loggingRole :: Lens.Lens' UpdateConnector (Prelude.Maybe Prelude.Text)
updateConnector_loggingRole = Lens.lens (\UpdateConnector' {loggingRole} -> loggingRole) (\s@UpdateConnector' {} a -> s {loggingRole = a} :: UpdateConnector)

-- | The URL of the partner\'s AS2 endpoint.
updateConnector_url :: Lens.Lens' UpdateConnector (Prelude.Maybe Prelude.Text)
updateConnector_url = Lens.lens (\UpdateConnector' {url} -> url) (\s@UpdateConnector' {} a -> s {url = a} :: UpdateConnector)

-- | The unique identifier for the connector.
updateConnector_connectorId :: Lens.Lens' UpdateConnector Prelude.Text
updateConnector_connectorId = Lens.lens (\UpdateConnector' {connectorId} -> connectorId) (\s@UpdateConnector' {} a -> s {connectorId = a} :: UpdateConnector)

instance Core.AWSRequest UpdateConnector where
  type
    AWSResponse UpdateConnector =
      UpdateConnectorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateConnectorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ConnectorId")
      )

instance Prelude.Hashable UpdateConnector where
  hashWithSalt _salt UpdateConnector' {..} =
    _salt
      `Prelude.hashWithSalt` accessRole
      `Prelude.hashWithSalt` as2Config
      `Prelude.hashWithSalt` loggingRole
      `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` connectorId

instance Prelude.NFData UpdateConnector where
  rnf UpdateConnector' {..} =
    Prelude.rnf accessRole `Prelude.seq`
      Prelude.rnf as2Config `Prelude.seq`
        Prelude.rnf loggingRole `Prelude.seq`
          Prelude.rnf url `Prelude.seq`
            Prelude.rnf connectorId

instance Data.ToHeaders UpdateConnector where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TransferService.UpdateConnector" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateConnector where
  toJSON UpdateConnector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccessRole" Data..=) Prelude.<$> accessRole,
            ("As2Config" Data..=) Prelude.<$> as2Config,
            ("LoggingRole" Data..=) Prelude.<$> loggingRole,
            ("Url" Data..=) Prelude.<$> url,
            Prelude.Just ("ConnectorId" Data..= connectorId)
          ]
      )

instance Data.ToPath UpdateConnector where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateConnector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateConnectorResponse' smart constructor.
data UpdateConnectorResponse = UpdateConnectorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Returns the identifier of the connector object that you are updating.
    connectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConnectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateConnectorResponse_httpStatus' - The response's http status code.
--
-- 'connectorId', 'updateConnectorResponse_connectorId' - Returns the identifier of the connector object that you are updating.
newUpdateConnectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'connectorId'
  Prelude.Text ->
  UpdateConnectorResponse
newUpdateConnectorResponse pHttpStatus_ pConnectorId_ =
  UpdateConnectorResponse'
    { httpStatus = pHttpStatus_,
      connectorId = pConnectorId_
    }

-- | The response's http status code.
updateConnectorResponse_httpStatus :: Lens.Lens' UpdateConnectorResponse Prelude.Int
updateConnectorResponse_httpStatus = Lens.lens (\UpdateConnectorResponse' {httpStatus} -> httpStatus) (\s@UpdateConnectorResponse' {} a -> s {httpStatus = a} :: UpdateConnectorResponse)

-- | Returns the identifier of the connector object that you are updating.
updateConnectorResponse_connectorId :: Lens.Lens' UpdateConnectorResponse Prelude.Text
updateConnectorResponse_connectorId = Lens.lens (\UpdateConnectorResponse' {connectorId} -> connectorId) (\s@UpdateConnectorResponse' {} a -> s {connectorId = a} :: UpdateConnectorResponse)

instance Prelude.NFData UpdateConnectorResponse where
  rnf UpdateConnectorResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf connectorId
