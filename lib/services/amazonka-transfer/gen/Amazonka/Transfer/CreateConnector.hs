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
-- Module      : Amazonka.Transfer.CreateConnector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the connector, which captures the parameters for an outbound
-- connection for the AS2 protocol. The connector is required for sending
-- files to an externally hosted AS2 server. For more details about
-- connectors, see
-- <https://docs.aws.amazon.com/transfer/latest/userguide/create-b2b-server.html#configure-as2-connector Create AS2 connectors>.
module Amazonka.Transfer.CreateConnector
  ( -- * Creating a Request
    CreateConnector (..),
    newCreateConnector,

    -- * Request Lenses
    createConnector_loggingRole,
    createConnector_tags,
    createConnector_url,
    createConnector_as2Config,
    createConnector_accessRole,

    -- * Destructuring the Response
    CreateConnectorResponse (..),
    newCreateConnectorResponse,

    -- * Response Lenses
    createConnectorResponse_httpStatus,
    createConnectorResponse_connectorId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newCreateConnector' smart constructor.
data CreateConnector = CreateConnector'
  { -- | The Amazon Resource Name (ARN) of the Identity and Access Management
    -- (IAM) role that allows a connector to turn on CloudWatch logging for
    -- Amazon S3 events. When set, you can view connector activity in your
    -- CloudWatch logs.
    loggingRole :: Prelude.Maybe Prelude.Text,
    -- | Key-value pairs that can be used to group and search for connectors.
    -- Tags are metadata attached to connectors for any purpose.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The URL of the partner\'s AS2 endpoint.
    url :: Prelude.Text,
    -- | A structure that contains the parameters for a connector object.
    as2Config :: As2ConnectorConfig,
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
    accessRole :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConnector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loggingRole', 'createConnector_loggingRole' - The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that allows a connector to turn on CloudWatch logging for
-- Amazon S3 events. When set, you can view connector activity in your
-- CloudWatch logs.
--
-- 'tags', 'createConnector_tags' - Key-value pairs that can be used to group and search for connectors.
-- Tags are metadata attached to connectors for any purpose.
--
-- 'url', 'createConnector_url' - The URL of the partner\'s AS2 endpoint.
--
-- 'as2Config', 'createConnector_as2Config' - A structure that contains the parameters for a connector object.
--
-- 'accessRole', 'createConnector_accessRole' - With AS2, you can send files by calling @StartFileTransfer@ and
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
newCreateConnector ::
  -- | 'url'
  Prelude.Text ->
  -- | 'as2Config'
  As2ConnectorConfig ->
  -- | 'accessRole'
  Prelude.Text ->
  CreateConnector
newCreateConnector pUrl_ pAs2Config_ pAccessRole_ =
  CreateConnector'
    { loggingRole = Prelude.Nothing,
      tags = Prelude.Nothing,
      url = pUrl_,
      as2Config = pAs2Config_,
      accessRole = pAccessRole_
    }

-- | The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that allows a connector to turn on CloudWatch logging for
-- Amazon S3 events. When set, you can view connector activity in your
-- CloudWatch logs.
createConnector_loggingRole :: Lens.Lens' CreateConnector (Prelude.Maybe Prelude.Text)
createConnector_loggingRole = Lens.lens (\CreateConnector' {loggingRole} -> loggingRole) (\s@CreateConnector' {} a -> s {loggingRole = a} :: CreateConnector)

-- | Key-value pairs that can be used to group and search for connectors.
-- Tags are metadata attached to connectors for any purpose.
createConnector_tags :: Lens.Lens' CreateConnector (Prelude.Maybe (Prelude.NonEmpty Tag))
createConnector_tags = Lens.lens (\CreateConnector' {tags} -> tags) (\s@CreateConnector' {} a -> s {tags = a} :: CreateConnector) Prelude.. Lens.mapping Lens.coerced

-- | The URL of the partner\'s AS2 endpoint.
createConnector_url :: Lens.Lens' CreateConnector Prelude.Text
createConnector_url = Lens.lens (\CreateConnector' {url} -> url) (\s@CreateConnector' {} a -> s {url = a} :: CreateConnector)

-- | A structure that contains the parameters for a connector object.
createConnector_as2Config :: Lens.Lens' CreateConnector As2ConnectorConfig
createConnector_as2Config = Lens.lens (\CreateConnector' {as2Config} -> as2Config) (\s@CreateConnector' {} a -> s {as2Config = a} :: CreateConnector)

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
createConnector_accessRole :: Lens.Lens' CreateConnector Prelude.Text
createConnector_accessRole = Lens.lens (\CreateConnector' {accessRole} -> accessRole) (\s@CreateConnector' {} a -> s {accessRole = a} :: CreateConnector)

instance Core.AWSRequest CreateConnector where
  type
    AWSResponse CreateConnector =
      CreateConnectorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateConnectorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ConnectorId")
      )

instance Prelude.Hashable CreateConnector where
  hashWithSalt _salt CreateConnector' {..} =
    _salt `Prelude.hashWithSalt` loggingRole
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` as2Config
      `Prelude.hashWithSalt` accessRole

instance Prelude.NFData CreateConnector where
  rnf CreateConnector' {..} =
    Prelude.rnf loggingRole
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf url
      `Prelude.seq` Prelude.rnf as2Config
      `Prelude.seq` Prelude.rnf accessRole

instance Data.ToHeaders CreateConnector where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TransferService.CreateConnector" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateConnector where
  toJSON CreateConnector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LoggingRole" Data..=) Prelude.<$> loggingRole,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Url" Data..= url),
            Prelude.Just ("As2Config" Data..= as2Config),
            Prelude.Just ("AccessRole" Data..= accessRole)
          ]
      )

instance Data.ToPath CreateConnector where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateConnector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateConnectorResponse' smart constructor.
data CreateConnectorResponse = CreateConnectorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique identifier for the connector, returned after the API call
    -- succeeds.
    connectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConnectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createConnectorResponse_httpStatus' - The response's http status code.
--
-- 'connectorId', 'createConnectorResponse_connectorId' - The unique identifier for the connector, returned after the API call
-- succeeds.
newCreateConnectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'connectorId'
  Prelude.Text ->
  CreateConnectorResponse
newCreateConnectorResponse pHttpStatus_ pConnectorId_ =
  CreateConnectorResponse'
    { httpStatus = pHttpStatus_,
      connectorId = pConnectorId_
    }

-- | The response's http status code.
createConnectorResponse_httpStatus :: Lens.Lens' CreateConnectorResponse Prelude.Int
createConnectorResponse_httpStatus = Lens.lens (\CreateConnectorResponse' {httpStatus} -> httpStatus) (\s@CreateConnectorResponse' {} a -> s {httpStatus = a} :: CreateConnectorResponse)

-- | The unique identifier for the connector, returned after the API call
-- succeeds.
createConnectorResponse_connectorId :: Lens.Lens' CreateConnectorResponse Prelude.Text
createConnectorResponse_connectorId = Lens.lens (\CreateConnectorResponse' {connectorId} -> connectorId) (\s@CreateConnectorResponse' {} a -> s {connectorId = a} :: CreateConnectorResponse)

instance Prelude.NFData CreateConnectorResponse where
  rnf CreateConnectorResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf connectorId
