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
-- Module      : Amazonka.AppFlow.UpdateConnectorRegistration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a custom connector that you\'ve previously registered. This
-- operation updates the connector with one of the following:
--
-- -   The latest version of the AWS Lambda function that\'s assigned to
--     the connector
--
-- -   A new AWS Lambda function that you specify
module Amazonka.AppFlow.UpdateConnectorRegistration
  ( -- * Creating a Request
    UpdateConnectorRegistration (..),
    newUpdateConnectorRegistration,

    -- * Request Lenses
    updateConnectorRegistration_clientToken,
    updateConnectorRegistration_connectorProvisioningConfig,
    updateConnectorRegistration_description,
    updateConnectorRegistration_connectorLabel,

    -- * Destructuring the Response
    UpdateConnectorRegistrationResponse (..),
    newUpdateConnectorRegistrationResponse,

    -- * Response Lenses
    updateConnectorRegistrationResponse_connectorArn,
    updateConnectorRegistrationResponse_httpStatus,
  )
where

import Amazonka.AppFlow.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateConnectorRegistration' smart constructor.
data UpdateConnectorRegistration = UpdateConnectorRegistration'
  { -- | The @clientToken@ parameter is an idempotency token. It ensures that
    -- your @UpdateConnectorRegistration@ request completes only once. You
    -- choose the value to pass. For example, if you don\'t receive a response
    -- from your request, you can safely retry the request with the same
    -- @clientToken@ parameter value.
    --
    -- If you omit a @clientToken@ value, the Amazon Web Services SDK that you
    -- are using inserts a value for you. This way, the SDK can safely retry
    -- requests multiple times after a network error. You must provide your own
    -- value for other use cases.
    --
    -- If you specify input parameters that differ from your first request, an
    -- error occurs. If you use a different value for @clientToken@, Amazon
    -- AppFlow considers it a new call to @UpdateConnectorRegistration@. The
    -- token is active for 8 hours.
    clientToken :: Prelude.Maybe Prelude.Text,
    connectorProvisioningConfig :: Prelude.Maybe ConnectorProvisioningConfig,
    -- | A description about the update that you\'re applying to the connector.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the connector. The name is unique for each connector
    -- registration in your AWS account.
    connectorLabel :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConnectorRegistration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateConnectorRegistration_clientToken' - The @clientToken@ parameter is an idempotency token. It ensures that
-- your @UpdateConnectorRegistration@ request completes only once. You
-- choose the value to pass. For example, if you don\'t receive a response
-- from your request, you can safely retry the request with the same
-- @clientToken@ parameter value.
--
-- If you omit a @clientToken@ value, the Amazon Web Services SDK that you
-- are using inserts a value for you. This way, the SDK can safely retry
-- requests multiple times after a network error. You must provide your own
-- value for other use cases.
--
-- If you specify input parameters that differ from your first request, an
-- error occurs. If you use a different value for @clientToken@, Amazon
-- AppFlow considers it a new call to @UpdateConnectorRegistration@. The
-- token is active for 8 hours.
--
-- 'connectorProvisioningConfig', 'updateConnectorRegistration_connectorProvisioningConfig' - Undocumented member.
--
-- 'description', 'updateConnectorRegistration_description' - A description about the update that you\'re applying to the connector.
--
-- 'connectorLabel', 'updateConnectorRegistration_connectorLabel' - The name of the connector. The name is unique for each connector
-- registration in your AWS account.
newUpdateConnectorRegistration ::
  -- | 'connectorLabel'
  Prelude.Text ->
  UpdateConnectorRegistration
newUpdateConnectorRegistration pConnectorLabel_ =
  UpdateConnectorRegistration'
    { clientToken =
        Prelude.Nothing,
      connectorProvisioningConfig = Prelude.Nothing,
      description = Prelude.Nothing,
      connectorLabel = pConnectorLabel_
    }

-- | The @clientToken@ parameter is an idempotency token. It ensures that
-- your @UpdateConnectorRegistration@ request completes only once. You
-- choose the value to pass. For example, if you don\'t receive a response
-- from your request, you can safely retry the request with the same
-- @clientToken@ parameter value.
--
-- If you omit a @clientToken@ value, the Amazon Web Services SDK that you
-- are using inserts a value for you. This way, the SDK can safely retry
-- requests multiple times after a network error. You must provide your own
-- value for other use cases.
--
-- If you specify input parameters that differ from your first request, an
-- error occurs. If you use a different value for @clientToken@, Amazon
-- AppFlow considers it a new call to @UpdateConnectorRegistration@. The
-- token is active for 8 hours.
updateConnectorRegistration_clientToken :: Lens.Lens' UpdateConnectorRegistration (Prelude.Maybe Prelude.Text)
updateConnectorRegistration_clientToken = Lens.lens (\UpdateConnectorRegistration' {clientToken} -> clientToken) (\s@UpdateConnectorRegistration' {} a -> s {clientToken = a} :: UpdateConnectorRegistration)

-- | Undocumented member.
updateConnectorRegistration_connectorProvisioningConfig :: Lens.Lens' UpdateConnectorRegistration (Prelude.Maybe ConnectorProvisioningConfig)
updateConnectorRegistration_connectorProvisioningConfig = Lens.lens (\UpdateConnectorRegistration' {connectorProvisioningConfig} -> connectorProvisioningConfig) (\s@UpdateConnectorRegistration' {} a -> s {connectorProvisioningConfig = a} :: UpdateConnectorRegistration)

-- | A description about the update that you\'re applying to the connector.
updateConnectorRegistration_description :: Lens.Lens' UpdateConnectorRegistration (Prelude.Maybe Prelude.Text)
updateConnectorRegistration_description = Lens.lens (\UpdateConnectorRegistration' {description} -> description) (\s@UpdateConnectorRegistration' {} a -> s {description = a} :: UpdateConnectorRegistration)

-- | The name of the connector. The name is unique for each connector
-- registration in your AWS account.
updateConnectorRegistration_connectorLabel :: Lens.Lens' UpdateConnectorRegistration Prelude.Text
updateConnectorRegistration_connectorLabel = Lens.lens (\UpdateConnectorRegistration' {connectorLabel} -> connectorLabel) (\s@UpdateConnectorRegistration' {} a -> s {connectorLabel = a} :: UpdateConnectorRegistration)

instance Core.AWSRequest UpdateConnectorRegistration where
  type
    AWSResponse UpdateConnectorRegistration =
      UpdateConnectorRegistrationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateConnectorRegistrationResponse'
            Prelude.<$> (x Data..?> "connectorArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateConnectorRegistration where
  hashWithSalt _salt UpdateConnectorRegistration' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` connectorProvisioningConfig
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` connectorLabel

instance Prelude.NFData UpdateConnectorRegistration where
  rnf UpdateConnectorRegistration' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf connectorProvisioningConfig
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf connectorLabel

instance Data.ToHeaders UpdateConnectorRegistration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateConnectorRegistration where
  toJSON UpdateConnectorRegistration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("connectorProvisioningConfig" Data..=)
              Prelude.<$> connectorProvisioningConfig,
            ("description" Data..=) Prelude.<$> description,
            Prelude.Just
              ("connectorLabel" Data..= connectorLabel)
          ]
      )

instance Data.ToPath UpdateConnectorRegistration where
  toPath =
    Prelude.const "/update-connector-registration"

instance Data.ToQuery UpdateConnectorRegistration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateConnectorRegistrationResponse' smart constructor.
data UpdateConnectorRegistrationResponse = UpdateConnectorRegistrationResponse'
  { -- | The ARN of the connector being updated.
    connectorArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConnectorRegistrationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectorArn', 'updateConnectorRegistrationResponse_connectorArn' - The ARN of the connector being updated.
--
-- 'httpStatus', 'updateConnectorRegistrationResponse_httpStatus' - The response's http status code.
newUpdateConnectorRegistrationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateConnectorRegistrationResponse
newUpdateConnectorRegistrationResponse pHttpStatus_ =
  UpdateConnectorRegistrationResponse'
    { connectorArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the connector being updated.
updateConnectorRegistrationResponse_connectorArn :: Lens.Lens' UpdateConnectorRegistrationResponse (Prelude.Maybe Prelude.Text)
updateConnectorRegistrationResponse_connectorArn = Lens.lens (\UpdateConnectorRegistrationResponse' {connectorArn} -> connectorArn) (\s@UpdateConnectorRegistrationResponse' {} a -> s {connectorArn = a} :: UpdateConnectorRegistrationResponse)

-- | The response's http status code.
updateConnectorRegistrationResponse_httpStatus :: Lens.Lens' UpdateConnectorRegistrationResponse Prelude.Int
updateConnectorRegistrationResponse_httpStatus = Lens.lens (\UpdateConnectorRegistrationResponse' {httpStatus} -> httpStatus) (\s@UpdateConnectorRegistrationResponse' {} a -> s {httpStatus = a} :: UpdateConnectorRegistrationResponse)

instance
  Prelude.NFData
    UpdateConnectorRegistrationResponse
  where
  rnf UpdateConnectorRegistrationResponse' {..} =
    Prelude.rnf connectorArn
      `Prelude.seq` Prelude.rnf httpStatus
