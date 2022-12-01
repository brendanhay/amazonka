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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    updateConnectorRegistration_description,
    updateConnectorRegistration_connectorProvisioningConfig,
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
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateConnectorRegistration' smart constructor.
data UpdateConnectorRegistration = UpdateConnectorRegistration'
  { -- | A description about the update that you\'re applying to the connector.
    description :: Prelude.Maybe Prelude.Text,
    connectorProvisioningConfig :: Prelude.Maybe ConnectorProvisioningConfig,
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
-- 'description', 'updateConnectorRegistration_description' - A description about the update that you\'re applying to the connector.
--
-- 'connectorProvisioningConfig', 'updateConnectorRegistration_connectorProvisioningConfig' - Undocumented member.
--
-- 'connectorLabel', 'updateConnectorRegistration_connectorLabel' - The name of the connector. The name is unique for each connector
-- registration in your AWS account.
newUpdateConnectorRegistration ::
  -- | 'connectorLabel'
  Prelude.Text ->
  UpdateConnectorRegistration
newUpdateConnectorRegistration pConnectorLabel_ =
  UpdateConnectorRegistration'
    { description =
        Prelude.Nothing,
      connectorProvisioningConfig = Prelude.Nothing,
      connectorLabel = pConnectorLabel_
    }

-- | A description about the update that you\'re applying to the connector.
updateConnectorRegistration_description :: Lens.Lens' UpdateConnectorRegistration (Prelude.Maybe Prelude.Text)
updateConnectorRegistration_description = Lens.lens (\UpdateConnectorRegistration' {description} -> description) (\s@UpdateConnectorRegistration' {} a -> s {description = a} :: UpdateConnectorRegistration)

-- | Undocumented member.
updateConnectorRegistration_connectorProvisioningConfig :: Lens.Lens' UpdateConnectorRegistration (Prelude.Maybe ConnectorProvisioningConfig)
updateConnectorRegistration_connectorProvisioningConfig = Lens.lens (\UpdateConnectorRegistration' {connectorProvisioningConfig} -> connectorProvisioningConfig) (\s@UpdateConnectorRegistration' {} a -> s {connectorProvisioningConfig = a} :: UpdateConnectorRegistration)

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
            Prelude.<$> (x Core..?> "connectorArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateConnectorRegistration where
  hashWithSalt _salt UpdateConnectorRegistration' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` connectorProvisioningConfig
      `Prelude.hashWithSalt` connectorLabel

instance Prelude.NFData UpdateConnectorRegistration where
  rnf UpdateConnectorRegistration' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf connectorProvisioningConfig
      `Prelude.seq` Prelude.rnf connectorLabel

instance Core.ToHeaders UpdateConnectorRegistration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateConnectorRegistration where
  toJSON UpdateConnectorRegistration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("description" Core..=) Prelude.<$> description,
            ("connectorProvisioningConfig" Core..=)
              Prelude.<$> connectorProvisioningConfig,
            Prelude.Just
              ("connectorLabel" Core..= connectorLabel)
          ]
      )

instance Core.ToPath UpdateConnectorRegistration where
  toPath =
    Prelude.const "/update-connector-registration"

instance Core.ToQuery UpdateConnectorRegistration where
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
