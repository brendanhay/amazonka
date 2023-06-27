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
-- Module      : Amazonka.AppFlow.UpdateConnectorProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a given connector profile associated with your account.
module Amazonka.AppFlow.UpdateConnectorProfile
  ( -- * Creating a Request
    UpdateConnectorProfile (..),
    newUpdateConnectorProfile,

    -- * Request Lenses
    updateConnectorProfile_clientToken,
    updateConnectorProfile_connectorProfileName,
    updateConnectorProfile_connectionMode,
    updateConnectorProfile_connectorProfileConfig,

    -- * Destructuring the Response
    UpdateConnectorProfileResponse (..),
    newUpdateConnectorProfileResponse,

    -- * Response Lenses
    updateConnectorProfileResponse_connectorProfileArn,
    updateConnectorProfileResponse_httpStatus,
  )
where

import Amazonka.AppFlow.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateConnectorProfile' smart constructor.
data UpdateConnectorProfile = UpdateConnectorProfile'
  { -- | The @clientToken@ parameter is an idempotency token. It ensures that
    -- your @UpdateConnectorProfile@ request completes only once. You choose
    -- the value to pass. For example, if you don\'t receive a response from
    -- your request, you can safely retry the request with the same
    -- @clientToken@ parameter value.
    --
    -- If you omit a @clientToken@ value, the Amazon Web Services SDK that you
    -- are using inserts a value for you. This way, the SDK can safely retry
    -- requests multiple times after a network error. You must provide your own
    -- value for other use cases.
    --
    -- If you specify input parameters that differ from your first request, an
    -- error occurs. If you use a different value for @clientToken@, Amazon
    -- AppFlow considers it a new call to @UpdateConnectorProfile@. The token
    -- is active for 8 hours.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the connector profile and is unique for each
    -- @ConnectorProfile@ in the Amazon Web Services account.
    connectorProfileName :: Prelude.Text,
    -- | Indicates the connection mode and if it is public or private.
    connectionMode :: ConnectionMode,
    -- | Defines the connector-specific profile configuration and credentials.
    connectorProfileConfig :: ConnectorProfileConfig
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConnectorProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateConnectorProfile_clientToken' - The @clientToken@ parameter is an idempotency token. It ensures that
-- your @UpdateConnectorProfile@ request completes only once. You choose
-- the value to pass. For example, if you don\'t receive a response from
-- your request, you can safely retry the request with the same
-- @clientToken@ parameter value.
--
-- If you omit a @clientToken@ value, the Amazon Web Services SDK that you
-- are using inserts a value for you. This way, the SDK can safely retry
-- requests multiple times after a network error. You must provide your own
-- value for other use cases.
--
-- If you specify input parameters that differ from your first request, an
-- error occurs. If you use a different value for @clientToken@, Amazon
-- AppFlow considers it a new call to @UpdateConnectorProfile@. The token
-- is active for 8 hours.
--
-- 'connectorProfileName', 'updateConnectorProfile_connectorProfileName' - The name of the connector profile and is unique for each
-- @ConnectorProfile@ in the Amazon Web Services account.
--
-- 'connectionMode', 'updateConnectorProfile_connectionMode' - Indicates the connection mode and if it is public or private.
--
-- 'connectorProfileConfig', 'updateConnectorProfile_connectorProfileConfig' - Defines the connector-specific profile configuration and credentials.
newUpdateConnectorProfile ::
  -- | 'connectorProfileName'
  Prelude.Text ->
  -- | 'connectionMode'
  ConnectionMode ->
  -- | 'connectorProfileConfig'
  ConnectorProfileConfig ->
  UpdateConnectorProfile
newUpdateConnectorProfile
  pConnectorProfileName_
  pConnectionMode_
  pConnectorProfileConfig_ =
    UpdateConnectorProfile'
      { clientToken =
          Prelude.Nothing,
        connectorProfileName = pConnectorProfileName_,
        connectionMode = pConnectionMode_,
        connectorProfileConfig = pConnectorProfileConfig_
      }

-- | The @clientToken@ parameter is an idempotency token. It ensures that
-- your @UpdateConnectorProfile@ request completes only once. You choose
-- the value to pass. For example, if you don\'t receive a response from
-- your request, you can safely retry the request with the same
-- @clientToken@ parameter value.
--
-- If you omit a @clientToken@ value, the Amazon Web Services SDK that you
-- are using inserts a value for you. This way, the SDK can safely retry
-- requests multiple times after a network error. You must provide your own
-- value for other use cases.
--
-- If you specify input parameters that differ from your first request, an
-- error occurs. If you use a different value for @clientToken@, Amazon
-- AppFlow considers it a new call to @UpdateConnectorProfile@. The token
-- is active for 8 hours.
updateConnectorProfile_clientToken :: Lens.Lens' UpdateConnectorProfile (Prelude.Maybe Prelude.Text)
updateConnectorProfile_clientToken = Lens.lens (\UpdateConnectorProfile' {clientToken} -> clientToken) (\s@UpdateConnectorProfile' {} a -> s {clientToken = a} :: UpdateConnectorProfile)

-- | The name of the connector profile and is unique for each
-- @ConnectorProfile@ in the Amazon Web Services account.
updateConnectorProfile_connectorProfileName :: Lens.Lens' UpdateConnectorProfile Prelude.Text
updateConnectorProfile_connectorProfileName = Lens.lens (\UpdateConnectorProfile' {connectorProfileName} -> connectorProfileName) (\s@UpdateConnectorProfile' {} a -> s {connectorProfileName = a} :: UpdateConnectorProfile)

-- | Indicates the connection mode and if it is public or private.
updateConnectorProfile_connectionMode :: Lens.Lens' UpdateConnectorProfile ConnectionMode
updateConnectorProfile_connectionMode = Lens.lens (\UpdateConnectorProfile' {connectionMode} -> connectionMode) (\s@UpdateConnectorProfile' {} a -> s {connectionMode = a} :: UpdateConnectorProfile)

-- | Defines the connector-specific profile configuration and credentials.
updateConnectorProfile_connectorProfileConfig :: Lens.Lens' UpdateConnectorProfile ConnectorProfileConfig
updateConnectorProfile_connectorProfileConfig = Lens.lens (\UpdateConnectorProfile' {connectorProfileConfig} -> connectorProfileConfig) (\s@UpdateConnectorProfile' {} a -> s {connectorProfileConfig = a} :: UpdateConnectorProfile)

instance Core.AWSRequest UpdateConnectorProfile where
  type
    AWSResponse UpdateConnectorProfile =
      UpdateConnectorProfileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateConnectorProfileResponse'
            Prelude.<$> (x Data..?> "connectorProfileArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateConnectorProfile where
  hashWithSalt _salt UpdateConnectorProfile' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` connectorProfileName
      `Prelude.hashWithSalt` connectionMode
      `Prelude.hashWithSalt` connectorProfileConfig

instance Prelude.NFData UpdateConnectorProfile where
  rnf UpdateConnectorProfile' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf connectorProfileName
      `Prelude.seq` Prelude.rnf connectionMode
      `Prelude.seq` Prelude.rnf connectorProfileConfig

instance Data.ToHeaders UpdateConnectorProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateConnectorProfile where
  toJSON UpdateConnectorProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just
              ( "connectorProfileName"
                  Data..= connectorProfileName
              ),
            Prelude.Just
              ("connectionMode" Data..= connectionMode),
            Prelude.Just
              ( "connectorProfileConfig"
                  Data..= connectorProfileConfig
              )
          ]
      )

instance Data.ToPath UpdateConnectorProfile where
  toPath = Prelude.const "/update-connector-profile"

instance Data.ToQuery UpdateConnectorProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateConnectorProfileResponse' smart constructor.
data UpdateConnectorProfileResponse = UpdateConnectorProfileResponse'
  { -- | The Amazon Resource Name (ARN) of the connector profile.
    connectorProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConnectorProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectorProfileArn', 'updateConnectorProfileResponse_connectorProfileArn' - The Amazon Resource Name (ARN) of the connector profile.
--
-- 'httpStatus', 'updateConnectorProfileResponse_httpStatus' - The response's http status code.
newUpdateConnectorProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateConnectorProfileResponse
newUpdateConnectorProfileResponse pHttpStatus_ =
  UpdateConnectorProfileResponse'
    { connectorProfileArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the connector profile.
updateConnectorProfileResponse_connectorProfileArn :: Lens.Lens' UpdateConnectorProfileResponse (Prelude.Maybe Prelude.Text)
updateConnectorProfileResponse_connectorProfileArn = Lens.lens (\UpdateConnectorProfileResponse' {connectorProfileArn} -> connectorProfileArn) (\s@UpdateConnectorProfileResponse' {} a -> s {connectorProfileArn = a} :: UpdateConnectorProfileResponse)

-- | The response's http status code.
updateConnectorProfileResponse_httpStatus :: Lens.Lens' UpdateConnectorProfileResponse Prelude.Int
updateConnectorProfileResponse_httpStatus = Lens.lens (\UpdateConnectorProfileResponse' {httpStatus} -> httpStatus) (\s@UpdateConnectorProfileResponse' {} a -> s {httpStatus = a} :: UpdateConnectorProfileResponse)

instance
  Prelude.NFData
    UpdateConnectorProfileResponse
  where
  rnf UpdateConnectorProfileResponse' {..} =
    Prelude.rnf connectorProfileArn
      `Prelude.seq` Prelude.rnf httpStatus
