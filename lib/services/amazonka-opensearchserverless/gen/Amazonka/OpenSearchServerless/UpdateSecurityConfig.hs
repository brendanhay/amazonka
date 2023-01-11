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
-- Module      : Amazonka.OpenSearchServerless.UpdateSecurityConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a security configuration for OpenSearch Serverless. For more
-- information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/serverless-saml.html SAML authentication for Amazon OpenSearch Serverless>.
module Amazonka.OpenSearchServerless.UpdateSecurityConfig
  ( -- * Creating a Request
    UpdateSecurityConfig (..),
    newUpdateSecurityConfig,

    -- * Request Lenses
    updateSecurityConfig_clientToken,
    updateSecurityConfig_description,
    updateSecurityConfig_samlOptions,
    updateSecurityConfig_configVersion,
    updateSecurityConfig_id,

    -- * Destructuring the Response
    UpdateSecurityConfigResponse (..),
    newUpdateSecurityConfigResponse,

    -- * Response Lenses
    updateSecurityConfigResponse_securityConfigDetail,
    updateSecurityConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSecurityConfig' smart constructor.
data UpdateSecurityConfig = UpdateSecurityConfig'
  { -- | Unique, case-sensitive identifier to ensure idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description of the security configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | SAML options in in the form of a key-value map.
    samlOptions :: Prelude.Maybe SamlConfigOptions,
    -- | The version of the security configuration to be updated. You can find
    -- the most recent version of a security configuration using the
    -- @GetSecurityPolicy@ command.
    configVersion :: Prelude.Text,
    -- | The security configuration identifier. For SAML the ID will be
    -- @saml\/\<accountId>\/\<idpProviderName>@. For example,
    -- @saml\/123456789123\/OKTADev@.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSecurityConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateSecurityConfig_clientToken' - Unique, case-sensitive identifier to ensure idempotency of the request.
--
-- 'description', 'updateSecurityConfig_description' - A description of the security configuration.
--
-- 'samlOptions', 'updateSecurityConfig_samlOptions' - SAML options in in the form of a key-value map.
--
-- 'configVersion', 'updateSecurityConfig_configVersion' - The version of the security configuration to be updated. You can find
-- the most recent version of a security configuration using the
-- @GetSecurityPolicy@ command.
--
-- 'id', 'updateSecurityConfig_id' - The security configuration identifier. For SAML the ID will be
-- @saml\/\<accountId>\/\<idpProviderName>@. For example,
-- @saml\/123456789123\/OKTADev@.
newUpdateSecurityConfig ::
  -- | 'configVersion'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  UpdateSecurityConfig
newUpdateSecurityConfig pConfigVersion_ pId_ =
  UpdateSecurityConfig'
    { clientToken =
        Prelude.Nothing,
      description = Prelude.Nothing,
      samlOptions = Prelude.Nothing,
      configVersion = pConfigVersion_,
      id = pId_
    }

-- | Unique, case-sensitive identifier to ensure idempotency of the request.
updateSecurityConfig_clientToken :: Lens.Lens' UpdateSecurityConfig (Prelude.Maybe Prelude.Text)
updateSecurityConfig_clientToken = Lens.lens (\UpdateSecurityConfig' {clientToken} -> clientToken) (\s@UpdateSecurityConfig' {} a -> s {clientToken = a} :: UpdateSecurityConfig)

-- | A description of the security configuration.
updateSecurityConfig_description :: Lens.Lens' UpdateSecurityConfig (Prelude.Maybe Prelude.Text)
updateSecurityConfig_description = Lens.lens (\UpdateSecurityConfig' {description} -> description) (\s@UpdateSecurityConfig' {} a -> s {description = a} :: UpdateSecurityConfig)

-- | SAML options in in the form of a key-value map.
updateSecurityConfig_samlOptions :: Lens.Lens' UpdateSecurityConfig (Prelude.Maybe SamlConfigOptions)
updateSecurityConfig_samlOptions = Lens.lens (\UpdateSecurityConfig' {samlOptions} -> samlOptions) (\s@UpdateSecurityConfig' {} a -> s {samlOptions = a} :: UpdateSecurityConfig)

-- | The version of the security configuration to be updated. You can find
-- the most recent version of a security configuration using the
-- @GetSecurityPolicy@ command.
updateSecurityConfig_configVersion :: Lens.Lens' UpdateSecurityConfig Prelude.Text
updateSecurityConfig_configVersion = Lens.lens (\UpdateSecurityConfig' {configVersion} -> configVersion) (\s@UpdateSecurityConfig' {} a -> s {configVersion = a} :: UpdateSecurityConfig)

-- | The security configuration identifier. For SAML the ID will be
-- @saml\/\<accountId>\/\<idpProviderName>@. For example,
-- @saml\/123456789123\/OKTADev@.
updateSecurityConfig_id :: Lens.Lens' UpdateSecurityConfig Prelude.Text
updateSecurityConfig_id = Lens.lens (\UpdateSecurityConfig' {id} -> id) (\s@UpdateSecurityConfig' {} a -> s {id = a} :: UpdateSecurityConfig)

instance Core.AWSRequest UpdateSecurityConfig where
  type
    AWSResponse UpdateSecurityConfig =
      UpdateSecurityConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSecurityConfigResponse'
            Prelude.<$> (x Data..?> "securityConfigDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSecurityConfig where
  hashWithSalt _salt UpdateSecurityConfig' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` samlOptions
      `Prelude.hashWithSalt` configVersion
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateSecurityConfig where
  rnf UpdateSecurityConfig' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf samlOptions
      `Prelude.seq` Prelude.rnf configVersion
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders UpdateSecurityConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpenSearchServerless.UpdateSecurityConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateSecurityConfig where
  toJSON UpdateSecurityConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("description" Data..=) Prelude.<$> description,
            ("samlOptions" Data..=) Prelude.<$> samlOptions,
            Prelude.Just ("configVersion" Data..= configVersion),
            Prelude.Just ("id" Data..= id)
          ]
      )

instance Data.ToPath UpdateSecurityConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateSecurityConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSecurityConfigResponse' smart constructor.
data UpdateSecurityConfigResponse = UpdateSecurityConfigResponse'
  { -- | Details about the updated security configuration.
    securityConfigDetail :: Prelude.Maybe SecurityConfigDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSecurityConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityConfigDetail', 'updateSecurityConfigResponse_securityConfigDetail' - Details about the updated security configuration.
--
-- 'httpStatus', 'updateSecurityConfigResponse_httpStatus' - The response's http status code.
newUpdateSecurityConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSecurityConfigResponse
newUpdateSecurityConfigResponse pHttpStatus_ =
  UpdateSecurityConfigResponse'
    { securityConfigDetail =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the updated security configuration.
updateSecurityConfigResponse_securityConfigDetail :: Lens.Lens' UpdateSecurityConfigResponse (Prelude.Maybe SecurityConfigDetail)
updateSecurityConfigResponse_securityConfigDetail = Lens.lens (\UpdateSecurityConfigResponse' {securityConfigDetail} -> securityConfigDetail) (\s@UpdateSecurityConfigResponse' {} a -> s {securityConfigDetail = a} :: UpdateSecurityConfigResponse)

-- | The response's http status code.
updateSecurityConfigResponse_httpStatus :: Lens.Lens' UpdateSecurityConfigResponse Prelude.Int
updateSecurityConfigResponse_httpStatus = Lens.lens (\UpdateSecurityConfigResponse' {httpStatus} -> httpStatus) (\s@UpdateSecurityConfigResponse' {} a -> s {httpStatus = a} :: UpdateSecurityConfigResponse)

instance Prelude.NFData UpdateSecurityConfigResponse where
  rnf UpdateSecurityConfigResponse' {..} =
    Prelude.rnf securityConfigDetail
      `Prelude.seq` Prelude.rnf httpStatus
