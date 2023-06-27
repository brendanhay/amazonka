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
-- Module      : Amazonka.OpenSearchServerless.CreateSecurityConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specifies a security configuration for OpenSearch Serverless. For more
-- information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/serverless-saml.html SAML authentication for Amazon OpenSearch Serverless>.
module Amazonka.OpenSearchServerless.CreateSecurityConfig
  ( -- * Creating a Request
    CreateSecurityConfig (..),
    newCreateSecurityConfig,

    -- * Request Lenses
    createSecurityConfig_clientToken,
    createSecurityConfig_description,
    createSecurityConfig_samlOptions,
    createSecurityConfig_name,
    createSecurityConfig_type,

    -- * Destructuring the Response
    CreateSecurityConfigResponse (..),
    newCreateSecurityConfigResponse,

    -- * Response Lenses
    createSecurityConfigResponse_securityConfigDetail,
    createSecurityConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSecurityConfig' smart constructor.
data CreateSecurityConfig = CreateSecurityConfig'
  { -- | Unique, case-sensitive identifier to ensure idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description of the security configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | Describes SAML options in in the form of a key-value map. This field is
    -- required if you specify @saml@ for the @type@ parameter.
    samlOptions :: Prelude.Maybe SamlConfigOptions,
    -- | The name of the security configuration.
    name :: Prelude.Text,
    -- | The type of security configuration.
    type' :: SecurityConfigType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSecurityConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createSecurityConfig_clientToken' - Unique, case-sensitive identifier to ensure idempotency of the request.
--
-- 'description', 'createSecurityConfig_description' - A description of the security configuration.
--
-- 'samlOptions', 'createSecurityConfig_samlOptions' - Describes SAML options in in the form of a key-value map. This field is
-- required if you specify @saml@ for the @type@ parameter.
--
-- 'name', 'createSecurityConfig_name' - The name of the security configuration.
--
-- 'type'', 'createSecurityConfig_type' - The type of security configuration.
newCreateSecurityConfig ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  SecurityConfigType ->
  CreateSecurityConfig
newCreateSecurityConfig pName_ pType_ =
  CreateSecurityConfig'
    { clientToken =
        Prelude.Nothing,
      description = Prelude.Nothing,
      samlOptions = Prelude.Nothing,
      name = pName_,
      type' = pType_
    }

-- | Unique, case-sensitive identifier to ensure idempotency of the request.
createSecurityConfig_clientToken :: Lens.Lens' CreateSecurityConfig (Prelude.Maybe Prelude.Text)
createSecurityConfig_clientToken = Lens.lens (\CreateSecurityConfig' {clientToken} -> clientToken) (\s@CreateSecurityConfig' {} a -> s {clientToken = a} :: CreateSecurityConfig)

-- | A description of the security configuration.
createSecurityConfig_description :: Lens.Lens' CreateSecurityConfig (Prelude.Maybe Prelude.Text)
createSecurityConfig_description = Lens.lens (\CreateSecurityConfig' {description} -> description) (\s@CreateSecurityConfig' {} a -> s {description = a} :: CreateSecurityConfig)

-- | Describes SAML options in in the form of a key-value map. This field is
-- required if you specify @saml@ for the @type@ parameter.
createSecurityConfig_samlOptions :: Lens.Lens' CreateSecurityConfig (Prelude.Maybe SamlConfigOptions)
createSecurityConfig_samlOptions = Lens.lens (\CreateSecurityConfig' {samlOptions} -> samlOptions) (\s@CreateSecurityConfig' {} a -> s {samlOptions = a} :: CreateSecurityConfig)

-- | The name of the security configuration.
createSecurityConfig_name :: Lens.Lens' CreateSecurityConfig Prelude.Text
createSecurityConfig_name = Lens.lens (\CreateSecurityConfig' {name} -> name) (\s@CreateSecurityConfig' {} a -> s {name = a} :: CreateSecurityConfig)

-- | The type of security configuration.
createSecurityConfig_type :: Lens.Lens' CreateSecurityConfig SecurityConfigType
createSecurityConfig_type = Lens.lens (\CreateSecurityConfig' {type'} -> type') (\s@CreateSecurityConfig' {} a -> s {type' = a} :: CreateSecurityConfig)

instance Core.AWSRequest CreateSecurityConfig where
  type
    AWSResponse CreateSecurityConfig =
      CreateSecurityConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSecurityConfigResponse'
            Prelude.<$> (x Data..?> "securityConfigDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSecurityConfig where
  hashWithSalt _salt CreateSecurityConfig' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` samlOptions
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CreateSecurityConfig where
  rnf CreateSecurityConfig' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf samlOptions
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders CreateSecurityConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpenSearchServerless.CreateSecurityConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSecurityConfig where
  toJSON CreateSecurityConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("description" Data..=) Prelude.<$> description,
            ("samlOptions" Data..=) Prelude.<$> samlOptions,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("type" Data..= type')
          ]
      )

instance Data.ToPath CreateSecurityConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateSecurityConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSecurityConfigResponse' smart constructor.
data CreateSecurityConfigResponse = CreateSecurityConfigResponse'
  { -- | Details about the created security configuration.
    securityConfigDetail :: Prelude.Maybe SecurityConfigDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSecurityConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityConfigDetail', 'createSecurityConfigResponse_securityConfigDetail' - Details about the created security configuration.
--
-- 'httpStatus', 'createSecurityConfigResponse_httpStatus' - The response's http status code.
newCreateSecurityConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSecurityConfigResponse
newCreateSecurityConfigResponse pHttpStatus_ =
  CreateSecurityConfigResponse'
    { securityConfigDetail =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the created security configuration.
createSecurityConfigResponse_securityConfigDetail :: Lens.Lens' CreateSecurityConfigResponse (Prelude.Maybe SecurityConfigDetail)
createSecurityConfigResponse_securityConfigDetail = Lens.lens (\CreateSecurityConfigResponse' {securityConfigDetail} -> securityConfigDetail) (\s@CreateSecurityConfigResponse' {} a -> s {securityConfigDetail = a} :: CreateSecurityConfigResponse)

-- | The response's http status code.
createSecurityConfigResponse_httpStatus :: Lens.Lens' CreateSecurityConfigResponse Prelude.Int
createSecurityConfigResponse_httpStatus = Lens.lens (\CreateSecurityConfigResponse' {httpStatus} -> httpStatus) (\s@CreateSecurityConfigResponse' {} a -> s {httpStatus = a} :: CreateSecurityConfigResponse)

instance Prelude.NFData CreateSecurityConfigResponse where
  rnf CreateSecurityConfigResponse' {..} =
    Prelude.rnf securityConfigDetail
      `Prelude.seq` Prelude.rnf httpStatus
