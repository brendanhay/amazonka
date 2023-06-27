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
-- Module      : Amazonka.IoT.UpdateDomainConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates values stored in the domain configuration. Domain configurations
-- for default endpoints can\'t be updated.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions UpdateDomainConfiguration>
-- action.
module Amazonka.IoT.UpdateDomainConfiguration
  ( -- * Creating a Request
    UpdateDomainConfiguration (..),
    newUpdateDomainConfiguration,

    -- * Request Lenses
    updateDomainConfiguration_authorizerConfig,
    updateDomainConfiguration_domainConfigurationStatus,
    updateDomainConfiguration_removeAuthorizerConfig,
    updateDomainConfiguration_tlsConfig,
    updateDomainConfiguration_domainConfigurationName,

    -- * Destructuring the Response
    UpdateDomainConfigurationResponse (..),
    newUpdateDomainConfigurationResponse,

    -- * Response Lenses
    updateDomainConfigurationResponse_domainConfigurationArn,
    updateDomainConfigurationResponse_domainConfigurationName,
    updateDomainConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDomainConfiguration' smart constructor.
data UpdateDomainConfiguration = UpdateDomainConfiguration'
  { -- | An object that specifies the authorization service for a domain.
    authorizerConfig :: Prelude.Maybe AuthorizerConfig,
    -- | The status to which the domain configuration should be updated.
    domainConfigurationStatus :: Prelude.Maybe DomainConfigurationStatus,
    -- | Removes the authorization configuration from a domain.
    removeAuthorizerConfig :: Prelude.Maybe Prelude.Bool,
    -- | An object that specifies the TLS configuration for a domain.
    tlsConfig :: Prelude.Maybe TlsConfig,
    -- | The name of the domain configuration to be updated.
    domainConfigurationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDomainConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizerConfig', 'updateDomainConfiguration_authorizerConfig' - An object that specifies the authorization service for a domain.
--
-- 'domainConfigurationStatus', 'updateDomainConfiguration_domainConfigurationStatus' - The status to which the domain configuration should be updated.
--
-- 'removeAuthorizerConfig', 'updateDomainConfiguration_removeAuthorizerConfig' - Removes the authorization configuration from a domain.
--
-- 'tlsConfig', 'updateDomainConfiguration_tlsConfig' - An object that specifies the TLS configuration for a domain.
--
-- 'domainConfigurationName', 'updateDomainConfiguration_domainConfigurationName' - The name of the domain configuration to be updated.
newUpdateDomainConfiguration ::
  -- | 'domainConfigurationName'
  Prelude.Text ->
  UpdateDomainConfiguration
newUpdateDomainConfiguration
  pDomainConfigurationName_ =
    UpdateDomainConfiguration'
      { authorizerConfig =
          Prelude.Nothing,
        domainConfigurationStatus = Prelude.Nothing,
        removeAuthorizerConfig = Prelude.Nothing,
        tlsConfig = Prelude.Nothing,
        domainConfigurationName =
          pDomainConfigurationName_
      }

-- | An object that specifies the authorization service for a domain.
updateDomainConfiguration_authorizerConfig :: Lens.Lens' UpdateDomainConfiguration (Prelude.Maybe AuthorizerConfig)
updateDomainConfiguration_authorizerConfig = Lens.lens (\UpdateDomainConfiguration' {authorizerConfig} -> authorizerConfig) (\s@UpdateDomainConfiguration' {} a -> s {authorizerConfig = a} :: UpdateDomainConfiguration)

-- | The status to which the domain configuration should be updated.
updateDomainConfiguration_domainConfigurationStatus :: Lens.Lens' UpdateDomainConfiguration (Prelude.Maybe DomainConfigurationStatus)
updateDomainConfiguration_domainConfigurationStatus = Lens.lens (\UpdateDomainConfiguration' {domainConfigurationStatus} -> domainConfigurationStatus) (\s@UpdateDomainConfiguration' {} a -> s {domainConfigurationStatus = a} :: UpdateDomainConfiguration)

-- | Removes the authorization configuration from a domain.
updateDomainConfiguration_removeAuthorizerConfig :: Lens.Lens' UpdateDomainConfiguration (Prelude.Maybe Prelude.Bool)
updateDomainConfiguration_removeAuthorizerConfig = Lens.lens (\UpdateDomainConfiguration' {removeAuthorizerConfig} -> removeAuthorizerConfig) (\s@UpdateDomainConfiguration' {} a -> s {removeAuthorizerConfig = a} :: UpdateDomainConfiguration)

-- | An object that specifies the TLS configuration for a domain.
updateDomainConfiguration_tlsConfig :: Lens.Lens' UpdateDomainConfiguration (Prelude.Maybe TlsConfig)
updateDomainConfiguration_tlsConfig = Lens.lens (\UpdateDomainConfiguration' {tlsConfig} -> tlsConfig) (\s@UpdateDomainConfiguration' {} a -> s {tlsConfig = a} :: UpdateDomainConfiguration)

-- | The name of the domain configuration to be updated.
updateDomainConfiguration_domainConfigurationName :: Lens.Lens' UpdateDomainConfiguration Prelude.Text
updateDomainConfiguration_domainConfigurationName = Lens.lens (\UpdateDomainConfiguration' {domainConfigurationName} -> domainConfigurationName) (\s@UpdateDomainConfiguration' {} a -> s {domainConfigurationName = a} :: UpdateDomainConfiguration)

instance Core.AWSRequest UpdateDomainConfiguration where
  type
    AWSResponse UpdateDomainConfiguration =
      UpdateDomainConfigurationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDomainConfigurationResponse'
            Prelude.<$> (x Data..?> "domainConfigurationArn")
            Prelude.<*> (x Data..?> "domainConfigurationName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDomainConfiguration where
  hashWithSalt _salt UpdateDomainConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` authorizerConfig
      `Prelude.hashWithSalt` domainConfigurationStatus
      `Prelude.hashWithSalt` removeAuthorizerConfig
      `Prelude.hashWithSalt` tlsConfig
      `Prelude.hashWithSalt` domainConfigurationName

instance Prelude.NFData UpdateDomainConfiguration where
  rnf UpdateDomainConfiguration' {..} =
    Prelude.rnf authorizerConfig
      `Prelude.seq` Prelude.rnf domainConfigurationStatus
      `Prelude.seq` Prelude.rnf removeAuthorizerConfig
      `Prelude.seq` Prelude.rnf tlsConfig
      `Prelude.seq` Prelude.rnf domainConfigurationName

instance Data.ToHeaders UpdateDomainConfiguration where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateDomainConfiguration where
  toJSON UpdateDomainConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("authorizerConfig" Data..=)
              Prelude.<$> authorizerConfig,
            ("domainConfigurationStatus" Data..=)
              Prelude.<$> domainConfigurationStatus,
            ("removeAuthorizerConfig" Data..=)
              Prelude.<$> removeAuthorizerConfig,
            ("tlsConfig" Data..=) Prelude.<$> tlsConfig
          ]
      )

instance Data.ToPath UpdateDomainConfiguration where
  toPath UpdateDomainConfiguration' {..} =
    Prelude.mconcat
      [ "/domainConfigurations/",
        Data.toBS domainConfigurationName
      ]

instance Data.ToQuery UpdateDomainConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDomainConfigurationResponse' smart constructor.
data UpdateDomainConfigurationResponse = UpdateDomainConfigurationResponse'
  { -- | The ARN of the domain configuration that was updated.
    domainConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain configuration that was updated.
    domainConfigurationName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDomainConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainConfigurationArn', 'updateDomainConfigurationResponse_domainConfigurationArn' - The ARN of the domain configuration that was updated.
--
-- 'domainConfigurationName', 'updateDomainConfigurationResponse_domainConfigurationName' - The name of the domain configuration that was updated.
--
-- 'httpStatus', 'updateDomainConfigurationResponse_httpStatus' - The response's http status code.
newUpdateDomainConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDomainConfigurationResponse
newUpdateDomainConfigurationResponse pHttpStatus_ =
  UpdateDomainConfigurationResponse'
    { domainConfigurationArn =
        Prelude.Nothing,
      domainConfigurationName =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the domain configuration that was updated.
updateDomainConfigurationResponse_domainConfigurationArn :: Lens.Lens' UpdateDomainConfigurationResponse (Prelude.Maybe Prelude.Text)
updateDomainConfigurationResponse_domainConfigurationArn = Lens.lens (\UpdateDomainConfigurationResponse' {domainConfigurationArn} -> domainConfigurationArn) (\s@UpdateDomainConfigurationResponse' {} a -> s {domainConfigurationArn = a} :: UpdateDomainConfigurationResponse)

-- | The name of the domain configuration that was updated.
updateDomainConfigurationResponse_domainConfigurationName :: Lens.Lens' UpdateDomainConfigurationResponse (Prelude.Maybe Prelude.Text)
updateDomainConfigurationResponse_domainConfigurationName = Lens.lens (\UpdateDomainConfigurationResponse' {domainConfigurationName} -> domainConfigurationName) (\s@UpdateDomainConfigurationResponse' {} a -> s {domainConfigurationName = a} :: UpdateDomainConfigurationResponse)

-- | The response's http status code.
updateDomainConfigurationResponse_httpStatus :: Lens.Lens' UpdateDomainConfigurationResponse Prelude.Int
updateDomainConfigurationResponse_httpStatus = Lens.lens (\UpdateDomainConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateDomainConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateDomainConfigurationResponse)

instance
  Prelude.NFData
    UpdateDomainConfigurationResponse
  where
  rnf UpdateDomainConfigurationResponse' {..} =
    Prelude.rnf domainConfigurationArn
      `Prelude.seq` Prelude.rnf domainConfigurationName
      `Prelude.seq` Prelude.rnf httpStatus
