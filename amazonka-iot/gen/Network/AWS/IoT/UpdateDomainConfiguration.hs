{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.UpdateDomainConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates values stored in the domain configuration. Domain configurations
-- for default endpoints can\'t be updated.
--
-- The domain configuration feature is in public preview and is subject to
-- change.
module Network.AWS.IoT.UpdateDomainConfiguration
  ( -- * Creating a Request
    UpdateDomainConfiguration (..),
    newUpdateDomainConfiguration,

    -- * Request Lenses
    updateDomainConfiguration_domainConfigurationStatus,
    updateDomainConfiguration_authorizerConfig,
    updateDomainConfiguration_removeAuthorizerConfig,
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateDomainConfiguration' smart constructor.
data UpdateDomainConfiguration = UpdateDomainConfiguration'
  { -- | The status to which the domain configuration should be updated.
    domainConfigurationStatus :: Prelude.Maybe DomainConfigurationStatus,
    -- | An object that specifies the authorization service for a domain.
    authorizerConfig :: Prelude.Maybe AuthorizerConfig,
    -- | Removes the authorization configuration from a domain.
    removeAuthorizerConfig :: Prelude.Maybe Prelude.Bool,
    -- | The name of the domain configuration to be updated.
    domainConfigurationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateDomainConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainConfigurationStatus', 'updateDomainConfiguration_domainConfigurationStatus' - The status to which the domain configuration should be updated.
--
-- 'authorizerConfig', 'updateDomainConfiguration_authorizerConfig' - An object that specifies the authorization service for a domain.
--
-- 'removeAuthorizerConfig', 'updateDomainConfiguration_removeAuthorizerConfig' - Removes the authorization configuration from a domain.
--
-- 'domainConfigurationName', 'updateDomainConfiguration_domainConfigurationName' - The name of the domain configuration to be updated.
newUpdateDomainConfiguration ::
  -- | 'domainConfigurationName'
  Prelude.Text ->
  UpdateDomainConfiguration
newUpdateDomainConfiguration
  pDomainConfigurationName_ =
    UpdateDomainConfiguration'
      { domainConfigurationStatus =
          Prelude.Nothing,
        authorizerConfig = Prelude.Nothing,
        removeAuthorizerConfig = Prelude.Nothing,
        domainConfigurationName =
          pDomainConfigurationName_
      }

-- | The status to which the domain configuration should be updated.
updateDomainConfiguration_domainConfigurationStatus :: Lens.Lens' UpdateDomainConfiguration (Prelude.Maybe DomainConfigurationStatus)
updateDomainConfiguration_domainConfigurationStatus = Lens.lens (\UpdateDomainConfiguration' {domainConfigurationStatus} -> domainConfigurationStatus) (\s@UpdateDomainConfiguration' {} a -> s {domainConfigurationStatus = a} :: UpdateDomainConfiguration)

-- | An object that specifies the authorization service for a domain.
updateDomainConfiguration_authorizerConfig :: Lens.Lens' UpdateDomainConfiguration (Prelude.Maybe AuthorizerConfig)
updateDomainConfiguration_authorizerConfig = Lens.lens (\UpdateDomainConfiguration' {authorizerConfig} -> authorizerConfig) (\s@UpdateDomainConfiguration' {} a -> s {authorizerConfig = a} :: UpdateDomainConfiguration)

-- | Removes the authorization configuration from a domain.
updateDomainConfiguration_removeAuthorizerConfig :: Lens.Lens' UpdateDomainConfiguration (Prelude.Maybe Prelude.Bool)
updateDomainConfiguration_removeAuthorizerConfig = Lens.lens (\UpdateDomainConfiguration' {removeAuthorizerConfig} -> removeAuthorizerConfig) (\s@UpdateDomainConfiguration' {} a -> s {removeAuthorizerConfig = a} :: UpdateDomainConfiguration)

-- | The name of the domain configuration to be updated.
updateDomainConfiguration_domainConfigurationName :: Lens.Lens' UpdateDomainConfiguration Prelude.Text
updateDomainConfiguration_domainConfigurationName = Lens.lens (\UpdateDomainConfiguration' {domainConfigurationName} -> domainConfigurationName) (\s@UpdateDomainConfiguration' {} a -> s {domainConfigurationName = a} :: UpdateDomainConfiguration)

instance Prelude.AWSRequest UpdateDomainConfiguration where
  type
    Rs UpdateDomainConfiguration =
      UpdateDomainConfigurationResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDomainConfigurationResponse'
            Prelude.<$> (x Prelude..?> "domainConfigurationArn")
            Prelude.<*> (x Prelude..?> "domainConfigurationName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDomainConfiguration

instance Prelude.NFData UpdateDomainConfiguration

instance Prelude.ToHeaders UpdateDomainConfiguration where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON UpdateDomainConfiguration where
  toJSON UpdateDomainConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("domainConfigurationStatus" Prelude..=)
              Prelude.<$> domainConfigurationStatus,
            ("authorizerConfig" Prelude..=)
              Prelude.<$> authorizerConfig,
            ("removeAuthorizerConfig" Prelude..=)
              Prelude.<$> removeAuthorizerConfig
          ]
      )

instance Prelude.ToPath UpdateDomainConfiguration where
  toPath UpdateDomainConfiguration' {..} =
    Prelude.mconcat
      [ "/domainConfigurations/",
        Prelude.toBS domainConfigurationName
      ]

instance Prelude.ToQuery UpdateDomainConfiguration where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
