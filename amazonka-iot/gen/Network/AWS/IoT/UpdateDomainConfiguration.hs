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

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateDomainConfiguration' smart constructor.
data UpdateDomainConfiguration = UpdateDomainConfiguration'
  { -- | The status to which the domain configuration should be updated.
    domainConfigurationStatus :: Core.Maybe DomainConfigurationStatus,
    -- | An object that specifies the authorization service for a domain.
    authorizerConfig :: Core.Maybe AuthorizerConfig,
    -- | Removes the authorization configuration from a domain.
    removeAuthorizerConfig :: Core.Maybe Core.Bool,
    -- | The name of the domain configuration to be updated.
    domainConfigurationName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  UpdateDomainConfiguration
newUpdateDomainConfiguration
  pDomainConfigurationName_ =
    UpdateDomainConfiguration'
      { domainConfigurationStatus =
          Core.Nothing,
        authorizerConfig = Core.Nothing,
        removeAuthorizerConfig = Core.Nothing,
        domainConfigurationName =
          pDomainConfigurationName_
      }

-- | The status to which the domain configuration should be updated.
updateDomainConfiguration_domainConfigurationStatus :: Lens.Lens' UpdateDomainConfiguration (Core.Maybe DomainConfigurationStatus)
updateDomainConfiguration_domainConfigurationStatus = Lens.lens (\UpdateDomainConfiguration' {domainConfigurationStatus} -> domainConfigurationStatus) (\s@UpdateDomainConfiguration' {} a -> s {domainConfigurationStatus = a} :: UpdateDomainConfiguration)

-- | An object that specifies the authorization service for a domain.
updateDomainConfiguration_authorizerConfig :: Lens.Lens' UpdateDomainConfiguration (Core.Maybe AuthorizerConfig)
updateDomainConfiguration_authorizerConfig = Lens.lens (\UpdateDomainConfiguration' {authorizerConfig} -> authorizerConfig) (\s@UpdateDomainConfiguration' {} a -> s {authorizerConfig = a} :: UpdateDomainConfiguration)

-- | Removes the authorization configuration from a domain.
updateDomainConfiguration_removeAuthorizerConfig :: Lens.Lens' UpdateDomainConfiguration (Core.Maybe Core.Bool)
updateDomainConfiguration_removeAuthorizerConfig = Lens.lens (\UpdateDomainConfiguration' {removeAuthorizerConfig} -> removeAuthorizerConfig) (\s@UpdateDomainConfiguration' {} a -> s {removeAuthorizerConfig = a} :: UpdateDomainConfiguration)

-- | The name of the domain configuration to be updated.
updateDomainConfiguration_domainConfigurationName :: Lens.Lens' UpdateDomainConfiguration Core.Text
updateDomainConfiguration_domainConfigurationName = Lens.lens (\UpdateDomainConfiguration' {domainConfigurationName} -> domainConfigurationName) (\s@UpdateDomainConfiguration' {} a -> s {domainConfigurationName = a} :: UpdateDomainConfiguration)

instance Core.AWSRequest UpdateDomainConfiguration where
  type
    AWSResponse UpdateDomainConfiguration =
      UpdateDomainConfigurationResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDomainConfigurationResponse'
            Core.<$> (x Core..?> "domainConfigurationArn")
            Core.<*> (x Core..?> "domainConfigurationName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateDomainConfiguration

instance Core.NFData UpdateDomainConfiguration

instance Core.ToHeaders UpdateDomainConfiguration where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON UpdateDomainConfiguration where
  toJSON UpdateDomainConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("domainConfigurationStatus" Core..=)
              Core.<$> domainConfigurationStatus,
            ("authorizerConfig" Core..=)
              Core.<$> authorizerConfig,
            ("removeAuthorizerConfig" Core..=)
              Core.<$> removeAuthorizerConfig
          ]
      )

instance Core.ToPath UpdateDomainConfiguration where
  toPath UpdateDomainConfiguration' {..} =
    Core.mconcat
      [ "/domainConfigurations/",
        Core.toBS domainConfigurationName
      ]

instance Core.ToQuery UpdateDomainConfiguration where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateDomainConfigurationResponse' smart constructor.
data UpdateDomainConfigurationResponse = UpdateDomainConfigurationResponse'
  { -- | The ARN of the domain configuration that was updated.
    domainConfigurationArn :: Core.Maybe Core.Text,
    -- | The name of the domain configuration that was updated.
    domainConfigurationName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateDomainConfigurationResponse
newUpdateDomainConfigurationResponse pHttpStatus_ =
  UpdateDomainConfigurationResponse'
    { domainConfigurationArn =
        Core.Nothing,
      domainConfigurationName = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the domain configuration that was updated.
updateDomainConfigurationResponse_domainConfigurationArn :: Lens.Lens' UpdateDomainConfigurationResponse (Core.Maybe Core.Text)
updateDomainConfigurationResponse_domainConfigurationArn = Lens.lens (\UpdateDomainConfigurationResponse' {domainConfigurationArn} -> domainConfigurationArn) (\s@UpdateDomainConfigurationResponse' {} a -> s {domainConfigurationArn = a} :: UpdateDomainConfigurationResponse)

-- | The name of the domain configuration that was updated.
updateDomainConfigurationResponse_domainConfigurationName :: Lens.Lens' UpdateDomainConfigurationResponse (Core.Maybe Core.Text)
updateDomainConfigurationResponse_domainConfigurationName = Lens.lens (\UpdateDomainConfigurationResponse' {domainConfigurationName} -> domainConfigurationName) (\s@UpdateDomainConfigurationResponse' {} a -> s {domainConfigurationName = a} :: UpdateDomainConfigurationResponse)

-- | The response's http status code.
updateDomainConfigurationResponse_httpStatus :: Lens.Lens' UpdateDomainConfigurationResponse Core.Int
updateDomainConfigurationResponse_httpStatus = Lens.lens (\UpdateDomainConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateDomainConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateDomainConfigurationResponse)

instance
  Core.NFData
    UpdateDomainConfigurationResponse
