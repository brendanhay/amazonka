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
-- Module      : Network.AWS.Lightsail.UpdateContainerService
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration of your Amazon Lightsail container service,
-- such as its power, scale, and public domain names.
module Network.AWS.Lightsail.UpdateContainerService
  ( -- * Creating a Request
    UpdateContainerService (..),
    newUpdateContainerService,

    -- * Request Lenses
    updateContainerService_power,
    updateContainerService_scale,
    updateContainerService_publicDomainNames,
    updateContainerService_isDisabled,
    updateContainerService_serviceName,

    -- * Destructuring the Response
    UpdateContainerServiceResponse (..),
    newUpdateContainerServiceResponse,

    -- * Response Lenses
    updateContainerServiceResponse_containerService,
    updateContainerServiceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateContainerService' smart constructor.
data UpdateContainerService = UpdateContainerService'
  { -- | The power for the container service.
    --
    -- The power specifies the amount of memory, vCPUs, and base monthly cost
    -- of each node of the container service. The @power@ and @scale@ of a
    -- container service makes up its configured capacity. To determine the
    -- monthly price of your container service, multiply the base price of the
    -- @power@ with the @scale@ (the number of nodes) of the service.
    --
    -- Use the @GetContainerServicePowers@ action to view the specifications of
    -- each power option.
    power :: Core.Maybe ContainerServicePowerName,
    -- | The scale for the container service.
    --
    -- The scale specifies the allocated compute nodes of the container
    -- service. The @power@ and @scale@ of a container service makes up its
    -- configured capacity. To determine the monthly price of your container
    -- service, multiply the base price of the @power@ with the @scale@ (the
    -- number of nodes) of the service.
    scale :: Core.Maybe Core.Natural,
    -- | The public domain names to use with the container service, such as
    -- @example.com@ and @www.example.com@.
    --
    -- You can specify up to four public domain names for a container service.
    -- The domain names that you specify are used when you create a deployment
    -- with a container configured as the public endpoint of your container
    -- service.
    --
    -- If you don\'t specify public domain names, then you can use the default
    -- domain of the container service.
    --
    -- You must create and validate an SSL\/TLS certificate before you can use
    -- public domain names with your container service. Use the
    -- @CreateCertificate@ action to create a certificate for the public domain
    -- names you want to use with your container service.
    --
    -- You can specify public domain names using a string to array map as shown
    -- in the example later on this page.
    publicDomainNames :: Core.Maybe (Core.HashMap Core.Text [Core.Text]),
    -- | A Boolean value to indicate whether the container service is disabled.
    isDisabled :: Core.Maybe Core.Bool,
    -- | The name of the container service to update.
    serviceName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateContainerService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'power', 'updateContainerService_power' - The power for the container service.
--
-- The power specifies the amount of memory, vCPUs, and base monthly cost
-- of each node of the container service. The @power@ and @scale@ of a
-- container service makes up its configured capacity. To determine the
-- monthly price of your container service, multiply the base price of the
-- @power@ with the @scale@ (the number of nodes) of the service.
--
-- Use the @GetContainerServicePowers@ action to view the specifications of
-- each power option.
--
-- 'scale', 'updateContainerService_scale' - The scale for the container service.
--
-- The scale specifies the allocated compute nodes of the container
-- service. The @power@ and @scale@ of a container service makes up its
-- configured capacity. To determine the monthly price of your container
-- service, multiply the base price of the @power@ with the @scale@ (the
-- number of nodes) of the service.
--
-- 'publicDomainNames', 'updateContainerService_publicDomainNames' - The public domain names to use with the container service, such as
-- @example.com@ and @www.example.com@.
--
-- You can specify up to four public domain names for a container service.
-- The domain names that you specify are used when you create a deployment
-- with a container configured as the public endpoint of your container
-- service.
--
-- If you don\'t specify public domain names, then you can use the default
-- domain of the container service.
--
-- You must create and validate an SSL\/TLS certificate before you can use
-- public domain names with your container service. Use the
-- @CreateCertificate@ action to create a certificate for the public domain
-- names you want to use with your container service.
--
-- You can specify public domain names using a string to array map as shown
-- in the example later on this page.
--
-- 'isDisabled', 'updateContainerService_isDisabled' - A Boolean value to indicate whether the container service is disabled.
--
-- 'serviceName', 'updateContainerService_serviceName' - The name of the container service to update.
newUpdateContainerService ::
  -- | 'serviceName'
  Core.Text ->
  UpdateContainerService
newUpdateContainerService pServiceName_ =
  UpdateContainerService'
    { power = Core.Nothing,
      scale = Core.Nothing,
      publicDomainNames = Core.Nothing,
      isDisabled = Core.Nothing,
      serviceName = pServiceName_
    }

-- | The power for the container service.
--
-- The power specifies the amount of memory, vCPUs, and base monthly cost
-- of each node of the container service. The @power@ and @scale@ of a
-- container service makes up its configured capacity. To determine the
-- monthly price of your container service, multiply the base price of the
-- @power@ with the @scale@ (the number of nodes) of the service.
--
-- Use the @GetContainerServicePowers@ action to view the specifications of
-- each power option.
updateContainerService_power :: Lens.Lens' UpdateContainerService (Core.Maybe ContainerServicePowerName)
updateContainerService_power = Lens.lens (\UpdateContainerService' {power} -> power) (\s@UpdateContainerService' {} a -> s {power = a} :: UpdateContainerService)

-- | The scale for the container service.
--
-- The scale specifies the allocated compute nodes of the container
-- service. The @power@ and @scale@ of a container service makes up its
-- configured capacity. To determine the monthly price of your container
-- service, multiply the base price of the @power@ with the @scale@ (the
-- number of nodes) of the service.
updateContainerService_scale :: Lens.Lens' UpdateContainerService (Core.Maybe Core.Natural)
updateContainerService_scale = Lens.lens (\UpdateContainerService' {scale} -> scale) (\s@UpdateContainerService' {} a -> s {scale = a} :: UpdateContainerService)

-- | The public domain names to use with the container service, such as
-- @example.com@ and @www.example.com@.
--
-- You can specify up to four public domain names for a container service.
-- The domain names that you specify are used when you create a deployment
-- with a container configured as the public endpoint of your container
-- service.
--
-- If you don\'t specify public domain names, then you can use the default
-- domain of the container service.
--
-- You must create and validate an SSL\/TLS certificate before you can use
-- public domain names with your container service. Use the
-- @CreateCertificate@ action to create a certificate for the public domain
-- names you want to use with your container service.
--
-- You can specify public domain names using a string to array map as shown
-- in the example later on this page.
updateContainerService_publicDomainNames :: Lens.Lens' UpdateContainerService (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
updateContainerService_publicDomainNames = Lens.lens (\UpdateContainerService' {publicDomainNames} -> publicDomainNames) (\s@UpdateContainerService' {} a -> s {publicDomainNames = a} :: UpdateContainerService) Core.. Lens.mapping Lens._Coerce

-- | A Boolean value to indicate whether the container service is disabled.
updateContainerService_isDisabled :: Lens.Lens' UpdateContainerService (Core.Maybe Core.Bool)
updateContainerService_isDisabled = Lens.lens (\UpdateContainerService' {isDisabled} -> isDisabled) (\s@UpdateContainerService' {} a -> s {isDisabled = a} :: UpdateContainerService)

-- | The name of the container service to update.
updateContainerService_serviceName :: Lens.Lens' UpdateContainerService Core.Text
updateContainerService_serviceName = Lens.lens (\UpdateContainerService' {serviceName} -> serviceName) (\s@UpdateContainerService' {} a -> s {serviceName = a} :: UpdateContainerService)

instance Core.AWSRequest UpdateContainerService where
  type
    AWSResponse UpdateContainerService =
      UpdateContainerServiceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateContainerServiceResponse'
            Core.<$> (x Core..?> "containerService")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateContainerService

instance Core.NFData UpdateContainerService

instance Core.ToHeaders UpdateContainerService where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.UpdateContainerService" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateContainerService where
  toJSON UpdateContainerService' {..} =
    Core.object
      ( Core.catMaybes
          [ ("power" Core..=) Core.<$> power,
            ("scale" Core..=) Core.<$> scale,
            ("publicDomainNames" Core..=)
              Core.<$> publicDomainNames,
            ("isDisabled" Core..=) Core.<$> isDisabled,
            Core.Just ("serviceName" Core..= serviceName)
          ]
      )

instance Core.ToPath UpdateContainerService where
  toPath = Core.const "/"

instance Core.ToQuery UpdateContainerService where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateContainerServiceResponse' smart constructor.
data UpdateContainerServiceResponse = UpdateContainerServiceResponse'
  { -- | An object that describes a container service.
    containerService :: Core.Maybe ContainerService,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateContainerServiceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerService', 'updateContainerServiceResponse_containerService' - An object that describes a container service.
--
-- 'httpStatus', 'updateContainerServiceResponse_httpStatus' - The response's http status code.
newUpdateContainerServiceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateContainerServiceResponse
newUpdateContainerServiceResponse pHttpStatus_ =
  UpdateContainerServiceResponse'
    { containerService =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that describes a container service.
updateContainerServiceResponse_containerService :: Lens.Lens' UpdateContainerServiceResponse (Core.Maybe ContainerService)
updateContainerServiceResponse_containerService = Lens.lens (\UpdateContainerServiceResponse' {containerService} -> containerService) (\s@UpdateContainerServiceResponse' {} a -> s {containerService = a} :: UpdateContainerServiceResponse)

-- | The response's http status code.
updateContainerServiceResponse_httpStatus :: Lens.Lens' UpdateContainerServiceResponse Core.Int
updateContainerServiceResponse_httpStatus = Lens.lens (\UpdateContainerServiceResponse' {httpStatus} -> httpStatus) (\s@UpdateContainerServiceResponse' {} a -> s {httpStatus = a} :: UpdateContainerServiceResponse)

instance Core.NFData UpdateContainerServiceResponse
