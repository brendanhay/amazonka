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
import qualified Network.AWS.Prelude as Prelude
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
    power :: Prelude.Maybe ContainerServicePowerName,
    -- | The scale for the container service.
    --
    -- The scale specifies the allocated compute nodes of the container
    -- service. The @power@ and @scale@ of a container service makes up its
    -- configured capacity. To determine the monthly price of your container
    -- service, multiply the base price of the @power@ with the @scale@ (the
    -- number of nodes) of the service.
    scale :: Prelude.Maybe Prelude.Natural,
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
    publicDomainNames :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | A Boolean value to indicate whether the container service is disabled.
    isDisabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the container service to update.
    serviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  UpdateContainerService
newUpdateContainerService pServiceName_ =
  UpdateContainerService'
    { power = Prelude.Nothing,
      scale = Prelude.Nothing,
      publicDomainNames = Prelude.Nothing,
      isDisabled = Prelude.Nothing,
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
updateContainerService_power :: Lens.Lens' UpdateContainerService (Prelude.Maybe ContainerServicePowerName)
updateContainerService_power = Lens.lens (\UpdateContainerService' {power} -> power) (\s@UpdateContainerService' {} a -> s {power = a} :: UpdateContainerService)

-- | The scale for the container service.
--
-- The scale specifies the allocated compute nodes of the container
-- service. The @power@ and @scale@ of a container service makes up its
-- configured capacity. To determine the monthly price of your container
-- service, multiply the base price of the @power@ with the @scale@ (the
-- number of nodes) of the service.
updateContainerService_scale :: Lens.Lens' UpdateContainerService (Prelude.Maybe Prelude.Natural)
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
updateContainerService_publicDomainNames :: Lens.Lens' UpdateContainerService (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
updateContainerService_publicDomainNames = Lens.lens (\UpdateContainerService' {publicDomainNames} -> publicDomainNames) (\s@UpdateContainerService' {} a -> s {publicDomainNames = a} :: UpdateContainerService) Prelude.. Lens.mapping Lens._Coerce

-- | A Boolean value to indicate whether the container service is disabled.
updateContainerService_isDisabled :: Lens.Lens' UpdateContainerService (Prelude.Maybe Prelude.Bool)
updateContainerService_isDisabled = Lens.lens (\UpdateContainerService' {isDisabled} -> isDisabled) (\s@UpdateContainerService' {} a -> s {isDisabled = a} :: UpdateContainerService)

-- | The name of the container service to update.
updateContainerService_serviceName :: Lens.Lens' UpdateContainerService Prelude.Text
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
            Prelude.<$> (x Core..?> "containerService")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateContainerService

instance Prelude.NFData UpdateContainerService

instance Core.ToHeaders UpdateContainerService where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.UpdateContainerService" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateContainerService where
  toJSON UpdateContainerService' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("power" Core..=) Prelude.<$> power,
            ("scale" Core..=) Prelude.<$> scale,
            ("publicDomainNames" Core..=)
              Prelude.<$> publicDomainNames,
            ("isDisabled" Core..=) Prelude.<$> isDisabled,
            Prelude.Just ("serviceName" Core..= serviceName)
          ]
      )

instance Core.ToPath UpdateContainerService where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateContainerService where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateContainerServiceResponse' smart constructor.
data UpdateContainerServiceResponse = UpdateContainerServiceResponse'
  { -- | An object that describes a container service.
    containerService :: Prelude.Maybe ContainerService,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateContainerServiceResponse
newUpdateContainerServiceResponse pHttpStatus_ =
  UpdateContainerServiceResponse'
    { containerService =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that describes a container service.
updateContainerServiceResponse_containerService :: Lens.Lens' UpdateContainerServiceResponse (Prelude.Maybe ContainerService)
updateContainerServiceResponse_containerService = Lens.lens (\UpdateContainerServiceResponse' {containerService} -> containerService) (\s@UpdateContainerServiceResponse' {} a -> s {containerService = a} :: UpdateContainerServiceResponse)

-- | The response's http status code.
updateContainerServiceResponse_httpStatus :: Lens.Lens' UpdateContainerServiceResponse Prelude.Int
updateContainerServiceResponse_httpStatus = Lens.lens (\UpdateContainerServiceResponse' {httpStatus} -> httpStatus) (\s@UpdateContainerServiceResponse' {} a -> s {httpStatus = a} :: UpdateContainerServiceResponse)

instance
  Prelude.NFData
    UpdateContainerServiceResponse
