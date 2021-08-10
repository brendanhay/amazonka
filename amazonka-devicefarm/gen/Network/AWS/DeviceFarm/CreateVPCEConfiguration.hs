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
-- Module      : Network.AWS.DeviceFarm.CreateVPCEConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a configuration record in Device Farm for your Amazon Virtual
-- Private Cloud (VPC) endpoint.
module Network.AWS.DeviceFarm.CreateVPCEConfiguration
  ( -- * Creating a Request
    CreateVPCEConfiguration (..),
    newCreateVPCEConfiguration,

    -- * Request Lenses
    createVPCEConfiguration_vpceConfigurationDescription,
    createVPCEConfiguration_vpceConfigurationName,
    createVPCEConfiguration_vpceServiceName,
    createVPCEConfiguration_serviceDnsName,

    -- * Destructuring the Response
    CreateVPCEConfigurationResponse (..),
    newCreateVPCEConfigurationResponse,

    -- * Response Lenses
    createVPCEConfigurationResponse_vpceConfiguration,
    createVPCEConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateVPCEConfiguration' smart constructor.
data CreateVPCEConfiguration = CreateVPCEConfiguration'
  { -- | An optional description that provides details about your VPC endpoint
    -- configuration.
    vpceConfigurationDescription :: Prelude.Maybe Prelude.Text,
    -- | The friendly name you give to your VPC endpoint configuration, to manage
    -- your configurations more easily.
    vpceConfigurationName :: Prelude.Text,
    -- | The name of the VPC endpoint service running in your AWS account that
    -- you want Device Farm to test.
    vpceServiceName :: Prelude.Text,
    -- | The DNS name of the service running in your VPC that you want Device
    -- Farm to test.
    serviceDnsName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVPCEConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpceConfigurationDescription', 'createVPCEConfiguration_vpceConfigurationDescription' - An optional description that provides details about your VPC endpoint
-- configuration.
--
-- 'vpceConfigurationName', 'createVPCEConfiguration_vpceConfigurationName' - The friendly name you give to your VPC endpoint configuration, to manage
-- your configurations more easily.
--
-- 'vpceServiceName', 'createVPCEConfiguration_vpceServiceName' - The name of the VPC endpoint service running in your AWS account that
-- you want Device Farm to test.
--
-- 'serviceDnsName', 'createVPCEConfiguration_serviceDnsName' - The DNS name of the service running in your VPC that you want Device
-- Farm to test.
newCreateVPCEConfiguration ::
  -- | 'vpceConfigurationName'
  Prelude.Text ->
  -- | 'vpceServiceName'
  Prelude.Text ->
  -- | 'serviceDnsName'
  Prelude.Text ->
  CreateVPCEConfiguration
newCreateVPCEConfiguration
  pVpceConfigurationName_
  pVpceServiceName_
  pServiceDnsName_ =
    CreateVPCEConfiguration'
      { vpceConfigurationDescription =
          Prelude.Nothing,
        vpceConfigurationName = pVpceConfigurationName_,
        vpceServiceName = pVpceServiceName_,
        serviceDnsName = pServiceDnsName_
      }

-- | An optional description that provides details about your VPC endpoint
-- configuration.
createVPCEConfiguration_vpceConfigurationDescription :: Lens.Lens' CreateVPCEConfiguration (Prelude.Maybe Prelude.Text)
createVPCEConfiguration_vpceConfigurationDescription = Lens.lens (\CreateVPCEConfiguration' {vpceConfigurationDescription} -> vpceConfigurationDescription) (\s@CreateVPCEConfiguration' {} a -> s {vpceConfigurationDescription = a} :: CreateVPCEConfiguration)

-- | The friendly name you give to your VPC endpoint configuration, to manage
-- your configurations more easily.
createVPCEConfiguration_vpceConfigurationName :: Lens.Lens' CreateVPCEConfiguration Prelude.Text
createVPCEConfiguration_vpceConfigurationName = Lens.lens (\CreateVPCEConfiguration' {vpceConfigurationName} -> vpceConfigurationName) (\s@CreateVPCEConfiguration' {} a -> s {vpceConfigurationName = a} :: CreateVPCEConfiguration)

-- | The name of the VPC endpoint service running in your AWS account that
-- you want Device Farm to test.
createVPCEConfiguration_vpceServiceName :: Lens.Lens' CreateVPCEConfiguration Prelude.Text
createVPCEConfiguration_vpceServiceName = Lens.lens (\CreateVPCEConfiguration' {vpceServiceName} -> vpceServiceName) (\s@CreateVPCEConfiguration' {} a -> s {vpceServiceName = a} :: CreateVPCEConfiguration)

-- | The DNS name of the service running in your VPC that you want Device
-- Farm to test.
createVPCEConfiguration_serviceDnsName :: Lens.Lens' CreateVPCEConfiguration Prelude.Text
createVPCEConfiguration_serviceDnsName = Lens.lens (\CreateVPCEConfiguration' {serviceDnsName} -> serviceDnsName) (\s@CreateVPCEConfiguration' {} a -> s {serviceDnsName = a} :: CreateVPCEConfiguration)

instance Core.AWSRequest CreateVPCEConfiguration where
  type
    AWSResponse CreateVPCEConfiguration =
      CreateVPCEConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVPCEConfigurationResponse'
            Prelude.<$> (x Core..?> "vpceConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateVPCEConfiguration

instance Prelude.NFData CreateVPCEConfiguration

instance Core.ToHeaders CreateVPCEConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.CreateVPCEConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateVPCEConfiguration where
  toJSON CreateVPCEConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("vpceConfigurationDescription" Core..=)
              Prelude.<$> vpceConfigurationDescription,
            Prelude.Just
              ( "vpceConfigurationName"
                  Core..= vpceConfigurationName
              ),
            Prelude.Just
              ("vpceServiceName" Core..= vpceServiceName),
            Prelude.Just
              ("serviceDnsName" Core..= serviceDnsName)
          ]
      )

instance Core.ToPath CreateVPCEConfiguration where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateVPCEConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateVPCEConfigurationResponse' smart constructor.
data CreateVPCEConfigurationResponse = CreateVPCEConfigurationResponse'
  { -- | An object that contains information about your VPC endpoint
    -- configuration.
    vpceConfiguration :: Prelude.Maybe VPCEConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVPCEConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpceConfiguration', 'createVPCEConfigurationResponse_vpceConfiguration' - An object that contains information about your VPC endpoint
-- configuration.
--
-- 'httpStatus', 'createVPCEConfigurationResponse_httpStatus' - The response's http status code.
newCreateVPCEConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVPCEConfigurationResponse
newCreateVPCEConfigurationResponse pHttpStatus_ =
  CreateVPCEConfigurationResponse'
    { vpceConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains information about your VPC endpoint
-- configuration.
createVPCEConfigurationResponse_vpceConfiguration :: Lens.Lens' CreateVPCEConfigurationResponse (Prelude.Maybe VPCEConfiguration)
createVPCEConfigurationResponse_vpceConfiguration = Lens.lens (\CreateVPCEConfigurationResponse' {vpceConfiguration} -> vpceConfiguration) (\s@CreateVPCEConfigurationResponse' {} a -> s {vpceConfiguration = a} :: CreateVPCEConfigurationResponse)

-- | The response's http status code.
createVPCEConfigurationResponse_httpStatus :: Lens.Lens' CreateVPCEConfigurationResponse Prelude.Int
createVPCEConfigurationResponse_httpStatus = Lens.lens (\CreateVPCEConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreateVPCEConfigurationResponse' {} a -> s {httpStatus = a} :: CreateVPCEConfigurationResponse)

instance
  Prelude.NFData
    CreateVPCEConfigurationResponse
