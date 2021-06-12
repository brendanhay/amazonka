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
-- Module      : Network.AWS.DeviceFarm.UpdateVPCEConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information about an Amazon Virtual Private Cloud (VPC) endpoint
-- configuration.
module Network.AWS.DeviceFarm.UpdateVPCEConfiguration
  ( -- * Creating a Request
    UpdateVPCEConfiguration (..),
    newUpdateVPCEConfiguration,

    -- * Request Lenses
    updateVPCEConfiguration_vpceConfigurationName,
    updateVPCEConfiguration_vpceConfigurationDescription,
    updateVPCEConfiguration_serviceDnsName,
    updateVPCEConfiguration_vpceServiceName,
    updateVPCEConfiguration_arn,

    -- * Destructuring the Response
    UpdateVPCEConfigurationResponse (..),
    newUpdateVPCEConfigurationResponse,

    -- * Response Lenses
    updateVPCEConfigurationResponse_vpceConfiguration,
    updateVPCEConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateVPCEConfiguration' smart constructor.
data UpdateVPCEConfiguration = UpdateVPCEConfiguration'
  { -- | The friendly name you give to your VPC endpoint configuration to manage
    -- your configurations more easily.
    vpceConfigurationName :: Core.Maybe Core.Text,
    -- | An optional description that provides details about your VPC endpoint
    -- configuration.
    vpceConfigurationDescription :: Core.Maybe Core.Text,
    -- | The DNS (domain) name used to connect to your private service in your
    -- VPC. The DNS name must not already be in use on the internet.
    serviceDnsName :: Core.Maybe Core.Text,
    -- | The name of the VPC endpoint service running in your AWS account that
    -- you want Device Farm to test.
    vpceServiceName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the VPC endpoint configuration you
    -- want to update.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateVPCEConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpceConfigurationName', 'updateVPCEConfiguration_vpceConfigurationName' - The friendly name you give to your VPC endpoint configuration to manage
-- your configurations more easily.
--
-- 'vpceConfigurationDescription', 'updateVPCEConfiguration_vpceConfigurationDescription' - An optional description that provides details about your VPC endpoint
-- configuration.
--
-- 'serviceDnsName', 'updateVPCEConfiguration_serviceDnsName' - The DNS (domain) name used to connect to your private service in your
-- VPC. The DNS name must not already be in use on the internet.
--
-- 'vpceServiceName', 'updateVPCEConfiguration_vpceServiceName' - The name of the VPC endpoint service running in your AWS account that
-- you want Device Farm to test.
--
-- 'arn', 'updateVPCEConfiguration_arn' - The Amazon Resource Name (ARN) of the VPC endpoint configuration you
-- want to update.
newUpdateVPCEConfiguration ::
  -- | 'arn'
  Core.Text ->
  UpdateVPCEConfiguration
newUpdateVPCEConfiguration pArn_ =
  UpdateVPCEConfiguration'
    { vpceConfigurationName =
        Core.Nothing,
      vpceConfigurationDescription = Core.Nothing,
      serviceDnsName = Core.Nothing,
      vpceServiceName = Core.Nothing,
      arn = pArn_
    }

-- | The friendly name you give to your VPC endpoint configuration to manage
-- your configurations more easily.
updateVPCEConfiguration_vpceConfigurationName :: Lens.Lens' UpdateVPCEConfiguration (Core.Maybe Core.Text)
updateVPCEConfiguration_vpceConfigurationName = Lens.lens (\UpdateVPCEConfiguration' {vpceConfigurationName} -> vpceConfigurationName) (\s@UpdateVPCEConfiguration' {} a -> s {vpceConfigurationName = a} :: UpdateVPCEConfiguration)

-- | An optional description that provides details about your VPC endpoint
-- configuration.
updateVPCEConfiguration_vpceConfigurationDescription :: Lens.Lens' UpdateVPCEConfiguration (Core.Maybe Core.Text)
updateVPCEConfiguration_vpceConfigurationDescription = Lens.lens (\UpdateVPCEConfiguration' {vpceConfigurationDescription} -> vpceConfigurationDescription) (\s@UpdateVPCEConfiguration' {} a -> s {vpceConfigurationDescription = a} :: UpdateVPCEConfiguration)

-- | The DNS (domain) name used to connect to your private service in your
-- VPC. The DNS name must not already be in use on the internet.
updateVPCEConfiguration_serviceDnsName :: Lens.Lens' UpdateVPCEConfiguration (Core.Maybe Core.Text)
updateVPCEConfiguration_serviceDnsName = Lens.lens (\UpdateVPCEConfiguration' {serviceDnsName} -> serviceDnsName) (\s@UpdateVPCEConfiguration' {} a -> s {serviceDnsName = a} :: UpdateVPCEConfiguration)

-- | The name of the VPC endpoint service running in your AWS account that
-- you want Device Farm to test.
updateVPCEConfiguration_vpceServiceName :: Lens.Lens' UpdateVPCEConfiguration (Core.Maybe Core.Text)
updateVPCEConfiguration_vpceServiceName = Lens.lens (\UpdateVPCEConfiguration' {vpceServiceName} -> vpceServiceName) (\s@UpdateVPCEConfiguration' {} a -> s {vpceServiceName = a} :: UpdateVPCEConfiguration)

-- | The Amazon Resource Name (ARN) of the VPC endpoint configuration you
-- want to update.
updateVPCEConfiguration_arn :: Lens.Lens' UpdateVPCEConfiguration Core.Text
updateVPCEConfiguration_arn = Lens.lens (\UpdateVPCEConfiguration' {arn} -> arn) (\s@UpdateVPCEConfiguration' {} a -> s {arn = a} :: UpdateVPCEConfiguration)

instance Core.AWSRequest UpdateVPCEConfiguration where
  type
    AWSResponse UpdateVPCEConfiguration =
      UpdateVPCEConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateVPCEConfigurationResponse'
            Core.<$> (x Core..?> "vpceConfiguration")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateVPCEConfiguration

instance Core.NFData UpdateVPCEConfiguration

instance Core.ToHeaders UpdateVPCEConfiguration where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.UpdateVPCEConfiguration" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateVPCEConfiguration where
  toJSON UpdateVPCEConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("vpceConfigurationName" Core..=)
              Core.<$> vpceConfigurationName,
            ("vpceConfigurationDescription" Core..=)
              Core.<$> vpceConfigurationDescription,
            ("serviceDnsName" Core..=) Core.<$> serviceDnsName,
            ("vpceServiceName" Core..=) Core.<$> vpceServiceName,
            Core.Just ("arn" Core..= arn)
          ]
      )

instance Core.ToPath UpdateVPCEConfiguration where
  toPath = Core.const "/"

instance Core.ToQuery UpdateVPCEConfiguration where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateVPCEConfigurationResponse' smart constructor.
data UpdateVPCEConfigurationResponse = UpdateVPCEConfigurationResponse'
  { -- | An object that contains information about your VPC endpoint
    -- configuration.
    vpceConfiguration :: Core.Maybe VPCEConfiguration,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateVPCEConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpceConfiguration', 'updateVPCEConfigurationResponse_vpceConfiguration' - An object that contains information about your VPC endpoint
-- configuration.
--
-- 'httpStatus', 'updateVPCEConfigurationResponse_httpStatus' - The response's http status code.
newUpdateVPCEConfigurationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateVPCEConfigurationResponse
newUpdateVPCEConfigurationResponse pHttpStatus_ =
  UpdateVPCEConfigurationResponse'
    { vpceConfiguration =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains information about your VPC endpoint
-- configuration.
updateVPCEConfigurationResponse_vpceConfiguration :: Lens.Lens' UpdateVPCEConfigurationResponse (Core.Maybe VPCEConfiguration)
updateVPCEConfigurationResponse_vpceConfiguration = Lens.lens (\UpdateVPCEConfigurationResponse' {vpceConfiguration} -> vpceConfiguration) (\s@UpdateVPCEConfigurationResponse' {} a -> s {vpceConfiguration = a} :: UpdateVPCEConfigurationResponse)

-- | The response's http status code.
updateVPCEConfigurationResponse_httpStatus :: Lens.Lens' UpdateVPCEConfigurationResponse Core.Int
updateVPCEConfigurationResponse_httpStatus = Lens.lens (\UpdateVPCEConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateVPCEConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateVPCEConfigurationResponse)

instance Core.NFData UpdateVPCEConfigurationResponse
