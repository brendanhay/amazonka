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
-- Module      : Amazonka.DeviceFarm.UpdateVPCEConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information about an Amazon Virtual Private Cloud (VPC) endpoint
-- configuration.
module Amazonka.DeviceFarm.UpdateVPCEConfiguration
  ( -- * Creating a Request
    UpdateVPCEConfiguration (..),
    newUpdateVPCEConfiguration,

    -- * Request Lenses
    updateVPCEConfiguration_serviceDnsName,
    updateVPCEConfiguration_vpceConfigurationDescription,
    updateVPCEConfiguration_vpceConfigurationName,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateVPCEConfiguration' smart constructor.
data UpdateVPCEConfiguration = UpdateVPCEConfiguration'
  { -- | The DNS (domain) name used to connect to your private service in your
    -- VPC. The DNS name must not already be in use on the internet.
    serviceDnsName :: Prelude.Maybe Prelude.Text,
    -- | An optional description that provides details about your VPC endpoint
    -- configuration.
    vpceConfigurationDescription :: Prelude.Maybe Prelude.Text,
    -- | The friendly name you give to your VPC endpoint configuration to manage
    -- your configurations more easily.
    vpceConfigurationName :: Prelude.Maybe Prelude.Text,
    -- | The name of the VPC endpoint service running in your AWS account that
    -- you want Device Farm to test.
    vpceServiceName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the VPC endpoint configuration you
    -- want to update.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVPCEConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceDnsName', 'updateVPCEConfiguration_serviceDnsName' - The DNS (domain) name used to connect to your private service in your
-- VPC. The DNS name must not already be in use on the internet.
--
-- 'vpceConfigurationDescription', 'updateVPCEConfiguration_vpceConfigurationDescription' - An optional description that provides details about your VPC endpoint
-- configuration.
--
-- 'vpceConfigurationName', 'updateVPCEConfiguration_vpceConfigurationName' - The friendly name you give to your VPC endpoint configuration to manage
-- your configurations more easily.
--
-- 'vpceServiceName', 'updateVPCEConfiguration_vpceServiceName' - The name of the VPC endpoint service running in your AWS account that
-- you want Device Farm to test.
--
-- 'arn', 'updateVPCEConfiguration_arn' - The Amazon Resource Name (ARN) of the VPC endpoint configuration you
-- want to update.
newUpdateVPCEConfiguration ::
  -- | 'arn'
  Prelude.Text ->
  UpdateVPCEConfiguration
newUpdateVPCEConfiguration pArn_ =
  UpdateVPCEConfiguration'
    { serviceDnsName =
        Prelude.Nothing,
      vpceConfigurationDescription = Prelude.Nothing,
      vpceConfigurationName = Prelude.Nothing,
      vpceServiceName = Prelude.Nothing,
      arn = pArn_
    }

-- | The DNS (domain) name used to connect to your private service in your
-- VPC. The DNS name must not already be in use on the internet.
updateVPCEConfiguration_serviceDnsName :: Lens.Lens' UpdateVPCEConfiguration (Prelude.Maybe Prelude.Text)
updateVPCEConfiguration_serviceDnsName = Lens.lens (\UpdateVPCEConfiguration' {serviceDnsName} -> serviceDnsName) (\s@UpdateVPCEConfiguration' {} a -> s {serviceDnsName = a} :: UpdateVPCEConfiguration)

-- | An optional description that provides details about your VPC endpoint
-- configuration.
updateVPCEConfiguration_vpceConfigurationDescription :: Lens.Lens' UpdateVPCEConfiguration (Prelude.Maybe Prelude.Text)
updateVPCEConfiguration_vpceConfigurationDescription = Lens.lens (\UpdateVPCEConfiguration' {vpceConfigurationDescription} -> vpceConfigurationDescription) (\s@UpdateVPCEConfiguration' {} a -> s {vpceConfigurationDescription = a} :: UpdateVPCEConfiguration)

-- | The friendly name you give to your VPC endpoint configuration to manage
-- your configurations more easily.
updateVPCEConfiguration_vpceConfigurationName :: Lens.Lens' UpdateVPCEConfiguration (Prelude.Maybe Prelude.Text)
updateVPCEConfiguration_vpceConfigurationName = Lens.lens (\UpdateVPCEConfiguration' {vpceConfigurationName} -> vpceConfigurationName) (\s@UpdateVPCEConfiguration' {} a -> s {vpceConfigurationName = a} :: UpdateVPCEConfiguration)

-- | The name of the VPC endpoint service running in your AWS account that
-- you want Device Farm to test.
updateVPCEConfiguration_vpceServiceName :: Lens.Lens' UpdateVPCEConfiguration (Prelude.Maybe Prelude.Text)
updateVPCEConfiguration_vpceServiceName = Lens.lens (\UpdateVPCEConfiguration' {vpceServiceName} -> vpceServiceName) (\s@UpdateVPCEConfiguration' {} a -> s {vpceServiceName = a} :: UpdateVPCEConfiguration)

-- | The Amazon Resource Name (ARN) of the VPC endpoint configuration you
-- want to update.
updateVPCEConfiguration_arn :: Lens.Lens' UpdateVPCEConfiguration Prelude.Text
updateVPCEConfiguration_arn = Lens.lens (\UpdateVPCEConfiguration' {arn} -> arn) (\s@UpdateVPCEConfiguration' {} a -> s {arn = a} :: UpdateVPCEConfiguration)

instance Core.AWSRequest UpdateVPCEConfiguration where
  type
    AWSResponse UpdateVPCEConfiguration =
      UpdateVPCEConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateVPCEConfigurationResponse'
            Prelude.<$> (x Data..?> "vpceConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateVPCEConfiguration where
  hashWithSalt _salt UpdateVPCEConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` serviceDnsName
      `Prelude.hashWithSalt` vpceConfigurationDescription
      `Prelude.hashWithSalt` vpceConfigurationName
      `Prelude.hashWithSalt` vpceServiceName
      `Prelude.hashWithSalt` arn

instance Prelude.NFData UpdateVPCEConfiguration where
  rnf UpdateVPCEConfiguration' {..} =
    Prelude.rnf serviceDnsName `Prelude.seq`
      Prelude.rnf vpceConfigurationDescription `Prelude.seq`
        Prelude.rnf vpceConfigurationName `Prelude.seq`
          Prelude.rnf vpceServiceName `Prelude.seq`
            Prelude.rnf arn

instance Data.ToHeaders UpdateVPCEConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DeviceFarm_20150623.UpdateVPCEConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateVPCEConfiguration where
  toJSON UpdateVPCEConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("serviceDnsName" Data..=)
              Prelude.<$> serviceDnsName,
            ("vpceConfigurationDescription" Data..=)
              Prelude.<$> vpceConfigurationDescription,
            ("vpceConfigurationName" Data..=)
              Prelude.<$> vpceConfigurationName,
            ("vpceServiceName" Data..=)
              Prelude.<$> vpceServiceName,
            Prelude.Just ("arn" Data..= arn)
          ]
      )

instance Data.ToPath UpdateVPCEConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateVPCEConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateVPCEConfigurationResponse' smart constructor.
data UpdateVPCEConfigurationResponse = UpdateVPCEConfigurationResponse'
  { -- | An object that contains information about your VPC endpoint
    -- configuration.
    vpceConfiguration :: Prelude.Maybe VPCEConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateVPCEConfigurationResponse
newUpdateVPCEConfigurationResponse pHttpStatus_ =
  UpdateVPCEConfigurationResponse'
    { vpceConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains information about your VPC endpoint
-- configuration.
updateVPCEConfigurationResponse_vpceConfiguration :: Lens.Lens' UpdateVPCEConfigurationResponse (Prelude.Maybe VPCEConfiguration)
updateVPCEConfigurationResponse_vpceConfiguration = Lens.lens (\UpdateVPCEConfigurationResponse' {vpceConfiguration} -> vpceConfiguration) (\s@UpdateVPCEConfigurationResponse' {} a -> s {vpceConfiguration = a} :: UpdateVPCEConfigurationResponse)

-- | The response's http status code.
updateVPCEConfigurationResponse_httpStatus :: Lens.Lens' UpdateVPCEConfigurationResponse Prelude.Int
updateVPCEConfigurationResponse_httpStatus = Lens.lens (\UpdateVPCEConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateVPCEConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateVPCEConfigurationResponse)

instance
  Prelude.NFData
    UpdateVPCEConfigurationResponse
  where
  rnf UpdateVPCEConfigurationResponse' {..} =
    Prelude.rnf vpceConfiguration `Prelude.seq`
      Prelude.rnf httpStatus
