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
-- Module      : Network.AWS.DeviceFarm.GetVPCEConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the configuration settings for your Amazon
-- Virtual Private Cloud (VPC) endpoint.
module Network.AWS.DeviceFarm.GetVPCEConfiguration
  ( -- * Creating a Request
    GetVPCEConfiguration (..),
    newGetVPCEConfiguration,

    -- * Request Lenses
    getVPCEConfiguration_arn,

    -- * Destructuring the Response
    GetVPCEConfigurationResponse (..),
    newGetVPCEConfigurationResponse,

    -- * Response Lenses
    getVPCEConfigurationResponse_vpceConfiguration,
    getVPCEConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetVPCEConfiguration' smart constructor.
data GetVPCEConfiguration = GetVPCEConfiguration'
  { -- | The Amazon Resource Name (ARN) of the VPC endpoint configuration you
    -- want to describe.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVPCEConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getVPCEConfiguration_arn' - The Amazon Resource Name (ARN) of the VPC endpoint configuration you
-- want to describe.
newGetVPCEConfiguration ::
  -- | 'arn'
  Prelude.Text ->
  GetVPCEConfiguration
newGetVPCEConfiguration pArn_ =
  GetVPCEConfiguration' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the VPC endpoint configuration you
-- want to describe.
getVPCEConfiguration_arn :: Lens.Lens' GetVPCEConfiguration Prelude.Text
getVPCEConfiguration_arn = Lens.lens (\GetVPCEConfiguration' {arn} -> arn) (\s@GetVPCEConfiguration' {} a -> s {arn = a} :: GetVPCEConfiguration)

instance Core.AWSRequest GetVPCEConfiguration where
  type
    AWSResponse GetVPCEConfiguration =
      GetVPCEConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVPCEConfigurationResponse'
            Prelude.<$> (x Core..?> "vpceConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetVPCEConfiguration

instance Prelude.NFData GetVPCEConfiguration

instance Core.ToHeaders GetVPCEConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.GetVPCEConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetVPCEConfiguration where
  toJSON GetVPCEConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Core..= arn)]
      )

instance Core.ToPath GetVPCEConfiguration where
  toPath = Prelude.const "/"

instance Core.ToQuery GetVPCEConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetVPCEConfigurationResponse' smart constructor.
data GetVPCEConfigurationResponse = GetVPCEConfigurationResponse'
  { -- | An object that contains information about your VPC endpoint
    -- configuration.
    vpceConfiguration :: Prelude.Maybe VPCEConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVPCEConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpceConfiguration', 'getVPCEConfigurationResponse_vpceConfiguration' - An object that contains information about your VPC endpoint
-- configuration.
--
-- 'httpStatus', 'getVPCEConfigurationResponse_httpStatus' - The response's http status code.
newGetVPCEConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetVPCEConfigurationResponse
newGetVPCEConfigurationResponse pHttpStatus_ =
  GetVPCEConfigurationResponse'
    { vpceConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains information about your VPC endpoint
-- configuration.
getVPCEConfigurationResponse_vpceConfiguration :: Lens.Lens' GetVPCEConfigurationResponse (Prelude.Maybe VPCEConfiguration)
getVPCEConfigurationResponse_vpceConfiguration = Lens.lens (\GetVPCEConfigurationResponse' {vpceConfiguration} -> vpceConfiguration) (\s@GetVPCEConfigurationResponse' {} a -> s {vpceConfiguration = a} :: GetVPCEConfigurationResponse)

-- | The response's http status code.
getVPCEConfigurationResponse_httpStatus :: Lens.Lens' GetVPCEConfigurationResponse Prelude.Int
getVPCEConfigurationResponse_httpStatus = Lens.lens (\GetVPCEConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetVPCEConfigurationResponse' {} a -> s {httpStatus = a} :: GetVPCEConfigurationResponse)

instance Prelude.NFData GetVPCEConfigurationResponse
