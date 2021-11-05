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
-- Module      : Amazonka.WorkLink.DescribeDevicePolicyConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the device policy configuration for the specified fleet.
module Amazonka.WorkLink.DescribeDevicePolicyConfiguration
  ( -- * Creating a Request
    DescribeDevicePolicyConfiguration (..),
    newDescribeDevicePolicyConfiguration,

    -- * Request Lenses
    describeDevicePolicyConfiguration_fleetArn,

    -- * Destructuring the Response
    DescribeDevicePolicyConfigurationResponse (..),
    newDescribeDevicePolicyConfigurationResponse,

    -- * Response Lenses
    describeDevicePolicyConfigurationResponse_deviceCaCertificate,
    describeDevicePolicyConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkLink.Types

-- | /See:/ 'newDescribeDevicePolicyConfiguration' smart constructor.
data DescribeDevicePolicyConfiguration = DescribeDevicePolicyConfiguration'
  { -- | The ARN of the fleet.
    fleetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDevicePolicyConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetArn', 'describeDevicePolicyConfiguration_fleetArn' - The ARN of the fleet.
newDescribeDevicePolicyConfiguration ::
  -- | 'fleetArn'
  Prelude.Text ->
  DescribeDevicePolicyConfiguration
newDescribeDevicePolicyConfiguration pFleetArn_ =
  DescribeDevicePolicyConfiguration'
    { fleetArn =
        pFleetArn_
    }

-- | The ARN of the fleet.
describeDevicePolicyConfiguration_fleetArn :: Lens.Lens' DescribeDevicePolicyConfiguration Prelude.Text
describeDevicePolicyConfiguration_fleetArn = Lens.lens (\DescribeDevicePolicyConfiguration' {fleetArn} -> fleetArn) (\s@DescribeDevicePolicyConfiguration' {} a -> s {fleetArn = a} :: DescribeDevicePolicyConfiguration)

instance
  Core.AWSRequest
    DescribeDevicePolicyConfiguration
  where
  type
    AWSResponse DescribeDevicePolicyConfiguration =
      DescribeDevicePolicyConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDevicePolicyConfigurationResponse'
            Prelude.<$> (x Core..?> "DeviceCaCertificate")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeDevicePolicyConfiguration

instance
  Prelude.NFData
    DescribeDevicePolicyConfiguration

instance
  Core.ToHeaders
    DescribeDevicePolicyConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    DescribeDevicePolicyConfiguration
  where
  toJSON DescribeDevicePolicyConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("FleetArn" Core..= fleetArn)]
      )

instance
  Core.ToPath
    DescribeDevicePolicyConfiguration
  where
  toPath =
    Prelude.const "/describeDevicePolicyConfiguration"

instance
  Core.ToQuery
    DescribeDevicePolicyConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDevicePolicyConfigurationResponse' smart constructor.
data DescribeDevicePolicyConfigurationResponse = DescribeDevicePolicyConfigurationResponse'
  { -- | The certificate chain, including intermediate certificates and the root
    -- certificate authority certificate used to issue device certificates.
    deviceCaCertificate :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDevicePolicyConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceCaCertificate', 'describeDevicePolicyConfigurationResponse_deviceCaCertificate' - The certificate chain, including intermediate certificates and the root
-- certificate authority certificate used to issue device certificates.
--
-- 'httpStatus', 'describeDevicePolicyConfigurationResponse_httpStatus' - The response's http status code.
newDescribeDevicePolicyConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDevicePolicyConfigurationResponse
newDescribeDevicePolicyConfigurationResponse
  pHttpStatus_ =
    DescribeDevicePolicyConfigurationResponse'
      { deviceCaCertificate =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The certificate chain, including intermediate certificates and the root
-- certificate authority certificate used to issue device certificates.
describeDevicePolicyConfigurationResponse_deviceCaCertificate :: Lens.Lens' DescribeDevicePolicyConfigurationResponse (Prelude.Maybe Prelude.Text)
describeDevicePolicyConfigurationResponse_deviceCaCertificate = Lens.lens (\DescribeDevicePolicyConfigurationResponse' {deviceCaCertificate} -> deviceCaCertificate) (\s@DescribeDevicePolicyConfigurationResponse' {} a -> s {deviceCaCertificate = a} :: DescribeDevicePolicyConfigurationResponse)

-- | The response's http status code.
describeDevicePolicyConfigurationResponse_httpStatus :: Lens.Lens' DescribeDevicePolicyConfigurationResponse Prelude.Int
describeDevicePolicyConfigurationResponse_httpStatus = Lens.lens (\DescribeDevicePolicyConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeDevicePolicyConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeDevicePolicyConfigurationResponse)

instance
  Prelude.NFData
    DescribeDevicePolicyConfigurationResponse
