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
-- Module      : Network.AWS.WorkLink.DescribeCompanyNetworkConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the networking configuration to access the internal websites
-- associated with the specified fleet.
module Network.AWS.WorkLink.DescribeCompanyNetworkConfiguration
  ( -- * Creating a Request
    DescribeCompanyNetworkConfiguration (..),
    newDescribeCompanyNetworkConfiguration,

    -- * Request Lenses
    describeCompanyNetworkConfiguration_fleetArn,

    -- * Destructuring the Response
    DescribeCompanyNetworkConfigurationResponse (..),
    newDescribeCompanyNetworkConfigurationResponse,

    -- * Response Lenses
    describeCompanyNetworkConfigurationResponse_securityGroupIds,
    describeCompanyNetworkConfigurationResponse_subnetIds,
    describeCompanyNetworkConfigurationResponse_vpcId,
    describeCompanyNetworkConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkLink.Types

-- | /See:/ 'newDescribeCompanyNetworkConfiguration' smart constructor.
data DescribeCompanyNetworkConfiguration = DescribeCompanyNetworkConfiguration'
  { -- | The ARN of the fleet.
    fleetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCompanyNetworkConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetArn', 'describeCompanyNetworkConfiguration_fleetArn' - The ARN of the fleet.
newDescribeCompanyNetworkConfiguration ::
  -- | 'fleetArn'
  Prelude.Text ->
  DescribeCompanyNetworkConfiguration
newDescribeCompanyNetworkConfiguration pFleetArn_ =
  DescribeCompanyNetworkConfiguration'
    { fleetArn =
        pFleetArn_
    }

-- | The ARN of the fleet.
describeCompanyNetworkConfiguration_fleetArn :: Lens.Lens' DescribeCompanyNetworkConfiguration Prelude.Text
describeCompanyNetworkConfiguration_fleetArn = Lens.lens (\DescribeCompanyNetworkConfiguration' {fleetArn} -> fleetArn) (\s@DescribeCompanyNetworkConfiguration' {} a -> s {fleetArn = a} :: DescribeCompanyNetworkConfiguration)

instance
  Core.AWSRequest
    DescribeCompanyNetworkConfiguration
  where
  type
    AWSResponse DescribeCompanyNetworkConfiguration =
      DescribeCompanyNetworkConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCompanyNetworkConfigurationResponse'
            Prelude.<$> ( x Core..?> "SecurityGroupIds"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (x Core..?> "SubnetIds" Core..!@ Prelude.mempty)
              Prelude.<*> (x Core..?> "VpcId")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeCompanyNetworkConfiguration

instance
  Prelude.NFData
    DescribeCompanyNetworkConfiguration

instance
  Core.ToHeaders
    DescribeCompanyNetworkConfiguration
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
    DescribeCompanyNetworkConfiguration
  where
  toJSON DescribeCompanyNetworkConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("FleetArn" Core..= fleetArn)]
      )

instance
  Core.ToPath
    DescribeCompanyNetworkConfiguration
  where
  toPath =
    Prelude.const
      "/describeCompanyNetworkConfiguration"

instance
  Core.ToQuery
    DescribeCompanyNetworkConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCompanyNetworkConfigurationResponse' smart constructor.
data DescribeCompanyNetworkConfigurationResponse = DescribeCompanyNetworkConfigurationResponse'
  { -- | The security groups associated with access to the provided subnets.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The subnets used for X-ENI connections from Amazon WorkLink rendering
    -- containers.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The VPC with connectivity to associated websites.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCompanyNetworkConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'describeCompanyNetworkConfigurationResponse_securityGroupIds' - The security groups associated with access to the provided subnets.
--
-- 'subnetIds', 'describeCompanyNetworkConfigurationResponse_subnetIds' - The subnets used for X-ENI connections from Amazon WorkLink rendering
-- containers.
--
-- 'vpcId', 'describeCompanyNetworkConfigurationResponse_vpcId' - The VPC with connectivity to associated websites.
--
-- 'httpStatus', 'describeCompanyNetworkConfigurationResponse_httpStatus' - The response's http status code.
newDescribeCompanyNetworkConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCompanyNetworkConfigurationResponse
newDescribeCompanyNetworkConfigurationResponse
  pHttpStatus_ =
    DescribeCompanyNetworkConfigurationResponse'
      { securityGroupIds =
          Prelude.Nothing,
        subnetIds = Prelude.Nothing,
        vpcId = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The security groups associated with access to the provided subnets.
describeCompanyNetworkConfigurationResponse_securityGroupIds :: Lens.Lens' DescribeCompanyNetworkConfigurationResponse (Prelude.Maybe [Prelude.Text])
describeCompanyNetworkConfigurationResponse_securityGroupIds = Lens.lens (\DescribeCompanyNetworkConfigurationResponse' {securityGroupIds} -> securityGroupIds) (\s@DescribeCompanyNetworkConfigurationResponse' {} a -> s {securityGroupIds = a} :: DescribeCompanyNetworkConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The subnets used for X-ENI connections from Amazon WorkLink rendering
-- containers.
describeCompanyNetworkConfigurationResponse_subnetIds :: Lens.Lens' DescribeCompanyNetworkConfigurationResponse (Prelude.Maybe [Prelude.Text])
describeCompanyNetworkConfigurationResponse_subnetIds = Lens.lens (\DescribeCompanyNetworkConfigurationResponse' {subnetIds} -> subnetIds) (\s@DescribeCompanyNetworkConfigurationResponse' {} a -> s {subnetIds = a} :: DescribeCompanyNetworkConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The VPC with connectivity to associated websites.
describeCompanyNetworkConfigurationResponse_vpcId :: Lens.Lens' DescribeCompanyNetworkConfigurationResponse (Prelude.Maybe Prelude.Text)
describeCompanyNetworkConfigurationResponse_vpcId = Lens.lens (\DescribeCompanyNetworkConfigurationResponse' {vpcId} -> vpcId) (\s@DescribeCompanyNetworkConfigurationResponse' {} a -> s {vpcId = a} :: DescribeCompanyNetworkConfigurationResponse)

-- | The response's http status code.
describeCompanyNetworkConfigurationResponse_httpStatus :: Lens.Lens' DescribeCompanyNetworkConfigurationResponse Prelude.Int
describeCompanyNetworkConfigurationResponse_httpStatus = Lens.lens (\DescribeCompanyNetworkConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeCompanyNetworkConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeCompanyNetworkConfigurationResponse)

instance
  Prelude.NFData
    DescribeCompanyNetworkConfigurationResponse
