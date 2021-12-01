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
-- Module      : Amazonka.WorkLink.UpdateCompanyNetworkConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the company network configuration for the fleet.
module Amazonka.WorkLink.UpdateCompanyNetworkConfiguration
  ( -- * Creating a Request
    UpdateCompanyNetworkConfiguration (..),
    newUpdateCompanyNetworkConfiguration,

    -- * Request Lenses
    updateCompanyNetworkConfiguration_fleetArn,
    updateCompanyNetworkConfiguration_vpcId,
    updateCompanyNetworkConfiguration_subnetIds,
    updateCompanyNetworkConfiguration_securityGroupIds,

    -- * Destructuring the Response
    UpdateCompanyNetworkConfigurationResponse (..),
    newUpdateCompanyNetworkConfigurationResponse,

    -- * Response Lenses
    updateCompanyNetworkConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkLink.Types

-- | /See:/ 'newUpdateCompanyNetworkConfiguration' smart constructor.
data UpdateCompanyNetworkConfiguration = UpdateCompanyNetworkConfiguration'
  { -- | The ARN of the fleet.
    fleetArn :: Prelude.Text,
    -- | The VPC with connectivity to associated websites.
    vpcId :: Prelude.Text,
    -- | The subnets used for X-ENI connections from Amazon WorkLink rendering
    -- containers.
    subnetIds :: [Prelude.Text],
    -- | The security groups associated with access to the provided subnets.
    securityGroupIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCompanyNetworkConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetArn', 'updateCompanyNetworkConfiguration_fleetArn' - The ARN of the fleet.
--
-- 'vpcId', 'updateCompanyNetworkConfiguration_vpcId' - The VPC with connectivity to associated websites.
--
-- 'subnetIds', 'updateCompanyNetworkConfiguration_subnetIds' - The subnets used for X-ENI connections from Amazon WorkLink rendering
-- containers.
--
-- 'securityGroupIds', 'updateCompanyNetworkConfiguration_securityGroupIds' - The security groups associated with access to the provided subnets.
newUpdateCompanyNetworkConfiguration ::
  -- | 'fleetArn'
  Prelude.Text ->
  -- | 'vpcId'
  Prelude.Text ->
  UpdateCompanyNetworkConfiguration
newUpdateCompanyNetworkConfiguration
  pFleetArn_
  pVpcId_ =
    UpdateCompanyNetworkConfiguration'
      { fleetArn =
          pFleetArn_,
        vpcId = pVpcId_,
        subnetIds = Prelude.mempty,
        securityGroupIds = Prelude.mempty
      }

-- | The ARN of the fleet.
updateCompanyNetworkConfiguration_fleetArn :: Lens.Lens' UpdateCompanyNetworkConfiguration Prelude.Text
updateCompanyNetworkConfiguration_fleetArn = Lens.lens (\UpdateCompanyNetworkConfiguration' {fleetArn} -> fleetArn) (\s@UpdateCompanyNetworkConfiguration' {} a -> s {fleetArn = a} :: UpdateCompanyNetworkConfiguration)

-- | The VPC with connectivity to associated websites.
updateCompanyNetworkConfiguration_vpcId :: Lens.Lens' UpdateCompanyNetworkConfiguration Prelude.Text
updateCompanyNetworkConfiguration_vpcId = Lens.lens (\UpdateCompanyNetworkConfiguration' {vpcId} -> vpcId) (\s@UpdateCompanyNetworkConfiguration' {} a -> s {vpcId = a} :: UpdateCompanyNetworkConfiguration)

-- | The subnets used for X-ENI connections from Amazon WorkLink rendering
-- containers.
updateCompanyNetworkConfiguration_subnetIds :: Lens.Lens' UpdateCompanyNetworkConfiguration [Prelude.Text]
updateCompanyNetworkConfiguration_subnetIds = Lens.lens (\UpdateCompanyNetworkConfiguration' {subnetIds} -> subnetIds) (\s@UpdateCompanyNetworkConfiguration' {} a -> s {subnetIds = a} :: UpdateCompanyNetworkConfiguration) Prelude.. Lens.coerced

-- | The security groups associated with access to the provided subnets.
updateCompanyNetworkConfiguration_securityGroupIds :: Lens.Lens' UpdateCompanyNetworkConfiguration [Prelude.Text]
updateCompanyNetworkConfiguration_securityGroupIds = Lens.lens (\UpdateCompanyNetworkConfiguration' {securityGroupIds} -> securityGroupIds) (\s@UpdateCompanyNetworkConfiguration' {} a -> s {securityGroupIds = a} :: UpdateCompanyNetworkConfiguration) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    UpdateCompanyNetworkConfiguration
  where
  type
    AWSResponse UpdateCompanyNetworkConfiguration =
      UpdateCompanyNetworkConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateCompanyNetworkConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateCompanyNetworkConfiguration
  where
  hashWithSalt
    salt'
    UpdateCompanyNetworkConfiguration' {..} =
      salt' `Prelude.hashWithSalt` securityGroupIds
        `Prelude.hashWithSalt` subnetIds
        `Prelude.hashWithSalt` vpcId
        `Prelude.hashWithSalt` fleetArn

instance
  Prelude.NFData
    UpdateCompanyNetworkConfiguration
  where
  rnf UpdateCompanyNetworkConfiguration' {..} =
    Prelude.rnf fleetArn
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf vpcId

instance
  Core.ToHeaders
    UpdateCompanyNetworkConfiguration
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
    UpdateCompanyNetworkConfiguration
  where
  toJSON UpdateCompanyNetworkConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FleetArn" Core..= fleetArn),
            Prelude.Just ("VpcId" Core..= vpcId),
            Prelude.Just ("SubnetIds" Core..= subnetIds),
            Prelude.Just
              ("SecurityGroupIds" Core..= securityGroupIds)
          ]
      )

instance
  Core.ToPath
    UpdateCompanyNetworkConfiguration
  where
  toPath =
    Prelude.const "/updateCompanyNetworkConfiguration"

instance
  Core.ToQuery
    UpdateCompanyNetworkConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCompanyNetworkConfigurationResponse' smart constructor.
data UpdateCompanyNetworkConfigurationResponse = UpdateCompanyNetworkConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCompanyNetworkConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateCompanyNetworkConfigurationResponse_httpStatus' - The response's http status code.
newUpdateCompanyNetworkConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateCompanyNetworkConfigurationResponse
newUpdateCompanyNetworkConfigurationResponse
  pHttpStatus_ =
    UpdateCompanyNetworkConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateCompanyNetworkConfigurationResponse_httpStatus :: Lens.Lens' UpdateCompanyNetworkConfigurationResponse Prelude.Int
updateCompanyNetworkConfigurationResponse_httpStatus = Lens.lens (\UpdateCompanyNetworkConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateCompanyNetworkConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateCompanyNetworkConfigurationResponse)

instance
  Prelude.NFData
    UpdateCompanyNetworkConfigurationResponse
  where
  rnf UpdateCompanyNetworkConfigurationResponse' {..} =
    Prelude.rnf httpStatus
