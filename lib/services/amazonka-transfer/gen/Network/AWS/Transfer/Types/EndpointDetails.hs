{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transfer.Types.EndpointDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transfer.Types.EndpointDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The virtual private cloud (VPC) endpoint settings that are configured
-- for your file transfer protocol-enabled server. With a VPC endpoint, you
-- can restrict access to your server and resources only within your VPC.
-- To control incoming internet traffic, invoke the @UpdateServer@ API and
-- attach an Elastic IP address to your server\'s endpoint.
--
-- After May 19, 2021, you won\'t be able to create a server using
-- @EndpointType=VPC_ENDPOINT@ in your Amazon Web Servicesaccount if your
-- account hasn\'t already done so before May 19, 2021. If you have already
-- created servers with @EndpointType=VPC_ENDPOINT@ in your Amazon Web
-- Servicesaccount on or before May 19, 2021, you will not be affected.
-- After this date, use @EndpointType@=@VPC@.
--
-- For more information, see
-- https:\/\/docs.aws.amazon.com\/transfer\/latest\/userguide\/create-server-in-vpc.html#deprecate-vpc-endpoint.
--
-- /See:/ 'newEndpointDetails' smart constructor.
data EndpointDetails = EndpointDetails'
  { -- | A list of security groups IDs that are available to attach to your
    -- server\'s endpoint.
    --
    -- This property can only be set when @EndpointType@ is set to @VPC@.
    --
    -- You can edit the @SecurityGroupIds@ property in the
    -- <https://docs.aws.amazon.com/transfer/latest/userguide/API_UpdateServer.html UpdateServer>
    -- API only if you are changing the @EndpointType@ from @PUBLIC@ or
    -- @VPC_ENDPOINT@ to @VPC@. To change security groups associated with your
    -- server\'s VPC endpoint after creation, use the Amazon EC2
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyVpcEndpoint.html ModifyVpcEndpoint>
    -- API.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | A list of subnet IDs that are required to host your server endpoint in
    -- your VPC.
    --
    -- This property can only be set when @EndpointType@ is set to @VPC@.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The VPC ID of the VPC in which a server\'s endpoint will be hosted.
    --
    -- This property can only be set when @EndpointType@ is set to @VPC@.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | A list of address allocation IDs that are required to attach an Elastic
    -- IP address to your server\'s endpoint.
    --
    -- This property can only be set when @EndpointType@ is set to @VPC@ and it
    -- is only valid in the @UpdateServer@ API.
    addressAllocationIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the VPC endpoint.
    --
    -- This property can only be set when @EndpointType@ is set to
    -- @VPC_ENDPOINT@.
    --
    -- For more information, see
    -- https:\/\/docs.aws.amazon.com\/transfer\/latest\/userguide\/create-server-in-vpc.html#deprecate-vpc-endpoint.
    vpcEndpointId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EndpointDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'endpointDetails_securityGroupIds' - A list of security groups IDs that are available to attach to your
-- server\'s endpoint.
--
-- This property can only be set when @EndpointType@ is set to @VPC@.
--
-- You can edit the @SecurityGroupIds@ property in the
-- <https://docs.aws.amazon.com/transfer/latest/userguide/API_UpdateServer.html UpdateServer>
-- API only if you are changing the @EndpointType@ from @PUBLIC@ or
-- @VPC_ENDPOINT@ to @VPC@. To change security groups associated with your
-- server\'s VPC endpoint after creation, use the Amazon EC2
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyVpcEndpoint.html ModifyVpcEndpoint>
-- API.
--
-- 'subnetIds', 'endpointDetails_subnetIds' - A list of subnet IDs that are required to host your server endpoint in
-- your VPC.
--
-- This property can only be set when @EndpointType@ is set to @VPC@.
--
-- 'vpcId', 'endpointDetails_vpcId' - The VPC ID of the VPC in which a server\'s endpoint will be hosted.
--
-- This property can only be set when @EndpointType@ is set to @VPC@.
--
-- 'addressAllocationIds', 'endpointDetails_addressAllocationIds' - A list of address allocation IDs that are required to attach an Elastic
-- IP address to your server\'s endpoint.
--
-- This property can only be set when @EndpointType@ is set to @VPC@ and it
-- is only valid in the @UpdateServer@ API.
--
-- 'vpcEndpointId', 'endpointDetails_vpcEndpointId' - The ID of the VPC endpoint.
--
-- This property can only be set when @EndpointType@ is set to
-- @VPC_ENDPOINT@.
--
-- For more information, see
-- https:\/\/docs.aws.amazon.com\/transfer\/latest\/userguide\/create-server-in-vpc.html#deprecate-vpc-endpoint.
newEndpointDetails ::
  EndpointDetails
newEndpointDetails =
  EndpointDetails'
    { securityGroupIds =
        Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      addressAllocationIds = Prelude.Nothing,
      vpcEndpointId = Prelude.Nothing
    }

-- | A list of security groups IDs that are available to attach to your
-- server\'s endpoint.
--
-- This property can only be set when @EndpointType@ is set to @VPC@.
--
-- You can edit the @SecurityGroupIds@ property in the
-- <https://docs.aws.amazon.com/transfer/latest/userguide/API_UpdateServer.html UpdateServer>
-- API only if you are changing the @EndpointType@ from @PUBLIC@ or
-- @VPC_ENDPOINT@ to @VPC@. To change security groups associated with your
-- server\'s VPC endpoint after creation, use the Amazon EC2
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyVpcEndpoint.html ModifyVpcEndpoint>
-- API.
endpointDetails_securityGroupIds :: Lens.Lens' EndpointDetails (Prelude.Maybe [Prelude.Text])
endpointDetails_securityGroupIds = Lens.lens (\EndpointDetails' {securityGroupIds} -> securityGroupIds) (\s@EndpointDetails' {} a -> s {securityGroupIds = a} :: EndpointDetails) Prelude.. Lens.mapping Lens.coerced

-- | A list of subnet IDs that are required to host your server endpoint in
-- your VPC.
--
-- This property can only be set when @EndpointType@ is set to @VPC@.
endpointDetails_subnetIds :: Lens.Lens' EndpointDetails (Prelude.Maybe [Prelude.Text])
endpointDetails_subnetIds = Lens.lens (\EndpointDetails' {subnetIds} -> subnetIds) (\s@EndpointDetails' {} a -> s {subnetIds = a} :: EndpointDetails) Prelude.. Lens.mapping Lens.coerced

-- | The VPC ID of the VPC in which a server\'s endpoint will be hosted.
--
-- This property can only be set when @EndpointType@ is set to @VPC@.
endpointDetails_vpcId :: Lens.Lens' EndpointDetails (Prelude.Maybe Prelude.Text)
endpointDetails_vpcId = Lens.lens (\EndpointDetails' {vpcId} -> vpcId) (\s@EndpointDetails' {} a -> s {vpcId = a} :: EndpointDetails)

-- | A list of address allocation IDs that are required to attach an Elastic
-- IP address to your server\'s endpoint.
--
-- This property can only be set when @EndpointType@ is set to @VPC@ and it
-- is only valid in the @UpdateServer@ API.
endpointDetails_addressAllocationIds :: Lens.Lens' EndpointDetails (Prelude.Maybe [Prelude.Text])
endpointDetails_addressAllocationIds = Lens.lens (\EndpointDetails' {addressAllocationIds} -> addressAllocationIds) (\s@EndpointDetails' {} a -> s {addressAllocationIds = a} :: EndpointDetails) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the VPC endpoint.
--
-- This property can only be set when @EndpointType@ is set to
-- @VPC_ENDPOINT@.
--
-- For more information, see
-- https:\/\/docs.aws.amazon.com\/transfer\/latest\/userguide\/create-server-in-vpc.html#deprecate-vpc-endpoint.
endpointDetails_vpcEndpointId :: Lens.Lens' EndpointDetails (Prelude.Maybe Prelude.Text)
endpointDetails_vpcEndpointId = Lens.lens (\EndpointDetails' {vpcEndpointId} -> vpcEndpointId) (\s@EndpointDetails' {} a -> s {vpcEndpointId = a} :: EndpointDetails)

instance Core.FromJSON EndpointDetails where
  parseJSON =
    Core.withObject
      "EndpointDetails"
      ( \x ->
          EndpointDetails'
            Prelude.<$> ( x Core..:? "SecurityGroupIds"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "SubnetIds" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "VpcId")
            Prelude.<*> ( x Core..:? "AddressAllocationIds"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "VpcEndpointId")
      )

instance Prelude.Hashable EndpointDetails

instance Prelude.NFData EndpointDetails

instance Core.ToJSON EndpointDetails where
  toJSON EndpointDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SecurityGroupIds" Core..=)
              Prelude.<$> securityGroupIds,
            ("SubnetIds" Core..=) Prelude.<$> subnetIds,
            ("VpcId" Core..=) Prelude.<$> vpcId,
            ("AddressAllocationIds" Core..=)
              Prelude.<$> addressAllocationIds,
            ("VpcEndpointId" Core..=) Prelude.<$> vpcEndpointId
          ]
      )
