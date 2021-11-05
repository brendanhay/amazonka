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
-- Module      : Amazonka.EC2.Types.NatGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.NatGateway where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ConnectivityType
import Amazonka.EC2.Types.NatGatewayAddress
import Amazonka.EC2.Types.NatGatewayState
import Amazonka.EC2.Types.ProvisionedBandwidth
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a NAT gateway.
--
-- /See:/ 'newNatGateway' smart constructor.
data NatGateway = NatGateway'
  { -- | The state of the NAT gateway.
    --
    -- -   @pending@: The NAT gateway is being created and is not ready to
    --     process traffic.
    --
    -- -   @failed@: The NAT gateway could not be created. Check the
    --     @failureCode@ and @failureMessage@ fields for the reason.
    --
    -- -   @available@: The NAT gateway is able to process traffic. This status
    --     remains until you delete the NAT gateway, and does not indicate the
    --     health of the NAT gateway.
    --
    -- -   @deleting@: The NAT gateway is in the process of being terminated
    --     and may still be processing traffic.
    --
    -- -   @deleted@: The NAT gateway has been terminated and is no longer
    --     processing traffic.
    state :: Prelude.Maybe NatGatewayState,
    -- | Indicates whether the NAT gateway supports public or private
    -- connectivity.
    connectivityType :: Prelude.Maybe ConnectivityType,
    -- | If the NAT gateway could not be created, specifies the error code for
    -- the failure. (@InsufficientFreeAddressesInSubnet@ |
    -- @Gateway.NotAttached@ | @InvalidAllocationID.NotFound@ |
    -- @Resource.AlreadyAssociated@ | @InternalError@ |
    -- @InvalidSubnetID.NotFound@)
    failureCode :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC in which the NAT gateway is located.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | If the NAT gateway could not be created, specifies the error message for
    -- the failure, that corresponds to the error code.
    --
    -- -   For InsufficientFreeAddressesInSubnet: \"Subnet has insufficient
    --     free addresses to create this NAT gateway\"
    --
    -- -   For Gateway.NotAttached: \"Network vpc-xxxxxxxx has no Internet
    --     gateway attached\"
    --
    -- -   For InvalidAllocationID.NotFound: \"Elastic IP address
    --     eipalloc-xxxxxxxx could not be associated with this NAT gateway\"
    --
    -- -   For Resource.AlreadyAssociated: \"Elastic IP address
    --     eipalloc-xxxxxxxx is already associated\"
    --
    -- -   For InternalError: \"Network interface eni-xxxxxxxx, created and
    --     used internally by this NAT gateway is in an invalid state. Please
    --     try again.\"
    --
    -- -   For InvalidSubnetID.NotFound: \"The specified subnet subnet-xxxxxxxx
    --     does not exist or could not be found.\"
    failureMessage :: Prelude.Maybe Prelude.Text,
    -- | The ID of the NAT gateway.
    natGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subnet in which the NAT gateway is located.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The date and time the NAT gateway was deleted, if applicable.
    deleteTime :: Prelude.Maybe Core.ISO8601,
    -- | Reserved. If you need to sustain traffic greater than the
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
    -- contact us through the
    -- <https://console.aws.amazon.com/support/home? Support Center>.
    provisionedBandwidth :: Prelude.Maybe ProvisionedBandwidth,
    -- | Information about the IP addresses and network interface associated with
    -- the NAT gateway.
    natGatewayAddresses :: Prelude.Maybe [NatGatewayAddress],
    -- | The date and time the NAT gateway was created.
    createTime :: Prelude.Maybe Core.ISO8601,
    -- | The tags for the NAT gateway.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NatGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'natGateway_state' - The state of the NAT gateway.
--
-- -   @pending@: The NAT gateway is being created and is not ready to
--     process traffic.
--
-- -   @failed@: The NAT gateway could not be created. Check the
--     @failureCode@ and @failureMessage@ fields for the reason.
--
-- -   @available@: The NAT gateway is able to process traffic. This status
--     remains until you delete the NAT gateway, and does not indicate the
--     health of the NAT gateway.
--
-- -   @deleting@: The NAT gateway is in the process of being terminated
--     and may still be processing traffic.
--
-- -   @deleted@: The NAT gateway has been terminated and is no longer
--     processing traffic.
--
-- 'connectivityType', 'natGateway_connectivityType' - Indicates whether the NAT gateway supports public or private
-- connectivity.
--
-- 'failureCode', 'natGateway_failureCode' - If the NAT gateway could not be created, specifies the error code for
-- the failure. (@InsufficientFreeAddressesInSubnet@ |
-- @Gateway.NotAttached@ | @InvalidAllocationID.NotFound@ |
-- @Resource.AlreadyAssociated@ | @InternalError@ |
-- @InvalidSubnetID.NotFound@)
--
-- 'vpcId', 'natGateway_vpcId' - The ID of the VPC in which the NAT gateway is located.
--
-- 'failureMessage', 'natGateway_failureMessage' - If the NAT gateway could not be created, specifies the error message for
-- the failure, that corresponds to the error code.
--
-- -   For InsufficientFreeAddressesInSubnet: \"Subnet has insufficient
--     free addresses to create this NAT gateway\"
--
-- -   For Gateway.NotAttached: \"Network vpc-xxxxxxxx has no Internet
--     gateway attached\"
--
-- -   For InvalidAllocationID.NotFound: \"Elastic IP address
--     eipalloc-xxxxxxxx could not be associated with this NAT gateway\"
--
-- -   For Resource.AlreadyAssociated: \"Elastic IP address
--     eipalloc-xxxxxxxx is already associated\"
--
-- -   For InternalError: \"Network interface eni-xxxxxxxx, created and
--     used internally by this NAT gateway is in an invalid state. Please
--     try again.\"
--
-- -   For InvalidSubnetID.NotFound: \"The specified subnet subnet-xxxxxxxx
--     does not exist or could not be found.\"
--
-- 'natGatewayId', 'natGateway_natGatewayId' - The ID of the NAT gateway.
--
-- 'subnetId', 'natGateway_subnetId' - The ID of the subnet in which the NAT gateway is located.
--
-- 'deleteTime', 'natGateway_deleteTime' - The date and time the NAT gateway was deleted, if applicable.
--
-- 'provisionedBandwidth', 'natGateway_provisionedBandwidth' - Reserved. If you need to sustain traffic greater than the
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
-- contact us through the
-- <https://console.aws.amazon.com/support/home? Support Center>.
--
-- 'natGatewayAddresses', 'natGateway_natGatewayAddresses' - Information about the IP addresses and network interface associated with
-- the NAT gateway.
--
-- 'createTime', 'natGateway_createTime' - The date and time the NAT gateway was created.
--
-- 'tags', 'natGateway_tags' - The tags for the NAT gateway.
newNatGateway ::
  NatGateway
newNatGateway =
  NatGateway'
    { state = Prelude.Nothing,
      connectivityType = Prelude.Nothing,
      failureCode = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      failureMessage = Prelude.Nothing,
      natGatewayId = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      deleteTime = Prelude.Nothing,
      provisionedBandwidth = Prelude.Nothing,
      natGatewayAddresses = Prelude.Nothing,
      createTime = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The state of the NAT gateway.
--
-- -   @pending@: The NAT gateway is being created and is not ready to
--     process traffic.
--
-- -   @failed@: The NAT gateway could not be created. Check the
--     @failureCode@ and @failureMessage@ fields for the reason.
--
-- -   @available@: The NAT gateway is able to process traffic. This status
--     remains until you delete the NAT gateway, and does not indicate the
--     health of the NAT gateway.
--
-- -   @deleting@: The NAT gateway is in the process of being terminated
--     and may still be processing traffic.
--
-- -   @deleted@: The NAT gateway has been terminated and is no longer
--     processing traffic.
natGateway_state :: Lens.Lens' NatGateway (Prelude.Maybe NatGatewayState)
natGateway_state = Lens.lens (\NatGateway' {state} -> state) (\s@NatGateway' {} a -> s {state = a} :: NatGateway)

-- | Indicates whether the NAT gateway supports public or private
-- connectivity.
natGateway_connectivityType :: Lens.Lens' NatGateway (Prelude.Maybe ConnectivityType)
natGateway_connectivityType = Lens.lens (\NatGateway' {connectivityType} -> connectivityType) (\s@NatGateway' {} a -> s {connectivityType = a} :: NatGateway)

-- | If the NAT gateway could not be created, specifies the error code for
-- the failure. (@InsufficientFreeAddressesInSubnet@ |
-- @Gateway.NotAttached@ | @InvalidAllocationID.NotFound@ |
-- @Resource.AlreadyAssociated@ | @InternalError@ |
-- @InvalidSubnetID.NotFound@)
natGateway_failureCode :: Lens.Lens' NatGateway (Prelude.Maybe Prelude.Text)
natGateway_failureCode = Lens.lens (\NatGateway' {failureCode} -> failureCode) (\s@NatGateway' {} a -> s {failureCode = a} :: NatGateway)

-- | The ID of the VPC in which the NAT gateway is located.
natGateway_vpcId :: Lens.Lens' NatGateway (Prelude.Maybe Prelude.Text)
natGateway_vpcId = Lens.lens (\NatGateway' {vpcId} -> vpcId) (\s@NatGateway' {} a -> s {vpcId = a} :: NatGateway)

-- | If the NAT gateway could not be created, specifies the error message for
-- the failure, that corresponds to the error code.
--
-- -   For InsufficientFreeAddressesInSubnet: \"Subnet has insufficient
--     free addresses to create this NAT gateway\"
--
-- -   For Gateway.NotAttached: \"Network vpc-xxxxxxxx has no Internet
--     gateway attached\"
--
-- -   For InvalidAllocationID.NotFound: \"Elastic IP address
--     eipalloc-xxxxxxxx could not be associated with this NAT gateway\"
--
-- -   For Resource.AlreadyAssociated: \"Elastic IP address
--     eipalloc-xxxxxxxx is already associated\"
--
-- -   For InternalError: \"Network interface eni-xxxxxxxx, created and
--     used internally by this NAT gateway is in an invalid state. Please
--     try again.\"
--
-- -   For InvalidSubnetID.NotFound: \"The specified subnet subnet-xxxxxxxx
--     does not exist or could not be found.\"
natGateway_failureMessage :: Lens.Lens' NatGateway (Prelude.Maybe Prelude.Text)
natGateway_failureMessage = Lens.lens (\NatGateway' {failureMessage} -> failureMessage) (\s@NatGateway' {} a -> s {failureMessage = a} :: NatGateway)

-- | The ID of the NAT gateway.
natGateway_natGatewayId :: Lens.Lens' NatGateway (Prelude.Maybe Prelude.Text)
natGateway_natGatewayId = Lens.lens (\NatGateway' {natGatewayId} -> natGatewayId) (\s@NatGateway' {} a -> s {natGatewayId = a} :: NatGateway)

-- | The ID of the subnet in which the NAT gateway is located.
natGateway_subnetId :: Lens.Lens' NatGateway (Prelude.Maybe Prelude.Text)
natGateway_subnetId = Lens.lens (\NatGateway' {subnetId} -> subnetId) (\s@NatGateway' {} a -> s {subnetId = a} :: NatGateway)

-- | The date and time the NAT gateway was deleted, if applicable.
natGateway_deleteTime :: Lens.Lens' NatGateway (Prelude.Maybe Prelude.UTCTime)
natGateway_deleteTime = Lens.lens (\NatGateway' {deleteTime} -> deleteTime) (\s@NatGateway' {} a -> s {deleteTime = a} :: NatGateway) Prelude.. Lens.mapping Core._Time

-- | Reserved. If you need to sustain traffic greater than the
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
-- contact us through the
-- <https://console.aws.amazon.com/support/home? Support Center>.
natGateway_provisionedBandwidth :: Lens.Lens' NatGateway (Prelude.Maybe ProvisionedBandwidth)
natGateway_provisionedBandwidth = Lens.lens (\NatGateway' {provisionedBandwidth} -> provisionedBandwidth) (\s@NatGateway' {} a -> s {provisionedBandwidth = a} :: NatGateway)

-- | Information about the IP addresses and network interface associated with
-- the NAT gateway.
natGateway_natGatewayAddresses :: Lens.Lens' NatGateway (Prelude.Maybe [NatGatewayAddress])
natGateway_natGatewayAddresses = Lens.lens (\NatGateway' {natGatewayAddresses} -> natGatewayAddresses) (\s@NatGateway' {} a -> s {natGatewayAddresses = a} :: NatGateway) Prelude.. Lens.mapping Lens.coerced

-- | The date and time the NAT gateway was created.
natGateway_createTime :: Lens.Lens' NatGateway (Prelude.Maybe Prelude.UTCTime)
natGateway_createTime = Lens.lens (\NatGateway' {createTime} -> createTime) (\s@NatGateway' {} a -> s {createTime = a} :: NatGateway) Prelude.. Lens.mapping Core._Time

-- | The tags for the NAT gateway.
natGateway_tags :: Lens.Lens' NatGateway (Prelude.Maybe [Tag])
natGateway_tags = Lens.lens (\NatGateway' {tags} -> tags) (\s@NatGateway' {} a -> s {tags = a} :: NatGateway) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML NatGateway where
  parseXML x =
    NatGateway'
      Prelude.<$> (x Core..@? "state")
      Prelude.<*> (x Core..@? "connectivityType")
      Prelude.<*> (x Core..@? "failureCode")
      Prelude.<*> (x Core..@? "vpcId")
      Prelude.<*> (x Core..@? "failureMessage")
      Prelude.<*> (x Core..@? "natGatewayId")
      Prelude.<*> (x Core..@? "subnetId")
      Prelude.<*> (x Core..@? "deleteTime")
      Prelude.<*> (x Core..@? "provisionedBandwidth")
      Prelude.<*> ( x Core..@? "natGatewayAddressSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "createTime")
      Prelude.<*> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )

instance Prelude.Hashable NatGateway

instance Prelude.NFData NatGateway
