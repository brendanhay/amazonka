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
-- Module      : Network.AWS.EC2.Types.NatGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NatGateway where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.NatGatewayAddress
import Network.AWS.EC2.Types.NatGatewayState
import Network.AWS.EC2.Types.ProvisionedBandwidth
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Describes a NAT gateway.
--
-- /See:/ 'newNatGateway' smart constructor.
data NatGateway = NatGateway'
  { -- | Information about the IP addresses and network interface associated with
    -- the NAT gateway.
    natGatewayAddresses :: Core.Maybe [NatGatewayAddress],
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
    failureMessage :: Core.Maybe Core.Text,
    -- | If the NAT gateway could not be created, specifies the error code for
    -- the failure. (@InsufficientFreeAddressesInSubnet@ |
    -- @Gateway.NotAttached@ | @InvalidAllocationID.NotFound@ |
    -- @Resource.AlreadyAssociated@ | @InternalError@ |
    -- @InvalidSubnetID.NotFound@)
    failureCode :: Core.Maybe Core.Text,
    -- | Reserved. If you need to sustain traffic greater than the
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
    -- contact us through the
    -- <https://console.aws.amazon.com/support/home? Support Center>.
    provisionedBandwidth :: Core.Maybe ProvisionedBandwidth,
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
    state :: Core.Maybe NatGatewayState,
    -- | The date and time the NAT gateway was deleted, if applicable.
    deleteTime :: Core.Maybe Core.ISO8601,
    -- | The tags for the NAT gateway.
    tags :: Core.Maybe [Tag],
    -- | The date and time the NAT gateway was created.
    createTime :: Core.Maybe Core.ISO8601,
    -- | The ID of the subnet in which the NAT gateway is located.
    subnetId :: Core.Maybe Core.Text,
    -- | The ID of the NAT gateway.
    natGatewayId :: Core.Maybe Core.Text,
    -- | The ID of the VPC in which the NAT gateway is located.
    vpcId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NatGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'natGatewayAddresses', 'natGateway_natGatewayAddresses' - Information about the IP addresses and network interface associated with
-- the NAT gateway.
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
-- 'failureCode', 'natGateway_failureCode' - If the NAT gateway could not be created, specifies the error code for
-- the failure. (@InsufficientFreeAddressesInSubnet@ |
-- @Gateway.NotAttached@ | @InvalidAllocationID.NotFound@ |
-- @Resource.AlreadyAssociated@ | @InternalError@ |
-- @InvalidSubnetID.NotFound@)
--
-- 'provisionedBandwidth', 'natGateway_provisionedBandwidth' - Reserved. If you need to sustain traffic greater than the
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
-- contact us through the
-- <https://console.aws.amazon.com/support/home? Support Center>.
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
-- 'deleteTime', 'natGateway_deleteTime' - The date and time the NAT gateway was deleted, if applicable.
--
-- 'tags', 'natGateway_tags' - The tags for the NAT gateway.
--
-- 'createTime', 'natGateway_createTime' - The date and time the NAT gateway was created.
--
-- 'subnetId', 'natGateway_subnetId' - The ID of the subnet in which the NAT gateway is located.
--
-- 'natGatewayId', 'natGateway_natGatewayId' - The ID of the NAT gateway.
--
-- 'vpcId', 'natGateway_vpcId' - The ID of the VPC in which the NAT gateway is located.
newNatGateway ::
  NatGateway
newNatGateway =
  NatGateway'
    { natGatewayAddresses = Core.Nothing,
      failureMessage = Core.Nothing,
      failureCode = Core.Nothing,
      provisionedBandwidth = Core.Nothing,
      state = Core.Nothing,
      deleteTime = Core.Nothing,
      tags = Core.Nothing,
      createTime = Core.Nothing,
      subnetId = Core.Nothing,
      natGatewayId = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | Information about the IP addresses and network interface associated with
-- the NAT gateway.
natGateway_natGatewayAddresses :: Lens.Lens' NatGateway (Core.Maybe [NatGatewayAddress])
natGateway_natGatewayAddresses = Lens.lens (\NatGateway' {natGatewayAddresses} -> natGatewayAddresses) (\s@NatGateway' {} a -> s {natGatewayAddresses = a} :: NatGateway) Core.. Lens.mapping Lens._Coerce

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
natGateway_failureMessage :: Lens.Lens' NatGateway (Core.Maybe Core.Text)
natGateway_failureMessage = Lens.lens (\NatGateway' {failureMessage} -> failureMessage) (\s@NatGateway' {} a -> s {failureMessage = a} :: NatGateway)

-- | If the NAT gateway could not be created, specifies the error code for
-- the failure. (@InsufficientFreeAddressesInSubnet@ |
-- @Gateway.NotAttached@ | @InvalidAllocationID.NotFound@ |
-- @Resource.AlreadyAssociated@ | @InternalError@ |
-- @InvalidSubnetID.NotFound@)
natGateway_failureCode :: Lens.Lens' NatGateway (Core.Maybe Core.Text)
natGateway_failureCode = Lens.lens (\NatGateway' {failureCode} -> failureCode) (\s@NatGateway' {} a -> s {failureCode = a} :: NatGateway)

-- | Reserved. If you need to sustain traffic greater than the
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
-- contact us through the
-- <https://console.aws.amazon.com/support/home? Support Center>.
natGateway_provisionedBandwidth :: Lens.Lens' NatGateway (Core.Maybe ProvisionedBandwidth)
natGateway_provisionedBandwidth = Lens.lens (\NatGateway' {provisionedBandwidth} -> provisionedBandwidth) (\s@NatGateway' {} a -> s {provisionedBandwidth = a} :: NatGateway)

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
natGateway_state :: Lens.Lens' NatGateway (Core.Maybe NatGatewayState)
natGateway_state = Lens.lens (\NatGateway' {state} -> state) (\s@NatGateway' {} a -> s {state = a} :: NatGateway)

-- | The date and time the NAT gateway was deleted, if applicable.
natGateway_deleteTime :: Lens.Lens' NatGateway (Core.Maybe Core.UTCTime)
natGateway_deleteTime = Lens.lens (\NatGateway' {deleteTime} -> deleteTime) (\s@NatGateway' {} a -> s {deleteTime = a} :: NatGateway) Core.. Lens.mapping Core._Time

-- | The tags for the NAT gateway.
natGateway_tags :: Lens.Lens' NatGateway (Core.Maybe [Tag])
natGateway_tags = Lens.lens (\NatGateway' {tags} -> tags) (\s@NatGateway' {} a -> s {tags = a} :: NatGateway) Core.. Lens.mapping Lens._Coerce

-- | The date and time the NAT gateway was created.
natGateway_createTime :: Lens.Lens' NatGateway (Core.Maybe Core.UTCTime)
natGateway_createTime = Lens.lens (\NatGateway' {createTime} -> createTime) (\s@NatGateway' {} a -> s {createTime = a} :: NatGateway) Core.. Lens.mapping Core._Time

-- | The ID of the subnet in which the NAT gateway is located.
natGateway_subnetId :: Lens.Lens' NatGateway (Core.Maybe Core.Text)
natGateway_subnetId = Lens.lens (\NatGateway' {subnetId} -> subnetId) (\s@NatGateway' {} a -> s {subnetId = a} :: NatGateway)

-- | The ID of the NAT gateway.
natGateway_natGatewayId :: Lens.Lens' NatGateway (Core.Maybe Core.Text)
natGateway_natGatewayId = Lens.lens (\NatGateway' {natGatewayId} -> natGatewayId) (\s@NatGateway' {} a -> s {natGatewayId = a} :: NatGateway)

-- | The ID of the VPC in which the NAT gateway is located.
natGateway_vpcId :: Lens.Lens' NatGateway (Core.Maybe Core.Text)
natGateway_vpcId = Lens.lens (\NatGateway' {vpcId} -> vpcId) (\s@NatGateway' {} a -> s {vpcId = a} :: NatGateway)

instance Core.FromXML NatGateway where
  parseXML x =
    NatGateway'
      Core.<$> ( x Core..@? "natGatewayAddressSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "failureMessage")
      Core.<*> (x Core..@? "failureCode")
      Core.<*> (x Core..@? "provisionedBandwidth")
      Core.<*> (x Core..@? "state")
      Core.<*> (x Core..@? "deleteTime")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "createTime")
      Core.<*> (x Core..@? "subnetId")
      Core.<*> (x Core..@? "natGatewayId")
      Core.<*> (x Core..@? "vpcId")

instance Core.Hashable NatGateway

instance Core.NFData NatGateway
