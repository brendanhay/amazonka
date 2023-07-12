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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.NatGateway where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ConnectivityType
import Amazonka.EC2.Types.NatGatewayAddress
import Amazonka.EC2.Types.NatGatewayState
import Amazonka.EC2.Types.ProvisionedBandwidth
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a NAT gateway.
--
-- /See:/ 'newNatGateway' smart constructor.
data NatGateway = NatGateway'
  { -- | Indicates whether the NAT gateway supports public or private
    -- connectivity.
    connectivityType :: Prelude.Maybe ConnectivityType,
    -- | The date and time the NAT gateway was created.
    createTime :: Prelude.Maybe Data.ISO8601,
    -- | The date and time the NAT gateway was deleted, if applicable.
    deleteTime :: Prelude.Maybe Data.ISO8601,
    -- | If the NAT gateway could not be created, specifies the error code for
    -- the failure. (@InsufficientFreeAddressesInSubnet@ |
    -- @Gateway.NotAttached@ | @InvalidAllocationID.NotFound@ |
    -- @Resource.AlreadyAssociated@ | @InternalError@ |
    -- @InvalidSubnetID.NotFound@)
    failureCode :: Prelude.Maybe Prelude.Text,
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
    -- | Information about the IP addresses and network interface associated with
    -- the NAT gateway.
    natGatewayAddresses :: Prelude.Maybe [NatGatewayAddress],
    -- | The ID of the NAT gateway.
    natGatewayId :: Prelude.Maybe Prelude.Text,
    -- | Reserved. If you need to sustain traffic greater than the
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
    -- contact us through the
    -- <https://console.aws.amazon.com/support/home? Support Center>.
    provisionedBandwidth :: Prelude.Maybe ProvisionedBandwidth,
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
    state :: Prelude.Maybe NatGatewayState,
    -- | The ID of the subnet in which the NAT gateway is located.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The tags for the NAT gateway.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the VPC in which the NAT gateway is located.
    vpcId :: Prelude.Maybe Prelude.Text
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
-- 'connectivityType', 'natGateway_connectivityType' - Indicates whether the NAT gateway supports public or private
-- connectivity.
--
-- 'createTime', 'natGateway_createTime' - The date and time the NAT gateway was created.
--
-- 'deleteTime', 'natGateway_deleteTime' - The date and time the NAT gateway was deleted, if applicable.
--
-- 'failureCode', 'natGateway_failureCode' - If the NAT gateway could not be created, specifies the error code for
-- the failure. (@InsufficientFreeAddressesInSubnet@ |
-- @Gateway.NotAttached@ | @InvalidAllocationID.NotFound@ |
-- @Resource.AlreadyAssociated@ | @InternalError@ |
-- @InvalidSubnetID.NotFound@)
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
-- 'natGatewayAddresses', 'natGateway_natGatewayAddresses' - Information about the IP addresses and network interface associated with
-- the NAT gateway.
--
-- 'natGatewayId', 'natGateway_natGatewayId' - The ID of the NAT gateway.
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
-- 'subnetId', 'natGateway_subnetId' - The ID of the subnet in which the NAT gateway is located.
--
-- 'tags', 'natGateway_tags' - The tags for the NAT gateway.
--
-- 'vpcId', 'natGateway_vpcId' - The ID of the VPC in which the NAT gateway is located.
newNatGateway ::
  NatGateway
newNatGateway =
  NatGateway'
    { connectivityType = Prelude.Nothing,
      createTime = Prelude.Nothing,
      deleteTime = Prelude.Nothing,
      failureCode = Prelude.Nothing,
      failureMessage = Prelude.Nothing,
      natGatewayAddresses = Prelude.Nothing,
      natGatewayId = Prelude.Nothing,
      provisionedBandwidth = Prelude.Nothing,
      state = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      tags = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | Indicates whether the NAT gateway supports public or private
-- connectivity.
natGateway_connectivityType :: Lens.Lens' NatGateway (Prelude.Maybe ConnectivityType)
natGateway_connectivityType = Lens.lens (\NatGateway' {connectivityType} -> connectivityType) (\s@NatGateway' {} a -> s {connectivityType = a} :: NatGateway)

-- | The date and time the NAT gateway was created.
natGateway_createTime :: Lens.Lens' NatGateway (Prelude.Maybe Prelude.UTCTime)
natGateway_createTime = Lens.lens (\NatGateway' {createTime} -> createTime) (\s@NatGateway' {} a -> s {createTime = a} :: NatGateway) Prelude.. Lens.mapping Data._Time

-- | The date and time the NAT gateway was deleted, if applicable.
natGateway_deleteTime :: Lens.Lens' NatGateway (Prelude.Maybe Prelude.UTCTime)
natGateway_deleteTime = Lens.lens (\NatGateway' {deleteTime} -> deleteTime) (\s@NatGateway' {} a -> s {deleteTime = a} :: NatGateway) Prelude.. Lens.mapping Data._Time

-- | If the NAT gateway could not be created, specifies the error code for
-- the failure. (@InsufficientFreeAddressesInSubnet@ |
-- @Gateway.NotAttached@ | @InvalidAllocationID.NotFound@ |
-- @Resource.AlreadyAssociated@ | @InternalError@ |
-- @InvalidSubnetID.NotFound@)
natGateway_failureCode :: Lens.Lens' NatGateway (Prelude.Maybe Prelude.Text)
natGateway_failureCode = Lens.lens (\NatGateway' {failureCode} -> failureCode) (\s@NatGateway' {} a -> s {failureCode = a} :: NatGateway)

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

-- | Information about the IP addresses and network interface associated with
-- the NAT gateway.
natGateway_natGatewayAddresses :: Lens.Lens' NatGateway (Prelude.Maybe [NatGatewayAddress])
natGateway_natGatewayAddresses = Lens.lens (\NatGateway' {natGatewayAddresses} -> natGatewayAddresses) (\s@NatGateway' {} a -> s {natGatewayAddresses = a} :: NatGateway) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the NAT gateway.
natGateway_natGatewayId :: Lens.Lens' NatGateway (Prelude.Maybe Prelude.Text)
natGateway_natGatewayId = Lens.lens (\NatGateway' {natGatewayId} -> natGatewayId) (\s@NatGateway' {} a -> s {natGatewayId = a} :: NatGateway)

-- | Reserved. If you need to sustain traffic greater than the
-- <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits>,
-- contact us through the
-- <https://console.aws.amazon.com/support/home? Support Center>.
natGateway_provisionedBandwidth :: Lens.Lens' NatGateway (Prelude.Maybe ProvisionedBandwidth)
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
natGateway_state :: Lens.Lens' NatGateway (Prelude.Maybe NatGatewayState)
natGateway_state = Lens.lens (\NatGateway' {state} -> state) (\s@NatGateway' {} a -> s {state = a} :: NatGateway)

-- | The ID of the subnet in which the NAT gateway is located.
natGateway_subnetId :: Lens.Lens' NatGateway (Prelude.Maybe Prelude.Text)
natGateway_subnetId = Lens.lens (\NatGateway' {subnetId} -> subnetId) (\s@NatGateway' {} a -> s {subnetId = a} :: NatGateway)

-- | The tags for the NAT gateway.
natGateway_tags :: Lens.Lens' NatGateway (Prelude.Maybe [Tag])
natGateway_tags = Lens.lens (\NatGateway' {tags} -> tags) (\s@NatGateway' {} a -> s {tags = a} :: NatGateway) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the VPC in which the NAT gateway is located.
natGateway_vpcId :: Lens.Lens' NatGateway (Prelude.Maybe Prelude.Text)
natGateway_vpcId = Lens.lens (\NatGateway' {vpcId} -> vpcId) (\s@NatGateway' {} a -> s {vpcId = a} :: NatGateway)

instance Data.FromXML NatGateway where
  parseXML x =
    NatGateway'
      Prelude.<$> (x Data..@? "connectivityType")
      Prelude.<*> (x Data..@? "createTime")
      Prelude.<*> (x Data..@? "deleteTime")
      Prelude.<*> (x Data..@? "failureCode")
      Prelude.<*> (x Data..@? "failureMessage")
      Prelude.<*> ( x
                      Data..@? "natGatewayAddressSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "natGatewayId")
      Prelude.<*> (x Data..@? "provisionedBandwidth")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> (x Data..@? "subnetId")
      Prelude.<*> ( x
                      Data..@? "tagSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "vpcId")

instance Prelude.Hashable NatGateway where
  hashWithSalt _salt NatGateway' {..} =
    _salt
      `Prelude.hashWithSalt` connectivityType
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` deleteTime
      `Prelude.hashWithSalt` failureCode
      `Prelude.hashWithSalt` failureMessage
      `Prelude.hashWithSalt` natGatewayAddresses
      `Prelude.hashWithSalt` natGatewayId
      `Prelude.hashWithSalt` provisionedBandwidth
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData NatGateway where
  rnf NatGateway' {..} =
    Prelude.rnf connectivityType
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf deleteTime
      `Prelude.seq` Prelude.rnf failureCode
      `Prelude.seq` Prelude.rnf failureMessage
      `Prelude.seq` Prelude.rnf natGatewayAddresses
      `Prelude.seq` Prelude.rnf natGatewayId
      `Prelude.seq` Prelude.rnf provisionedBandwidth
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcId
