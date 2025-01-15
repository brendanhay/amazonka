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
-- Module      : Amazonka.FMS.Types.EC2CreateRouteAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.EC2CreateRouteAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types.ActionTarget
import qualified Amazonka.Prelude as Prelude

-- | Information about the CreateRoute action in Amazon EC2.
--
-- /See:/ 'newEC2CreateRouteAction' smart constructor.
data EC2CreateRouteAction = EC2CreateRouteAction'
  { -- | A description of CreateRoute action in Amazon EC2.
    description :: Prelude.Maybe Prelude.Text,
    -- | Information about the IPv4 CIDR address block used for the destination
    -- match.
    destinationCidrBlock :: Prelude.Maybe Prelude.Text,
    -- | Information about the IPv6 CIDR block destination.
    destinationIpv6CidrBlock :: Prelude.Maybe Prelude.Text,
    -- | Information about the ID of a prefix list used for the destination
    -- match.
    destinationPrefixListId :: Prelude.Maybe Prelude.Text,
    -- | Information about the ID of an internet gateway or virtual private
    -- gateway attached to your VPC.
    gatewayId :: Prelude.Maybe ActionTarget,
    -- | Information about the ID of a VPC endpoint. Supported for Gateway Load
    -- Balancer endpoints only.
    vpcEndpointId :: Prelude.Maybe ActionTarget,
    -- | Information about the ID of the route table for the route.
    routeTableId :: ActionTarget
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EC2CreateRouteAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'eC2CreateRouteAction_description' - A description of CreateRoute action in Amazon EC2.
--
-- 'destinationCidrBlock', 'eC2CreateRouteAction_destinationCidrBlock' - Information about the IPv4 CIDR address block used for the destination
-- match.
--
-- 'destinationIpv6CidrBlock', 'eC2CreateRouteAction_destinationIpv6CidrBlock' - Information about the IPv6 CIDR block destination.
--
-- 'destinationPrefixListId', 'eC2CreateRouteAction_destinationPrefixListId' - Information about the ID of a prefix list used for the destination
-- match.
--
-- 'gatewayId', 'eC2CreateRouteAction_gatewayId' - Information about the ID of an internet gateway or virtual private
-- gateway attached to your VPC.
--
-- 'vpcEndpointId', 'eC2CreateRouteAction_vpcEndpointId' - Information about the ID of a VPC endpoint. Supported for Gateway Load
-- Balancer endpoints only.
--
-- 'routeTableId', 'eC2CreateRouteAction_routeTableId' - Information about the ID of the route table for the route.
newEC2CreateRouteAction ::
  -- | 'routeTableId'
  ActionTarget ->
  EC2CreateRouteAction
newEC2CreateRouteAction pRouteTableId_ =
  EC2CreateRouteAction'
    { description =
        Prelude.Nothing,
      destinationCidrBlock = Prelude.Nothing,
      destinationIpv6CidrBlock = Prelude.Nothing,
      destinationPrefixListId = Prelude.Nothing,
      gatewayId = Prelude.Nothing,
      vpcEndpointId = Prelude.Nothing,
      routeTableId = pRouteTableId_
    }

-- | A description of CreateRoute action in Amazon EC2.
eC2CreateRouteAction_description :: Lens.Lens' EC2CreateRouteAction (Prelude.Maybe Prelude.Text)
eC2CreateRouteAction_description = Lens.lens (\EC2CreateRouteAction' {description} -> description) (\s@EC2CreateRouteAction' {} a -> s {description = a} :: EC2CreateRouteAction)

-- | Information about the IPv4 CIDR address block used for the destination
-- match.
eC2CreateRouteAction_destinationCidrBlock :: Lens.Lens' EC2CreateRouteAction (Prelude.Maybe Prelude.Text)
eC2CreateRouteAction_destinationCidrBlock = Lens.lens (\EC2CreateRouteAction' {destinationCidrBlock} -> destinationCidrBlock) (\s@EC2CreateRouteAction' {} a -> s {destinationCidrBlock = a} :: EC2CreateRouteAction)

-- | Information about the IPv6 CIDR block destination.
eC2CreateRouteAction_destinationIpv6CidrBlock :: Lens.Lens' EC2CreateRouteAction (Prelude.Maybe Prelude.Text)
eC2CreateRouteAction_destinationIpv6CidrBlock = Lens.lens (\EC2CreateRouteAction' {destinationIpv6CidrBlock} -> destinationIpv6CidrBlock) (\s@EC2CreateRouteAction' {} a -> s {destinationIpv6CidrBlock = a} :: EC2CreateRouteAction)

-- | Information about the ID of a prefix list used for the destination
-- match.
eC2CreateRouteAction_destinationPrefixListId :: Lens.Lens' EC2CreateRouteAction (Prelude.Maybe Prelude.Text)
eC2CreateRouteAction_destinationPrefixListId = Lens.lens (\EC2CreateRouteAction' {destinationPrefixListId} -> destinationPrefixListId) (\s@EC2CreateRouteAction' {} a -> s {destinationPrefixListId = a} :: EC2CreateRouteAction)

-- | Information about the ID of an internet gateway or virtual private
-- gateway attached to your VPC.
eC2CreateRouteAction_gatewayId :: Lens.Lens' EC2CreateRouteAction (Prelude.Maybe ActionTarget)
eC2CreateRouteAction_gatewayId = Lens.lens (\EC2CreateRouteAction' {gatewayId} -> gatewayId) (\s@EC2CreateRouteAction' {} a -> s {gatewayId = a} :: EC2CreateRouteAction)

-- | Information about the ID of a VPC endpoint. Supported for Gateway Load
-- Balancer endpoints only.
eC2CreateRouteAction_vpcEndpointId :: Lens.Lens' EC2CreateRouteAction (Prelude.Maybe ActionTarget)
eC2CreateRouteAction_vpcEndpointId = Lens.lens (\EC2CreateRouteAction' {vpcEndpointId} -> vpcEndpointId) (\s@EC2CreateRouteAction' {} a -> s {vpcEndpointId = a} :: EC2CreateRouteAction)

-- | Information about the ID of the route table for the route.
eC2CreateRouteAction_routeTableId :: Lens.Lens' EC2CreateRouteAction ActionTarget
eC2CreateRouteAction_routeTableId = Lens.lens (\EC2CreateRouteAction' {routeTableId} -> routeTableId) (\s@EC2CreateRouteAction' {} a -> s {routeTableId = a} :: EC2CreateRouteAction)

instance Data.FromJSON EC2CreateRouteAction where
  parseJSON =
    Data.withObject
      "EC2CreateRouteAction"
      ( \x ->
          EC2CreateRouteAction'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "DestinationCidrBlock")
            Prelude.<*> (x Data..:? "DestinationIpv6CidrBlock")
            Prelude.<*> (x Data..:? "DestinationPrefixListId")
            Prelude.<*> (x Data..:? "GatewayId")
            Prelude.<*> (x Data..:? "VpcEndpointId")
            Prelude.<*> (x Data..: "RouteTableId")
      )

instance Prelude.Hashable EC2CreateRouteAction where
  hashWithSalt _salt EC2CreateRouteAction' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` destinationCidrBlock
      `Prelude.hashWithSalt` destinationIpv6CidrBlock
      `Prelude.hashWithSalt` destinationPrefixListId
      `Prelude.hashWithSalt` gatewayId
      `Prelude.hashWithSalt` vpcEndpointId
      `Prelude.hashWithSalt` routeTableId

instance Prelude.NFData EC2CreateRouteAction where
  rnf EC2CreateRouteAction' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf destinationCidrBlock `Prelude.seq`
        Prelude.rnf destinationIpv6CidrBlock `Prelude.seq`
          Prelude.rnf destinationPrefixListId `Prelude.seq`
            Prelude.rnf gatewayId `Prelude.seq`
              Prelude.rnf vpcEndpointId `Prelude.seq`
                Prelude.rnf routeTableId
