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
-- Module      : Network.AWS.ELBv2.Types.TargetDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.TargetDescription where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a target.
--
-- /See:/ 'newTargetDescription' smart constructor.
data TargetDescription = TargetDescription'
  { -- | An Availability Zone or @all@. This determines whether the target
    -- receives traffic from the load balancer nodes in the specified
    -- Availability Zone or from all enabled Availability Zones for the load
    -- balancer.
    --
    -- This parameter is not supported if the target type of the target group
    -- is @instance@ or @alb@.
    --
    -- If the target type is @ip@ and the IP address is in a subnet of the VPC
    -- for the target group, the Availability Zone is automatically detected
    -- and this parameter is optional. If the IP address is outside the VPC,
    -- this parameter is required.
    --
    -- With an Application Load Balancer, if the target type is @ip@ and the IP
    -- address is outside the VPC for the target group, the only supported
    -- value is @all@.
    --
    -- If the target type is @lambda@, this parameter is optional and the only
    -- supported value is @all@.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The port on which the target is listening. If the target group protocol
    -- is GENEVE, the supported port is 6081. If the target type is @alb@, the
    -- targeted Application Load Balancer must have at least one listener whose
    -- port matches the target group port. Not used if the target is a Lambda
    -- function.
    port :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the target. If the target type of the target group is
    -- @instance@, specify an instance ID. If the target type is @ip@, specify
    -- an IP address. If the target type is @lambda@, specify the ARN of the
    -- Lambda function. If the target type is @alb@, specify the ARN of the
    -- Application Load Balancer target.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZone', 'targetDescription_availabilityZone' - An Availability Zone or @all@. This determines whether the target
-- receives traffic from the load balancer nodes in the specified
-- Availability Zone or from all enabled Availability Zones for the load
-- balancer.
--
-- This parameter is not supported if the target type of the target group
-- is @instance@ or @alb@.
--
-- If the target type is @ip@ and the IP address is in a subnet of the VPC
-- for the target group, the Availability Zone is automatically detected
-- and this parameter is optional. If the IP address is outside the VPC,
-- this parameter is required.
--
-- With an Application Load Balancer, if the target type is @ip@ and the IP
-- address is outside the VPC for the target group, the only supported
-- value is @all@.
--
-- If the target type is @lambda@, this parameter is optional and the only
-- supported value is @all@.
--
-- 'port', 'targetDescription_port' - The port on which the target is listening. If the target group protocol
-- is GENEVE, the supported port is 6081. If the target type is @alb@, the
-- targeted Application Load Balancer must have at least one listener whose
-- port matches the target group port. Not used if the target is a Lambda
-- function.
--
-- 'id', 'targetDescription_id' - The ID of the target. If the target type of the target group is
-- @instance@, specify an instance ID. If the target type is @ip@, specify
-- an IP address. If the target type is @lambda@, specify the ARN of the
-- Lambda function. If the target type is @alb@, specify the ARN of the
-- Application Load Balancer target.
newTargetDescription ::
  -- | 'id'
  Prelude.Text ->
  TargetDescription
newTargetDescription pId_ =
  TargetDescription'
    { availabilityZone =
        Prelude.Nothing,
      port = Prelude.Nothing,
      id = pId_
    }

-- | An Availability Zone or @all@. This determines whether the target
-- receives traffic from the load balancer nodes in the specified
-- Availability Zone or from all enabled Availability Zones for the load
-- balancer.
--
-- This parameter is not supported if the target type of the target group
-- is @instance@ or @alb@.
--
-- If the target type is @ip@ and the IP address is in a subnet of the VPC
-- for the target group, the Availability Zone is automatically detected
-- and this parameter is optional. If the IP address is outside the VPC,
-- this parameter is required.
--
-- With an Application Load Balancer, if the target type is @ip@ and the IP
-- address is outside the VPC for the target group, the only supported
-- value is @all@.
--
-- If the target type is @lambda@, this parameter is optional and the only
-- supported value is @all@.
targetDescription_availabilityZone :: Lens.Lens' TargetDescription (Prelude.Maybe Prelude.Text)
targetDescription_availabilityZone = Lens.lens (\TargetDescription' {availabilityZone} -> availabilityZone) (\s@TargetDescription' {} a -> s {availabilityZone = a} :: TargetDescription)

-- | The port on which the target is listening. If the target group protocol
-- is GENEVE, the supported port is 6081. If the target type is @alb@, the
-- targeted Application Load Balancer must have at least one listener whose
-- port matches the target group port. Not used if the target is a Lambda
-- function.
targetDescription_port :: Lens.Lens' TargetDescription (Prelude.Maybe Prelude.Natural)
targetDescription_port = Lens.lens (\TargetDescription' {port} -> port) (\s@TargetDescription' {} a -> s {port = a} :: TargetDescription)

-- | The ID of the target. If the target type of the target group is
-- @instance@, specify an instance ID. If the target type is @ip@, specify
-- an IP address. If the target type is @lambda@, specify the ARN of the
-- Lambda function. If the target type is @alb@, specify the ARN of the
-- Application Load Balancer target.
targetDescription_id :: Lens.Lens' TargetDescription Prelude.Text
targetDescription_id = Lens.lens (\TargetDescription' {id} -> id) (\s@TargetDescription' {} a -> s {id = a} :: TargetDescription)

instance Core.FromXML TargetDescription where
  parseXML x =
    TargetDescription'
      Prelude.<$> (x Core..@? "AvailabilityZone")
      Prelude.<*> (x Core..@? "Port")
      Prelude.<*> (x Core..@ "Id")

instance Prelude.Hashable TargetDescription

instance Prelude.NFData TargetDescription

instance Core.ToQuery TargetDescription where
  toQuery TargetDescription' {..} =
    Prelude.mconcat
      [ "AvailabilityZone" Core.=: availabilityZone,
        "Port" Core.=: port,
        "Id" Core.=: id
      ]
