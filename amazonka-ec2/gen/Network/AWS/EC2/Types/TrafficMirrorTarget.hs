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
-- Module      : Network.AWS.EC2.Types.TrafficMirrorTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TrafficMirrorTarget where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TrafficMirrorTargetType
import qualified Network.AWS.Lens as Lens

-- | Describes a Traffic Mirror target.
--
-- /See:/ 'newTrafficMirrorTarget' smart constructor.
data TrafficMirrorTarget = TrafficMirrorTarget'
  { -- | The ID of the account that owns the Traffic Mirror target.
    ownerId :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the Network Load Balancer.
    networkLoadBalancerArn :: Core.Maybe Core.Text,
    -- | The tags assigned to the Traffic Mirror target.
    tags :: Core.Maybe [Tag],
    -- | The network interface ID that is attached to the target.
    networkInterfaceId :: Core.Maybe Core.Text,
    -- | Information about the Traffic Mirror target.
    description :: Core.Maybe Core.Text,
    -- | The type of Traffic Mirror target.
    type' :: Core.Maybe TrafficMirrorTargetType,
    -- | The ID of the Traffic Mirror target.
    trafficMirrorTargetId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TrafficMirrorTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'trafficMirrorTarget_ownerId' - The ID of the account that owns the Traffic Mirror target.
--
-- 'networkLoadBalancerArn', 'trafficMirrorTarget_networkLoadBalancerArn' - The Amazon Resource Name (ARN) of the Network Load Balancer.
--
-- 'tags', 'trafficMirrorTarget_tags' - The tags assigned to the Traffic Mirror target.
--
-- 'networkInterfaceId', 'trafficMirrorTarget_networkInterfaceId' - The network interface ID that is attached to the target.
--
-- 'description', 'trafficMirrorTarget_description' - Information about the Traffic Mirror target.
--
-- 'type'', 'trafficMirrorTarget_type' - The type of Traffic Mirror target.
--
-- 'trafficMirrorTargetId', 'trafficMirrorTarget_trafficMirrorTargetId' - The ID of the Traffic Mirror target.
newTrafficMirrorTarget ::
  TrafficMirrorTarget
newTrafficMirrorTarget =
  TrafficMirrorTarget'
    { ownerId = Core.Nothing,
      networkLoadBalancerArn = Core.Nothing,
      tags = Core.Nothing,
      networkInterfaceId = Core.Nothing,
      description = Core.Nothing,
      type' = Core.Nothing,
      trafficMirrorTargetId = Core.Nothing
    }

-- | The ID of the account that owns the Traffic Mirror target.
trafficMirrorTarget_ownerId :: Lens.Lens' TrafficMirrorTarget (Core.Maybe Core.Text)
trafficMirrorTarget_ownerId = Lens.lens (\TrafficMirrorTarget' {ownerId} -> ownerId) (\s@TrafficMirrorTarget' {} a -> s {ownerId = a} :: TrafficMirrorTarget)

-- | The Amazon Resource Name (ARN) of the Network Load Balancer.
trafficMirrorTarget_networkLoadBalancerArn :: Lens.Lens' TrafficMirrorTarget (Core.Maybe Core.Text)
trafficMirrorTarget_networkLoadBalancerArn = Lens.lens (\TrafficMirrorTarget' {networkLoadBalancerArn} -> networkLoadBalancerArn) (\s@TrafficMirrorTarget' {} a -> s {networkLoadBalancerArn = a} :: TrafficMirrorTarget)

-- | The tags assigned to the Traffic Mirror target.
trafficMirrorTarget_tags :: Lens.Lens' TrafficMirrorTarget (Core.Maybe [Tag])
trafficMirrorTarget_tags = Lens.lens (\TrafficMirrorTarget' {tags} -> tags) (\s@TrafficMirrorTarget' {} a -> s {tags = a} :: TrafficMirrorTarget) Core.. Lens.mapping Lens._Coerce

-- | The network interface ID that is attached to the target.
trafficMirrorTarget_networkInterfaceId :: Lens.Lens' TrafficMirrorTarget (Core.Maybe Core.Text)
trafficMirrorTarget_networkInterfaceId = Lens.lens (\TrafficMirrorTarget' {networkInterfaceId} -> networkInterfaceId) (\s@TrafficMirrorTarget' {} a -> s {networkInterfaceId = a} :: TrafficMirrorTarget)

-- | Information about the Traffic Mirror target.
trafficMirrorTarget_description :: Lens.Lens' TrafficMirrorTarget (Core.Maybe Core.Text)
trafficMirrorTarget_description = Lens.lens (\TrafficMirrorTarget' {description} -> description) (\s@TrafficMirrorTarget' {} a -> s {description = a} :: TrafficMirrorTarget)

-- | The type of Traffic Mirror target.
trafficMirrorTarget_type :: Lens.Lens' TrafficMirrorTarget (Core.Maybe TrafficMirrorTargetType)
trafficMirrorTarget_type = Lens.lens (\TrafficMirrorTarget' {type'} -> type') (\s@TrafficMirrorTarget' {} a -> s {type' = a} :: TrafficMirrorTarget)

-- | The ID of the Traffic Mirror target.
trafficMirrorTarget_trafficMirrorTargetId :: Lens.Lens' TrafficMirrorTarget (Core.Maybe Core.Text)
trafficMirrorTarget_trafficMirrorTargetId = Lens.lens (\TrafficMirrorTarget' {trafficMirrorTargetId} -> trafficMirrorTargetId) (\s@TrafficMirrorTarget' {} a -> s {trafficMirrorTargetId = a} :: TrafficMirrorTarget)

instance Core.FromXML TrafficMirrorTarget where
  parseXML x =
    TrafficMirrorTarget'
      Core.<$> (x Core..@? "ownerId")
      Core.<*> (x Core..@? "networkLoadBalancerArn")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "networkInterfaceId")
      Core.<*> (x Core..@? "description")
      Core.<*> (x Core..@? "type")
      Core.<*> (x Core..@? "trafficMirrorTargetId")

instance Core.Hashable TrafficMirrorTarget

instance Core.NFData TrafficMirrorTarget
