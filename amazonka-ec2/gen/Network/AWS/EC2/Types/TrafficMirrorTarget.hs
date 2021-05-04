{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TrafficMirrorTargetType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a Traffic Mirror target.
--
-- /See:/ 'newTrafficMirrorTarget' smart constructor.
data TrafficMirrorTarget = TrafficMirrorTarget'
  { -- | The ID of the account that owns the Traffic Mirror target.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Network Load Balancer.
    networkLoadBalancerArn :: Prelude.Maybe Prelude.Text,
    -- | The tags assigned to the Traffic Mirror target.
    tags :: Prelude.Maybe [Tag],
    -- | The network interface ID that is attached to the target.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | Information about the Traffic Mirror target.
    description :: Prelude.Maybe Prelude.Text,
    -- | The type of Traffic Mirror target.
    type' :: Prelude.Maybe TrafficMirrorTargetType,
    -- | The ID of the Traffic Mirror target.
    trafficMirrorTargetId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { ownerId = Prelude.Nothing,
      networkLoadBalancerArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      description = Prelude.Nothing,
      type' = Prelude.Nothing,
      trafficMirrorTargetId = Prelude.Nothing
    }

-- | The ID of the account that owns the Traffic Mirror target.
trafficMirrorTarget_ownerId :: Lens.Lens' TrafficMirrorTarget (Prelude.Maybe Prelude.Text)
trafficMirrorTarget_ownerId = Lens.lens (\TrafficMirrorTarget' {ownerId} -> ownerId) (\s@TrafficMirrorTarget' {} a -> s {ownerId = a} :: TrafficMirrorTarget)

-- | The Amazon Resource Name (ARN) of the Network Load Balancer.
trafficMirrorTarget_networkLoadBalancerArn :: Lens.Lens' TrafficMirrorTarget (Prelude.Maybe Prelude.Text)
trafficMirrorTarget_networkLoadBalancerArn = Lens.lens (\TrafficMirrorTarget' {networkLoadBalancerArn} -> networkLoadBalancerArn) (\s@TrafficMirrorTarget' {} a -> s {networkLoadBalancerArn = a} :: TrafficMirrorTarget)

-- | The tags assigned to the Traffic Mirror target.
trafficMirrorTarget_tags :: Lens.Lens' TrafficMirrorTarget (Prelude.Maybe [Tag])
trafficMirrorTarget_tags = Lens.lens (\TrafficMirrorTarget' {tags} -> tags) (\s@TrafficMirrorTarget' {} a -> s {tags = a} :: TrafficMirrorTarget) Prelude.. Lens.mapping Prelude._Coerce

-- | The network interface ID that is attached to the target.
trafficMirrorTarget_networkInterfaceId :: Lens.Lens' TrafficMirrorTarget (Prelude.Maybe Prelude.Text)
trafficMirrorTarget_networkInterfaceId = Lens.lens (\TrafficMirrorTarget' {networkInterfaceId} -> networkInterfaceId) (\s@TrafficMirrorTarget' {} a -> s {networkInterfaceId = a} :: TrafficMirrorTarget)

-- | Information about the Traffic Mirror target.
trafficMirrorTarget_description :: Lens.Lens' TrafficMirrorTarget (Prelude.Maybe Prelude.Text)
trafficMirrorTarget_description = Lens.lens (\TrafficMirrorTarget' {description} -> description) (\s@TrafficMirrorTarget' {} a -> s {description = a} :: TrafficMirrorTarget)

-- | The type of Traffic Mirror target.
trafficMirrorTarget_type :: Lens.Lens' TrafficMirrorTarget (Prelude.Maybe TrafficMirrorTargetType)
trafficMirrorTarget_type = Lens.lens (\TrafficMirrorTarget' {type'} -> type') (\s@TrafficMirrorTarget' {} a -> s {type' = a} :: TrafficMirrorTarget)

-- | The ID of the Traffic Mirror target.
trafficMirrorTarget_trafficMirrorTargetId :: Lens.Lens' TrafficMirrorTarget (Prelude.Maybe Prelude.Text)
trafficMirrorTarget_trafficMirrorTargetId = Lens.lens (\TrafficMirrorTarget' {trafficMirrorTargetId} -> trafficMirrorTargetId) (\s@TrafficMirrorTarget' {} a -> s {trafficMirrorTargetId = a} :: TrafficMirrorTarget)

instance Prelude.FromXML TrafficMirrorTarget where
  parseXML x =
    TrafficMirrorTarget'
      Prelude.<$> (x Prelude..@? "ownerId")
      Prelude.<*> (x Prelude..@? "networkLoadBalancerArn")
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "networkInterfaceId")
      Prelude.<*> (x Prelude..@? "description")
      Prelude.<*> (x Prelude..@? "type")
      Prelude.<*> (x Prelude..@? "trafficMirrorTargetId")

instance Prelude.Hashable TrafficMirrorTarget

instance Prelude.NFData TrafficMirrorTarget
