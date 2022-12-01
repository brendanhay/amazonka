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
-- Module      : Amazonka.EC2.Types.TrafficMirrorTarget
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TrafficMirrorTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Tag
import Amazonka.EC2.Types.TrafficMirrorTargetType
import qualified Amazonka.Prelude as Prelude

-- | Describes a Traffic Mirror target.
--
-- /See:/ 'newTrafficMirrorTarget' smart constructor.
data TrafficMirrorTarget = TrafficMirrorTarget'
  { -- | The tags assigned to the Traffic Mirror target.
    tags :: Prelude.Maybe [Tag],
    -- | The type of Traffic Mirror target.
    type' :: Prelude.Maybe TrafficMirrorTargetType,
    -- | The ID of the account that owns the Traffic Mirror target.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Network Load Balancer.
    networkLoadBalancerArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Traffic Mirror target.
    trafficMirrorTargetId :: Prelude.Maybe Prelude.Text,
    -- | Information about the Traffic Mirror target.
    description :: Prelude.Maybe Prelude.Text,
    -- | The network interface ID that is attached to the target.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Gateway Load Balancer endpoint.
    gatewayLoadBalancerEndpointId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrafficMirrorTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'trafficMirrorTarget_tags' - The tags assigned to the Traffic Mirror target.
--
-- 'type'', 'trafficMirrorTarget_type' - The type of Traffic Mirror target.
--
-- 'ownerId', 'trafficMirrorTarget_ownerId' - The ID of the account that owns the Traffic Mirror target.
--
-- 'networkLoadBalancerArn', 'trafficMirrorTarget_networkLoadBalancerArn' - The Amazon Resource Name (ARN) of the Network Load Balancer.
--
-- 'trafficMirrorTargetId', 'trafficMirrorTarget_trafficMirrorTargetId' - The ID of the Traffic Mirror target.
--
-- 'description', 'trafficMirrorTarget_description' - Information about the Traffic Mirror target.
--
-- 'networkInterfaceId', 'trafficMirrorTarget_networkInterfaceId' - The network interface ID that is attached to the target.
--
-- 'gatewayLoadBalancerEndpointId', 'trafficMirrorTarget_gatewayLoadBalancerEndpointId' - The ID of the Gateway Load Balancer endpoint.
newTrafficMirrorTarget ::
  TrafficMirrorTarget
newTrafficMirrorTarget =
  TrafficMirrorTarget'
    { tags = Prelude.Nothing,
      type' = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      networkLoadBalancerArn = Prelude.Nothing,
      trafficMirrorTargetId = Prelude.Nothing,
      description = Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      gatewayLoadBalancerEndpointId = Prelude.Nothing
    }

-- | The tags assigned to the Traffic Mirror target.
trafficMirrorTarget_tags :: Lens.Lens' TrafficMirrorTarget (Prelude.Maybe [Tag])
trafficMirrorTarget_tags = Lens.lens (\TrafficMirrorTarget' {tags} -> tags) (\s@TrafficMirrorTarget' {} a -> s {tags = a} :: TrafficMirrorTarget) Prelude.. Lens.mapping Lens.coerced

-- | The type of Traffic Mirror target.
trafficMirrorTarget_type :: Lens.Lens' TrafficMirrorTarget (Prelude.Maybe TrafficMirrorTargetType)
trafficMirrorTarget_type = Lens.lens (\TrafficMirrorTarget' {type'} -> type') (\s@TrafficMirrorTarget' {} a -> s {type' = a} :: TrafficMirrorTarget)

-- | The ID of the account that owns the Traffic Mirror target.
trafficMirrorTarget_ownerId :: Lens.Lens' TrafficMirrorTarget (Prelude.Maybe Prelude.Text)
trafficMirrorTarget_ownerId = Lens.lens (\TrafficMirrorTarget' {ownerId} -> ownerId) (\s@TrafficMirrorTarget' {} a -> s {ownerId = a} :: TrafficMirrorTarget)

-- | The Amazon Resource Name (ARN) of the Network Load Balancer.
trafficMirrorTarget_networkLoadBalancerArn :: Lens.Lens' TrafficMirrorTarget (Prelude.Maybe Prelude.Text)
trafficMirrorTarget_networkLoadBalancerArn = Lens.lens (\TrafficMirrorTarget' {networkLoadBalancerArn} -> networkLoadBalancerArn) (\s@TrafficMirrorTarget' {} a -> s {networkLoadBalancerArn = a} :: TrafficMirrorTarget)

-- | The ID of the Traffic Mirror target.
trafficMirrorTarget_trafficMirrorTargetId :: Lens.Lens' TrafficMirrorTarget (Prelude.Maybe Prelude.Text)
trafficMirrorTarget_trafficMirrorTargetId = Lens.lens (\TrafficMirrorTarget' {trafficMirrorTargetId} -> trafficMirrorTargetId) (\s@TrafficMirrorTarget' {} a -> s {trafficMirrorTargetId = a} :: TrafficMirrorTarget)

-- | Information about the Traffic Mirror target.
trafficMirrorTarget_description :: Lens.Lens' TrafficMirrorTarget (Prelude.Maybe Prelude.Text)
trafficMirrorTarget_description = Lens.lens (\TrafficMirrorTarget' {description} -> description) (\s@TrafficMirrorTarget' {} a -> s {description = a} :: TrafficMirrorTarget)

-- | The network interface ID that is attached to the target.
trafficMirrorTarget_networkInterfaceId :: Lens.Lens' TrafficMirrorTarget (Prelude.Maybe Prelude.Text)
trafficMirrorTarget_networkInterfaceId = Lens.lens (\TrafficMirrorTarget' {networkInterfaceId} -> networkInterfaceId) (\s@TrafficMirrorTarget' {} a -> s {networkInterfaceId = a} :: TrafficMirrorTarget)

-- | The ID of the Gateway Load Balancer endpoint.
trafficMirrorTarget_gatewayLoadBalancerEndpointId :: Lens.Lens' TrafficMirrorTarget (Prelude.Maybe Prelude.Text)
trafficMirrorTarget_gatewayLoadBalancerEndpointId = Lens.lens (\TrafficMirrorTarget' {gatewayLoadBalancerEndpointId} -> gatewayLoadBalancerEndpointId) (\s@TrafficMirrorTarget' {} a -> s {gatewayLoadBalancerEndpointId = a} :: TrafficMirrorTarget)

instance Core.FromXML TrafficMirrorTarget where
  parseXML x =
    TrafficMirrorTarget'
      Prelude.<$> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "type")
      Prelude.<*> (x Core..@? "ownerId")
      Prelude.<*> (x Core..@? "networkLoadBalancerArn")
      Prelude.<*> (x Core..@? "trafficMirrorTargetId")
      Prelude.<*> (x Core..@? "description")
      Prelude.<*> (x Core..@? "networkInterfaceId")
      Prelude.<*> (x Core..@? "gatewayLoadBalancerEndpointId")

instance Prelude.Hashable TrafficMirrorTarget where
  hashWithSalt _salt TrafficMirrorTarget' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` networkLoadBalancerArn
      `Prelude.hashWithSalt` trafficMirrorTargetId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` networkInterfaceId
      `Prelude.hashWithSalt` gatewayLoadBalancerEndpointId

instance Prelude.NFData TrafficMirrorTarget where
  rnf TrafficMirrorTarget' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf networkLoadBalancerArn
      `Prelude.seq` Prelude.rnf trafficMirrorTargetId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf networkInterfaceId
      `Prelude.seq` Prelude.rnf gatewayLoadBalancerEndpointId
