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
-- Module      : Network.AWS.CodeDeploy.Types.LoadBalancerInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.LoadBalancerInfo where

import Network.AWS.CodeDeploy.Types.ELBInfo
import Network.AWS.CodeDeploy.Types.TargetGroupInfo
import Network.AWS.CodeDeploy.Types.TargetGroupPairInfo
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about the Elastic Load Balancing load balancer or target
-- group used in a deployment.
--
-- /See:/ 'newLoadBalancerInfo' smart constructor.
data LoadBalancerInfo = LoadBalancerInfo'
  { -- | The target group pair information. This is an array of
    -- @TargeGroupPairInfo@ objects with a maximum size of one.
    targetGroupPairInfoList :: Core.Maybe [TargetGroupPairInfo],
    -- | An array that contains information about the load balancer to use for
    -- load balancing in a deployment. In Elastic Load Balancing, load
    -- balancers are used with Classic Load Balancers.
    --
    -- Adding more than one load balancer to the array is not supported.
    elbInfoList :: Core.Maybe [ELBInfo],
    -- | An array that contains information about the target group to use for
    -- load balancing in a deployment. In Elastic Load Balancing, target groups
    -- are used with Application Load Balancers.
    --
    -- Adding more than one target group to the array is not supported.
    targetGroupInfoList :: Core.Maybe [TargetGroupInfo]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LoadBalancerInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetGroupPairInfoList', 'loadBalancerInfo_targetGroupPairInfoList' - The target group pair information. This is an array of
-- @TargeGroupPairInfo@ objects with a maximum size of one.
--
-- 'elbInfoList', 'loadBalancerInfo_elbInfoList' - An array that contains information about the load balancer to use for
-- load balancing in a deployment. In Elastic Load Balancing, load
-- balancers are used with Classic Load Balancers.
--
-- Adding more than one load balancer to the array is not supported.
--
-- 'targetGroupInfoList', 'loadBalancerInfo_targetGroupInfoList' - An array that contains information about the target group to use for
-- load balancing in a deployment. In Elastic Load Balancing, target groups
-- are used with Application Load Balancers.
--
-- Adding more than one target group to the array is not supported.
newLoadBalancerInfo ::
  LoadBalancerInfo
newLoadBalancerInfo =
  LoadBalancerInfo'
    { targetGroupPairInfoList =
        Core.Nothing,
      elbInfoList = Core.Nothing,
      targetGroupInfoList = Core.Nothing
    }

-- | The target group pair information. This is an array of
-- @TargeGroupPairInfo@ objects with a maximum size of one.
loadBalancerInfo_targetGroupPairInfoList :: Lens.Lens' LoadBalancerInfo (Core.Maybe [TargetGroupPairInfo])
loadBalancerInfo_targetGroupPairInfoList = Lens.lens (\LoadBalancerInfo' {targetGroupPairInfoList} -> targetGroupPairInfoList) (\s@LoadBalancerInfo' {} a -> s {targetGroupPairInfoList = a} :: LoadBalancerInfo) Core.. Lens.mapping Lens._Coerce

-- | An array that contains information about the load balancer to use for
-- load balancing in a deployment. In Elastic Load Balancing, load
-- balancers are used with Classic Load Balancers.
--
-- Adding more than one load balancer to the array is not supported.
loadBalancerInfo_elbInfoList :: Lens.Lens' LoadBalancerInfo (Core.Maybe [ELBInfo])
loadBalancerInfo_elbInfoList = Lens.lens (\LoadBalancerInfo' {elbInfoList} -> elbInfoList) (\s@LoadBalancerInfo' {} a -> s {elbInfoList = a} :: LoadBalancerInfo) Core.. Lens.mapping Lens._Coerce

-- | An array that contains information about the target group to use for
-- load balancing in a deployment. In Elastic Load Balancing, target groups
-- are used with Application Load Balancers.
--
-- Adding more than one target group to the array is not supported.
loadBalancerInfo_targetGroupInfoList :: Lens.Lens' LoadBalancerInfo (Core.Maybe [TargetGroupInfo])
loadBalancerInfo_targetGroupInfoList = Lens.lens (\LoadBalancerInfo' {targetGroupInfoList} -> targetGroupInfoList) (\s@LoadBalancerInfo' {} a -> s {targetGroupInfoList = a} :: LoadBalancerInfo) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON LoadBalancerInfo where
  parseJSON =
    Core.withObject
      "LoadBalancerInfo"
      ( \x ->
          LoadBalancerInfo'
            Core.<$> ( x Core..:? "targetGroupPairInfoList"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "elbInfoList" Core..!= Core.mempty)
            Core.<*> ( x Core..:? "targetGroupInfoList"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable LoadBalancerInfo

instance Core.NFData LoadBalancerInfo

instance Core.ToJSON LoadBalancerInfo where
  toJSON LoadBalancerInfo' {..} =
    Core.object
      ( Core.catMaybes
          [ ("targetGroupPairInfoList" Core..=)
              Core.<$> targetGroupPairInfoList,
            ("elbInfoList" Core..=) Core.<$> elbInfoList,
            ("targetGroupInfoList" Core..=)
              Core.<$> targetGroupInfoList
          ]
      )
