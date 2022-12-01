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
-- Module      : Amazonka.CodeDeploy.Types.LoadBalancerInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.LoadBalancerInfo where

import Amazonka.CodeDeploy.Types.ELBInfo
import Amazonka.CodeDeploy.Types.TargetGroupInfo
import Amazonka.CodeDeploy.Types.TargetGroupPairInfo
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the Elastic Load Balancing load balancer or target
-- group used in a deployment.
--
-- /See:/ 'newLoadBalancerInfo' smart constructor.
data LoadBalancerInfo = LoadBalancerInfo'
  { -- | The target group pair information. This is an array of
    -- @TargeGroupPairInfo@ objects with a maximum size of one.
    targetGroupPairInfoList :: Prelude.Maybe [TargetGroupPairInfo],
    -- | An array that contains information about the load balancer to use for
    -- load balancing in a deployment. In Elastic Load Balancing, load
    -- balancers are used with Classic Load Balancers.
    --
    -- Adding more than one load balancer to the array is not supported.
    elbInfoList :: Prelude.Maybe [ELBInfo],
    -- | An array that contains information about the target group to use for
    -- load balancing in a deployment. In Elastic Load Balancing, target groups
    -- are used with Application Load Balancers.
    --
    -- Adding more than one target group to the array is not supported.
    targetGroupInfoList :: Prelude.Maybe [TargetGroupInfo]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      elbInfoList = Prelude.Nothing,
      targetGroupInfoList = Prelude.Nothing
    }

-- | The target group pair information. This is an array of
-- @TargeGroupPairInfo@ objects with a maximum size of one.
loadBalancerInfo_targetGroupPairInfoList :: Lens.Lens' LoadBalancerInfo (Prelude.Maybe [TargetGroupPairInfo])
loadBalancerInfo_targetGroupPairInfoList = Lens.lens (\LoadBalancerInfo' {targetGroupPairInfoList} -> targetGroupPairInfoList) (\s@LoadBalancerInfo' {} a -> s {targetGroupPairInfoList = a} :: LoadBalancerInfo) Prelude.. Lens.mapping Lens.coerced

-- | An array that contains information about the load balancer to use for
-- load balancing in a deployment. In Elastic Load Balancing, load
-- balancers are used with Classic Load Balancers.
--
-- Adding more than one load balancer to the array is not supported.
loadBalancerInfo_elbInfoList :: Lens.Lens' LoadBalancerInfo (Prelude.Maybe [ELBInfo])
loadBalancerInfo_elbInfoList = Lens.lens (\LoadBalancerInfo' {elbInfoList} -> elbInfoList) (\s@LoadBalancerInfo' {} a -> s {elbInfoList = a} :: LoadBalancerInfo) Prelude.. Lens.mapping Lens.coerced

-- | An array that contains information about the target group to use for
-- load balancing in a deployment. In Elastic Load Balancing, target groups
-- are used with Application Load Balancers.
--
-- Adding more than one target group to the array is not supported.
loadBalancerInfo_targetGroupInfoList :: Lens.Lens' LoadBalancerInfo (Prelude.Maybe [TargetGroupInfo])
loadBalancerInfo_targetGroupInfoList = Lens.lens (\LoadBalancerInfo' {targetGroupInfoList} -> targetGroupInfoList) (\s@LoadBalancerInfo' {} a -> s {targetGroupInfoList = a} :: LoadBalancerInfo) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON LoadBalancerInfo where
  parseJSON =
    Core.withObject
      "LoadBalancerInfo"
      ( \x ->
          LoadBalancerInfo'
            Prelude.<$> ( x Core..:? "targetGroupPairInfoList"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "elbInfoList" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "targetGroupInfoList"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable LoadBalancerInfo where
  hashWithSalt _salt LoadBalancerInfo' {..} =
    _salt
      `Prelude.hashWithSalt` targetGroupPairInfoList
      `Prelude.hashWithSalt` elbInfoList
      `Prelude.hashWithSalt` targetGroupInfoList

instance Prelude.NFData LoadBalancerInfo where
  rnf LoadBalancerInfo' {..} =
    Prelude.rnf targetGroupPairInfoList
      `Prelude.seq` Prelude.rnf elbInfoList
      `Prelude.seq` Prelude.rnf targetGroupInfoList

instance Core.ToJSON LoadBalancerInfo where
  toJSON LoadBalancerInfo' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("targetGroupPairInfoList" Core..=)
              Prelude.<$> targetGroupPairInfoList,
            ("elbInfoList" Core..=) Prelude.<$> elbInfoList,
            ("targetGroupInfoList" Core..=)
              Prelude.<$> targetGroupInfoList
          ]
      )
