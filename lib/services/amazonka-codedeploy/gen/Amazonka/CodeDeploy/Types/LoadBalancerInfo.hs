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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.LoadBalancerInfo where

import Amazonka.CodeDeploy.Types.ELBInfo
import Amazonka.CodeDeploy.Types.TargetGroupInfo
import Amazonka.CodeDeploy.Types.TargetGroupPairInfo
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the Elastic Load Balancing load balancer or target
-- group used in a deployment.
--
-- /See:/ 'newLoadBalancerInfo' smart constructor.
data LoadBalancerInfo = LoadBalancerInfo'
  { -- | An array that contains information about the load balancer to use for
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
    targetGroupInfoList :: Prelude.Maybe [TargetGroupInfo],
    -- | The target group pair information. This is an array of
    -- @TargeGroupPairInfo@ objects with a maximum size of one.
    targetGroupPairInfoList :: Prelude.Maybe [TargetGroupPairInfo]
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
--
-- 'targetGroupPairInfoList', 'loadBalancerInfo_targetGroupPairInfoList' - The target group pair information. This is an array of
-- @TargeGroupPairInfo@ objects with a maximum size of one.
newLoadBalancerInfo ::
  LoadBalancerInfo
newLoadBalancerInfo =
  LoadBalancerInfo'
    { elbInfoList = Prelude.Nothing,
      targetGroupInfoList = Prelude.Nothing,
      targetGroupPairInfoList = Prelude.Nothing
    }

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

-- | The target group pair information. This is an array of
-- @TargeGroupPairInfo@ objects with a maximum size of one.
loadBalancerInfo_targetGroupPairInfoList :: Lens.Lens' LoadBalancerInfo (Prelude.Maybe [TargetGroupPairInfo])
loadBalancerInfo_targetGroupPairInfoList = Lens.lens (\LoadBalancerInfo' {targetGroupPairInfoList} -> targetGroupPairInfoList) (\s@LoadBalancerInfo' {} a -> s {targetGroupPairInfoList = a} :: LoadBalancerInfo) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON LoadBalancerInfo where
  parseJSON =
    Data.withObject
      "LoadBalancerInfo"
      ( \x ->
          LoadBalancerInfo'
            Prelude.<$> (x Data..:? "elbInfoList" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "targetGroupInfoList"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "targetGroupPairInfoList"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable LoadBalancerInfo where
  hashWithSalt _salt LoadBalancerInfo' {..} =
    _salt
      `Prelude.hashWithSalt` elbInfoList
      `Prelude.hashWithSalt` targetGroupInfoList
      `Prelude.hashWithSalt` targetGroupPairInfoList

instance Prelude.NFData LoadBalancerInfo where
  rnf LoadBalancerInfo' {..} =
    Prelude.rnf elbInfoList `Prelude.seq`
      Prelude.rnf targetGroupInfoList `Prelude.seq`
        Prelude.rnf targetGroupPairInfoList

instance Data.ToJSON LoadBalancerInfo where
  toJSON LoadBalancerInfo' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("elbInfoList" Data..=) Prelude.<$> elbInfoList,
            ("targetGroupInfoList" Data..=)
              Prelude.<$> targetGroupInfoList,
            ("targetGroupPairInfoList" Data..=)
              Prelude.<$> targetGroupPairInfoList
          ]
      )
