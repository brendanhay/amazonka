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
-- Module      : Network.AWS.CodeDeploy.Types.TargetGroupPairInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TargetGroupPairInfo where

import Network.AWS.CodeDeploy.Types.TargetGroupInfo
import Network.AWS.CodeDeploy.Types.TrafficRoute
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about two target groups and how traffic is routed during an
-- Amazon ECS deployment. An optional test traffic route can be specified.
--
-- /See:/ 'newTargetGroupPairInfo' smart constructor.
data TargetGroupPairInfo = TargetGroupPairInfo'
  { -- | One pair of target groups. One is associated with the original task set.
    -- The second is associated with the task set that serves traffic after the
    -- deployment is complete.
    targetGroups :: Prelude.Maybe [TargetGroupInfo],
    -- | The path used by a load balancer to route production traffic when an
    -- Amazon ECS deployment is complete.
    prodTrafficRoute :: Prelude.Maybe TrafficRoute,
    -- | An optional path used by a load balancer to route test traffic after an
    -- Amazon ECS deployment. Validation can occur while test traffic is served
    -- during a deployment.
    testTrafficRoute :: Prelude.Maybe TrafficRoute
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TargetGroupPairInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetGroups', 'targetGroupPairInfo_targetGroups' - One pair of target groups. One is associated with the original task set.
-- The second is associated with the task set that serves traffic after the
-- deployment is complete.
--
-- 'prodTrafficRoute', 'targetGroupPairInfo_prodTrafficRoute' - The path used by a load balancer to route production traffic when an
-- Amazon ECS deployment is complete.
--
-- 'testTrafficRoute', 'targetGroupPairInfo_testTrafficRoute' - An optional path used by a load balancer to route test traffic after an
-- Amazon ECS deployment. Validation can occur while test traffic is served
-- during a deployment.
newTargetGroupPairInfo ::
  TargetGroupPairInfo
newTargetGroupPairInfo =
  TargetGroupPairInfo'
    { targetGroups =
        Prelude.Nothing,
      prodTrafficRoute = Prelude.Nothing,
      testTrafficRoute = Prelude.Nothing
    }

-- | One pair of target groups. One is associated with the original task set.
-- The second is associated with the task set that serves traffic after the
-- deployment is complete.
targetGroupPairInfo_targetGroups :: Lens.Lens' TargetGroupPairInfo (Prelude.Maybe [TargetGroupInfo])
targetGroupPairInfo_targetGroups = Lens.lens (\TargetGroupPairInfo' {targetGroups} -> targetGroups) (\s@TargetGroupPairInfo' {} a -> s {targetGroups = a} :: TargetGroupPairInfo) Prelude.. Lens.mapping Prelude._Coerce

-- | The path used by a load balancer to route production traffic when an
-- Amazon ECS deployment is complete.
targetGroupPairInfo_prodTrafficRoute :: Lens.Lens' TargetGroupPairInfo (Prelude.Maybe TrafficRoute)
targetGroupPairInfo_prodTrafficRoute = Lens.lens (\TargetGroupPairInfo' {prodTrafficRoute} -> prodTrafficRoute) (\s@TargetGroupPairInfo' {} a -> s {prodTrafficRoute = a} :: TargetGroupPairInfo)

-- | An optional path used by a load balancer to route test traffic after an
-- Amazon ECS deployment. Validation can occur while test traffic is served
-- during a deployment.
targetGroupPairInfo_testTrafficRoute :: Lens.Lens' TargetGroupPairInfo (Prelude.Maybe TrafficRoute)
targetGroupPairInfo_testTrafficRoute = Lens.lens (\TargetGroupPairInfo' {testTrafficRoute} -> testTrafficRoute) (\s@TargetGroupPairInfo' {} a -> s {testTrafficRoute = a} :: TargetGroupPairInfo)

instance Prelude.FromJSON TargetGroupPairInfo where
  parseJSON =
    Prelude.withObject
      "TargetGroupPairInfo"
      ( \x ->
          TargetGroupPairInfo'
            Prelude.<$> ( x Prelude..:? "targetGroups"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "prodTrafficRoute")
            Prelude.<*> (x Prelude..:? "testTrafficRoute")
      )

instance Prelude.Hashable TargetGroupPairInfo

instance Prelude.NFData TargetGroupPairInfo

instance Prelude.ToJSON TargetGroupPairInfo where
  toJSON TargetGroupPairInfo' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("targetGroups" Prelude..=)
              Prelude.<$> targetGroups,
            ("prodTrafficRoute" Prelude..=)
              Prelude.<$> prodTrafficRoute,
            ("testTrafficRoute" Prelude..=)
              Prelude.<$> testTrafficRoute
          ]
      )
