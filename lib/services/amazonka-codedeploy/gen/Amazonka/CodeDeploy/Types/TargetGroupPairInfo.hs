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
-- Module      : Amazonka.CodeDeploy.Types.TargetGroupPairInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.TargetGroupPairInfo where

import Amazonka.CodeDeploy.Types.TargetGroupInfo
import Amazonka.CodeDeploy.Types.TrafficRoute
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about two target groups and how traffic is routed during an
-- Amazon ECS deployment. An optional test traffic route can be specified.
--
-- /See:/ 'newTargetGroupPairInfo' smart constructor.
data TargetGroupPairInfo = TargetGroupPairInfo'
  { -- | An optional path used by a load balancer to route test traffic after an
    -- Amazon ECS deployment. Validation can occur while test traffic is served
    -- during a deployment.
    testTrafficRoute :: Prelude.Maybe TrafficRoute,
    -- | The path used by a load balancer to route production traffic when an
    -- Amazon ECS deployment is complete.
    prodTrafficRoute :: Prelude.Maybe TrafficRoute,
    -- | One pair of target groups. One is associated with the original task set.
    -- The second is associated with the task set that serves traffic after the
    -- deployment is complete.
    targetGroups :: Prelude.Maybe [TargetGroupInfo]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetGroupPairInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'testTrafficRoute', 'targetGroupPairInfo_testTrafficRoute' - An optional path used by a load balancer to route test traffic after an
-- Amazon ECS deployment. Validation can occur while test traffic is served
-- during a deployment.
--
-- 'prodTrafficRoute', 'targetGroupPairInfo_prodTrafficRoute' - The path used by a load balancer to route production traffic when an
-- Amazon ECS deployment is complete.
--
-- 'targetGroups', 'targetGroupPairInfo_targetGroups' - One pair of target groups. One is associated with the original task set.
-- The second is associated with the task set that serves traffic after the
-- deployment is complete.
newTargetGroupPairInfo ::
  TargetGroupPairInfo
newTargetGroupPairInfo =
  TargetGroupPairInfo'
    { testTrafficRoute =
        Prelude.Nothing,
      prodTrafficRoute = Prelude.Nothing,
      targetGroups = Prelude.Nothing
    }

-- | An optional path used by a load balancer to route test traffic after an
-- Amazon ECS deployment. Validation can occur while test traffic is served
-- during a deployment.
targetGroupPairInfo_testTrafficRoute :: Lens.Lens' TargetGroupPairInfo (Prelude.Maybe TrafficRoute)
targetGroupPairInfo_testTrafficRoute = Lens.lens (\TargetGroupPairInfo' {testTrafficRoute} -> testTrafficRoute) (\s@TargetGroupPairInfo' {} a -> s {testTrafficRoute = a} :: TargetGroupPairInfo)

-- | The path used by a load balancer to route production traffic when an
-- Amazon ECS deployment is complete.
targetGroupPairInfo_prodTrafficRoute :: Lens.Lens' TargetGroupPairInfo (Prelude.Maybe TrafficRoute)
targetGroupPairInfo_prodTrafficRoute = Lens.lens (\TargetGroupPairInfo' {prodTrafficRoute} -> prodTrafficRoute) (\s@TargetGroupPairInfo' {} a -> s {prodTrafficRoute = a} :: TargetGroupPairInfo)

-- | One pair of target groups. One is associated with the original task set.
-- The second is associated with the task set that serves traffic after the
-- deployment is complete.
targetGroupPairInfo_targetGroups :: Lens.Lens' TargetGroupPairInfo (Prelude.Maybe [TargetGroupInfo])
targetGroupPairInfo_targetGroups = Lens.lens (\TargetGroupPairInfo' {targetGroups} -> targetGroups) (\s@TargetGroupPairInfo' {} a -> s {targetGroups = a} :: TargetGroupPairInfo) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON TargetGroupPairInfo where
  parseJSON =
    Data.withObject
      "TargetGroupPairInfo"
      ( \x ->
          TargetGroupPairInfo'
            Prelude.<$> (x Data..:? "testTrafficRoute")
            Prelude.<*> (x Data..:? "prodTrafficRoute")
            Prelude.<*> (x Data..:? "targetGroups" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable TargetGroupPairInfo where
  hashWithSalt _salt TargetGroupPairInfo' {..} =
    _salt `Prelude.hashWithSalt` testTrafficRoute
      `Prelude.hashWithSalt` prodTrafficRoute
      `Prelude.hashWithSalt` targetGroups

instance Prelude.NFData TargetGroupPairInfo where
  rnf TargetGroupPairInfo' {..} =
    Prelude.rnf testTrafficRoute
      `Prelude.seq` Prelude.rnf prodTrafficRoute
      `Prelude.seq` Prelude.rnf targetGroups

instance Data.ToJSON TargetGroupPairInfo where
  toJSON TargetGroupPairInfo' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("testTrafficRoute" Data..=)
              Prelude.<$> testTrafficRoute,
            ("prodTrafficRoute" Data..=)
              Prelude.<$> prodTrafficRoute,
            ("targetGroups" Data..=) Prelude.<$> targetGroups
          ]
      )
