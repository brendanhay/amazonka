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
-- Module      : Amazonka.CodeDeploy.Types.TargetGroupInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.TargetGroupInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a target group in Elastic Load Balancing to use in a
-- deployment. Instances are registered as targets in a target group, and
-- traffic is routed to the target group.
--
-- /See:/ 'newTargetGroupInfo' smart constructor.
data TargetGroupInfo = TargetGroupInfo'
  { -- | For blue\/green deployments, the name of the target group that instances
    -- in the original environment are deregistered from, and instances in the
    -- replacement environment are registered with. For in-place deployments,
    -- the name of the target group that instances are deregistered from, so
    -- they are not serving traffic during a deployment, and then re-registered
    -- with after the deployment is complete.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetGroupInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'targetGroupInfo_name' - For blue\/green deployments, the name of the target group that instances
-- in the original environment are deregistered from, and instances in the
-- replacement environment are registered with. For in-place deployments,
-- the name of the target group that instances are deregistered from, so
-- they are not serving traffic during a deployment, and then re-registered
-- with after the deployment is complete.
newTargetGroupInfo ::
  TargetGroupInfo
newTargetGroupInfo =
  TargetGroupInfo' {name = Prelude.Nothing}

-- | For blue\/green deployments, the name of the target group that instances
-- in the original environment are deregistered from, and instances in the
-- replacement environment are registered with. For in-place deployments,
-- the name of the target group that instances are deregistered from, so
-- they are not serving traffic during a deployment, and then re-registered
-- with after the deployment is complete.
targetGroupInfo_name :: Lens.Lens' TargetGroupInfo (Prelude.Maybe Prelude.Text)
targetGroupInfo_name = Lens.lens (\TargetGroupInfo' {name} -> name) (\s@TargetGroupInfo' {} a -> s {name = a} :: TargetGroupInfo)

instance Data.FromJSON TargetGroupInfo where
  parseJSON =
    Data.withObject
      "TargetGroupInfo"
      ( \x ->
          TargetGroupInfo' Prelude.<$> (x Data..:? "name")
      )

instance Prelude.Hashable TargetGroupInfo where
  hashWithSalt _salt TargetGroupInfo' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData TargetGroupInfo where
  rnf TargetGroupInfo' {..} = Prelude.rnf name

instance Data.ToJSON TargetGroupInfo where
  toJSON TargetGroupInfo' {..} =
    Data.object
      ( Prelude.catMaybes
          [("name" Data..=) Prelude.<$> name]
      )
