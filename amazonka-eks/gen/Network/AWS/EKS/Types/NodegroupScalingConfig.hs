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
-- Module      : Network.AWS.EKS.Types.NodegroupScalingConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.NodegroupScalingConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An object representing the scaling configuration details for the Auto
-- Scaling group that is associated with your node group. If you specify a
-- value for any property, then you must specify values for all of the
-- properties.
--
-- /See:/ 'newNodegroupScalingConfig' smart constructor.
data NodegroupScalingConfig = NodegroupScalingConfig'
  { -- | The minimum number of nodes that the managed node group can scale in to.
    -- This number must be greater than zero.
    minSize :: Core.Maybe Core.Natural,
    -- | The current number of nodes that the managed node group should maintain.
    desiredSize :: Core.Maybe Core.Natural,
    -- | The maximum number of nodes that the managed node group can scale out
    -- to. For information about the maximum number that you can specify, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/service-quotas.html Amazon EKS service quotas>
    -- in the /Amazon EKS User Guide/.
    maxSize :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NodegroupScalingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minSize', 'nodegroupScalingConfig_minSize' - The minimum number of nodes that the managed node group can scale in to.
-- This number must be greater than zero.
--
-- 'desiredSize', 'nodegroupScalingConfig_desiredSize' - The current number of nodes that the managed node group should maintain.
--
-- 'maxSize', 'nodegroupScalingConfig_maxSize' - The maximum number of nodes that the managed node group can scale out
-- to. For information about the maximum number that you can specify, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/service-quotas.html Amazon EKS service quotas>
-- in the /Amazon EKS User Guide/.
newNodegroupScalingConfig ::
  NodegroupScalingConfig
newNodegroupScalingConfig =
  NodegroupScalingConfig'
    { minSize = Core.Nothing,
      desiredSize = Core.Nothing,
      maxSize = Core.Nothing
    }

-- | The minimum number of nodes that the managed node group can scale in to.
-- This number must be greater than zero.
nodegroupScalingConfig_minSize :: Lens.Lens' NodegroupScalingConfig (Core.Maybe Core.Natural)
nodegroupScalingConfig_minSize = Lens.lens (\NodegroupScalingConfig' {minSize} -> minSize) (\s@NodegroupScalingConfig' {} a -> s {minSize = a} :: NodegroupScalingConfig)

-- | The current number of nodes that the managed node group should maintain.
nodegroupScalingConfig_desiredSize :: Lens.Lens' NodegroupScalingConfig (Core.Maybe Core.Natural)
nodegroupScalingConfig_desiredSize = Lens.lens (\NodegroupScalingConfig' {desiredSize} -> desiredSize) (\s@NodegroupScalingConfig' {} a -> s {desiredSize = a} :: NodegroupScalingConfig)

-- | The maximum number of nodes that the managed node group can scale out
-- to. For information about the maximum number that you can specify, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/service-quotas.html Amazon EKS service quotas>
-- in the /Amazon EKS User Guide/.
nodegroupScalingConfig_maxSize :: Lens.Lens' NodegroupScalingConfig (Core.Maybe Core.Natural)
nodegroupScalingConfig_maxSize = Lens.lens (\NodegroupScalingConfig' {maxSize} -> maxSize) (\s@NodegroupScalingConfig' {} a -> s {maxSize = a} :: NodegroupScalingConfig)

instance Core.FromJSON NodegroupScalingConfig where
  parseJSON =
    Core.withObject
      "NodegroupScalingConfig"
      ( \x ->
          NodegroupScalingConfig'
            Core.<$> (x Core..:? "minSize")
            Core.<*> (x Core..:? "desiredSize")
            Core.<*> (x Core..:? "maxSize")
      )

instance Core.Hashable NodegroupScalingConfig

instance Core.NFData NodegroupScalingConfig

instance Core.ToJSON NodegroupScalingConfig where
  toJSON NodegroupScalingConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("minSize" Core..=) Core.<$> minSize,
            ("desiredSize" Core..=) Core.<$> desiredSize,
            ("maxSize" Core..=) Core.<$> maxSize
          ]
      )
