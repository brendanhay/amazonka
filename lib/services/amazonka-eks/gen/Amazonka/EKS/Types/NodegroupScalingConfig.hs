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
-- Module      : Amazonka.EKS.Types.NodegroupScalingConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.NodegroupScalingConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object representing the scaling configuration details for the Auto
-- Scaling group that is associated with your node group. When creating a
-- node group, you must specify all or none of the properties. When
-- updating a node group, you can specify any or none of the properties.
--
-- /See:/ 'newNodegroupScalingConfig' smart constructor.
data NodegroupScalingConfig = NodegroupScalingConfig'
  { -- | The current number of nodes that the managed node group should maintain.
    desiredSize :: Prelude.Maybe Prelude.Natural,
    -- | The minimum number of nodes that the managed node group can scale in to.
    minSize :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of nodes that the managed node group can scale out
    -- to. For information about the maximum number that you can specify, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/service-quotas.html Amazon EKS service quotas>
    -- in the /Amazon EKS User Guide/.
    maxSize :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodegroupScalingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'desiredSize', 'nodegroupScalingConfig_desiredSize' - The current number of nodes that the managed node group should maintain.
--
-- 'minSize', 'nodegroupScalingConfig_minSize' - The minimum number of nodes that the managed node group can scale in to.
--
-- 'maxSize', 'nodegroupScalingConfig_maxSize' - The maximum number of nodes that the managed node group can scale out
-- to. For information about the maximum number that you can specify, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/service-quotas.html Amazon EKS service quotas>
-- in the /Amazon EKS User Guide/.
newNodegroupScalingConfig ::
  NodegroupScalingConfig
newNodegroupScalingConfig =
  NodegroupScalingConfig'
    { desiredSize =
        Prelude.Nothing,
      minSize = Prelude.Nothing,
      maxSize = Prelude.Nothing
    }

-- | The current number of nodes that the managed node group should maintain.
nodegroupScalingConfig_desiredSize :: Lens.Lens' NodegroupScalingConfig (Prelude.Maybe Prelude.Natural)
nodegroupScalingConfig_desiredSize = Lens.lens (\NodegroupScalingConfig' {desiredSize} -> desiredSize) (\s@NodegroupScalingConfig' {} a -> s {desiredSize = a} :: NodegroupScalingConfig)

-- | The minimum number of nodes that the managed node group can scale in to.
nodegroupScalingConfig_minSize :: Lens.Lens' NodegroupScalingConfig (Prelude.Maybe Prelude.Natural)
nodegroupScalingConfig_minSize = Lens.lens (\NodegroupScalingConfig' {minSize} -> minSize) (\s@NodegroupScalingConfig' {} a -> s {minSize = a} :: NodegroupScalingConfig)

-- | The maximum number of nodes that the managed node group can scale out
-- to. For information about the maximum number that you can specify, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/service-quotas.html Amazon EKS service quotas>
-- in the /Amazon EKS User Guide/.
nodegroupScalingConfig_maxSize :: Lens.Lens' NodegroupScalingConfig (Prelude.Maybe Prelude.Natural)
nodegroupScalingConfig_maxSize = Lens.lens (\NodegroupScalingConfig' {maxSize} -> maxSize) (\s@NodegroupScalingConfig' {} a -> s {maxSize = a} :: NodegroupScalingConfig)

instance Core.FromJSON NodegroupScalingConfig where
  parseJSON =
    Core.withObject
      "NodegroupScalingConfig"
      ( \x ->
          NodegroupScalingConfig'
            Prelude.<$> (x Core..:? "desiredSize")
            Prelude.<*> (x Core..:? "minSize")
            Prelude.<*> (x Core..:? "maxSize")
      )

instance Prelude.Hashable NodegroupScalingConfig where
  hashWithSalt _salt NodegroupScalingConfig' {..} =
    _salt `Prelude.hashWithSalt` desiredSize
      `Prelude.hashWithSalt` minSize
      `Prelude.hashWithSalt` maxSize

instance Prelude.NFData NodegroupScalingConfig where
  rnf NodegroupScalingConfig' {..} =
    Prelude.rnf desiredSize
      `Prelude.seq` Prelude.rnf minSize
      `Prelude.seq` Prelude.rnf maxSize

instance Core.ToJSON NodegroupScalingConfig where
  toJSON NodegroupScalingConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("desiredSize" Core..=) Prelude.<$> desiredSize,
            ("minSize" Core..=) Prelude.<$> minSize,
            ("maxSize" Core..=) Prelude.<$> maxSize
          ]
      )
