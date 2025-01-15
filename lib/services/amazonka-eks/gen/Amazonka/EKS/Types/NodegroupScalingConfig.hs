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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.NodegroupScalingConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object representing the scaling configuration details for the Auto
-- Scaling group that is associated with your node group. When creating a
-- node group, you must specify all or none of the properties. When
-- updating a node group, you can specify any or none of the properties.
--
-- /See:/ 'newNodegroupScalingConfig' smart constructor.
data NodegroupScalingConfig = NodegroupScalingConfig'
  { -- | The current number of nodes that the managed node group should maintain.
    --
    -- If you use Cluster Autoscaler, you shouldn\'t change the desiredSize
    -- value directly, as this can cause the Cluster Autoscaler to suddenly
    -- scale up or scale down.
    --
    -- Whenever this parameter changes, the number of worker nodes in the node
    -- group is updated to the specified size. If this parameter is given a
    -- value that is smaller than the current number of running worker nodes,
    -- the necessary number of worker nodes are terminated to match the given
    -- value. When using CloudFormation, no action occurs if you remove this
    -- parameter from your CFN template.
    --
    -- This parameter can be different from minSize in some cases, such as when
    -- starting with extra hosts for testing. This parameter can also be
    -- different when you want to start with an estimated number of needed
    -- hosts, but let Cluster Autoscaler reduce the number if there are too
    -- many. When Cluster Autoscaler is used, the desiredSize parameter is
    -- altered by Cluster Autoscaler (but can be out-of-date for short periods
    -- of time). Cluster Autoscaler doesn\'t scale a managed node group lower
    -- than minSize or higher than maxSize.
    desiredSize :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of nodes that the managed node group can scale out
    -- to. For information about the maximum number that you can specify, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/service-quotas.html Amazon EKS service quotas>
    -- in the /Amazon EKS User Guide/.
    maxSize :: Prelude.Maybe Prelude.Natural,
    -- | The minimum number of nodes that the managed node group can scale in to.
    minSize :: Prelude.Maybe Prelude.Natural
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
-- If you use Cluster Autoscaler, you shouldn\'t change the desiredSize
-- value directly, as this can cause the Cluster Autoscaler to suddenly
-- scale up or scale down.
--
-- Whenever this parameter changes, the number of worker nodes in the node
-- group is updated to the specified size. If this parameter is given a
-- value that is smaller than the current number of running worker nodes,
-- the necessary number of worker nodes are terminated to match the given
-- value. When using CloudFormation, no action occurs if you remove this
-- parameter from your CFN template.
--
-- This parameter can be different from minSize in some cases, such as when
-- starting with extra hosts for testing. This parameter can also be
-- different when you want to start with an estimated number of needed
-- hosts, but let Cluster Autoscaler reduce the number if there are too
-- many. When Cluster Autoscaler is used, the desiredSize parameter is
-- altered by Cluster Autoscaler (but can be out-of-date for short periods
-- of time). Cluster Autoscaler doesn\'t scale a managed node group lower
-- than minSize or higher than maxSize.
--
-- 'maxSize', 'nodegroupScalingConfig_maxSize' - The maximum number of nodes that the managed node group can scale out
-- to. For information about the maximum number that you can specify, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/service-quotas.html Amazon EKS service quotas>
-- in the /Amazon EKS User Guide/.
--
-- 'minSize', 'nodegroupScalingConfig_minSize' - The minimum number of nodes that the managed node group can scale in to.
newNodegroupScalingConfig ::
  NodegroupScalingConfig
newNodegroupScalingConfig =
  NodegroupScalingConfig'
    { desiredSize =
        Prelude.Nothing,
      maxSize = Prelude.Nothing,
      minSize = Prelude.Nothing
    }

-- | The current number of nodes that the managed node group should maintain.
--
-- If you use Cluster Autoscaler, you shouldn\'t change the desiredSize
-- value directly, as this can cause the Cluster Autoscaler to suddenly
-- scale up or scale down.
--
-- Whenever this parameter changes, the number of worker nodes in the node
-- group is updated to the specified size. If this parameter is given a
-- value that is smaller than the current number of running worker nodes,
-- the necessary number of worker nodes are terminated to match the given
-- value. When using CloudFormation, no action occurs if you remove this
-- parameter from your CFN template.
--
-- This parameter can be different from minSize in some cases, such as when
-- starting with extra hosts for testing. This parameter can also be
-- different when you want to start with an estimated number of needed
-- hosts, but let Cluster Autoscaler reduce the number if there are too
-- many. When Cluster Autoscaler is used, the desiredSize parameter is
-- altered by Cluster Autoscaler (but can be out-of-date for short periods
-- of time). Cluster Autoscaler doesn\'t scale a managed node group lower
-- than minSize or higher than maxSize.
nodegroupScalingConfig_desiredSize :: Lens.Lens' NodegroupScalingConfig (Prelude.Maybe Prelude.Natural)
nodegroupScalingConfig_desiredSize = Lens.lens (\NodegroupScalingConfig' {desiredSize} -> desiredSize) (\s@NodegroupScalingConfig' {} a -> s {desiredSize = a} :: NodegroupScalingConfig)

-- | The maximum number of nodes that the managed node group can scale out
-- to. For information about the maximum number that you can specify, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/service-quotas.html Amazon EKS service quotas>
-- in the /Amazon EKS User Guide/.
nodegroupScalingConfig_maxSize :: Lens.Lens' NodegroupScalingConfig (Prelude.Maybe Prelude.Natural)
nodegroupScalingConfig_maxSize = Lens.lens (\NodegroupScalingConfig' {maxSize} -> maxSize) (\s@NodegroupScalingConfig' {} a -> s {maxSize = a} :: NodegroupScalingConfig)

-- | The minimum number of nodes that the managed node group can scale in to.
nodegroupScalingConfig_minSize :: Lens.Lens' NodegroupScalingConfig (Prelude.Maybe Prelude.Natural)
nodegroupScalingConfig_minSize = Lens.lens (\NodegroupScalingConfig' {minSize} -> minSize) (\s@NodegroupScalingConfig' {} a -> s {minSize = a} :: NodegroupScalingConfig)

instance Data.FromJSON NodegroupScalingConfig where
  parseJSON =
    Data.withObject
      "NodegroupScalingConfig"
      ( \x ->
          NodegroupScalingConfig'
            Prelude.<$> (x Data..:? "desiredSize")
            Prelude.<*> (x Data..:? "maxSize")
            Prelude.<*> (x Data..:? "minSize")
      )

instance Prelude.Hashable NodegroupScalingConfig where
  hashWithSalt _salt NodegroupScalingConfig' {..} =
    _salt
      `Prelude.hashWithSalt` desiredSize
      `Prelude.hashWithSalt` maxSize
      `Prelude.hashWithSalt` minSize

instance Prelude.NFData NodegroupScalingConfig where
  rnf NodegroupScalingConfig' {..} =
    Prelude.rnf desiredSize `Prelude.seq`
      Prelude.rnf maxSize `Prelude.seq`
        Prelude.rnf minSize

instance Data.ToJSON NodegroupScalingConfig where
  toJSON NodegroupScalingConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("desiredSize" Data..=) Prelude.<$> desiredSize,
            ("maxSize" Data..=) Prelude.<$> maxSize,
            ("minSize" Data..=) Prelude.<$> minSize
          ]
      )
