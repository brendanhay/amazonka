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
-- Module      : Network.AWS.CodeDeploy.Types.TargetInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TargetInstances where

import Network.AWS.CodeDeploy.Types.EC2TagFilter
import Network.AWS.CodeDeploy.Types.EC2TagSet
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about the instances to be used in the replacement
-- environment in a blue\/green deployment.
--
-- /See:/ 'newTargetInstances' smart constructor.
data TargetInstances = TargetInstances'
  { -- | The tag filter key, type, and value used to identify Amazon EC2
    -- instances in a replacement environment for a blue\/green deployment.
    -- Cannot be used in the same call as @ec2TagSet@.
    tagFilters :: Core.Maybe [EC2TagFilter],
    -- | Information about the groups of EC2 instance tags that an instance must
    -- be identified by in order for it to be included in the replacement
    -- environment for a blue\/green deployment. Cannot be used in the same
    -- call as @tagFilters@.
    ec2TagSet :: Core.Maybe EC2TagSet,
    -- | The names of one or more Auto Scaling groups to identify a replacement
    -- environment for a blue\/green deployment.
    autoScalingGroups :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TargetInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagFilters', 'targetInstances_tagFilters' - The tag filter key, type, and value used to identify Amazon EC2
-- instances in a replacement environment for a blue\/green deployment.
-- Cannot be used in the same call as @ec2TagSet@.
--
-- 'ec2TagSet', 'targetInstances_ec2TagSet' - Information about the groups of EC2 instance tags that an instance must
-- be identified by in order for it to be included in the replacement
-- environment for a blue\/green deployment. Cannot be used in the same
-- call as @tagFilters@.
--
-- 'autoScalingGroups', 'targetInstances_autoScalingGroups' - The names of one or more Auto Scaling groups to identify a replacement
-- environment for a blue\/green deployment.
newTargetInstances ::
  TargetInstances
newTargetInstances =
  TargetInstances'
    { tagFilters = Core.Nothing,
      ec2TagSet = Core.Nothing,
      autoScalingGroups = Core.Nothing
    }

-- | The tag filter key, type, and value used to identify Amazon EC2
-- instances in a replacement environment for a blue\/green deployment.
-- Cannot be used in the same call as @ec2TagSet@.
targetInstances_tagFilters :: Lens.Lens' TargetInstances (Core.Maybe [EC2TagFilter])
targetInstances_tagFilters = Lens.lens (\TargetInstances' {tagFilters} -> tagFilters) (\s@TargetInstances' {} a -> s {tagFilters = a} :: TargetInstances) Core.. Lens.mapping Lens._Coerce

-- | Information about the groups of EC2 instance tags that an instance must
-- be identified by in order for it to be included in the replacement
-- environment for a blue\/green deployment. Cannot be used in the same
-- call as @tagFilters@.
targetInstances_ec2TagSet :: Lens.Lens' TargetInstances (Core.Maybe EC2TagSet)
targetInstances_ec2TagSet = Lens.lens (\TargetInstances' {ec2TagSet} -> ec2TagSet) (\s@TargetInstances' {} a -> s {ec2TagSet = a} :: TargetInstances)

-- | The names of one or more Auto Scaling groups to identify a replacement
-- environment for a blue\/green deployment.
targetInstances_autoScalingGroups :: Lens.Lens' TargetInstances (Core.Maybe [Core.Text])
targetInstances_autoScalingGroups = Lens.lens (\TargetInstances' {autoScalingGroups} -> autoScalingGroups) (\s@TargetInstances' {} a -> s {autoScalingGroups = a} :: TargetInstances) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON TargetInstances where
  parseJSON =
    Core.withObject
      "TargetInstances"
      ( \x ->
          TargetInstances'
            Core.<$> (x Core..:? "tagFilters" Core..!= Core.mempty)
            Core.<*> (x Core..:? "ec2TagSet")
            Core.<*> ( x Core..:? "autoScalingGroups"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable TargetInstances

instance Core.NFData TargetInstances

instance Core.ToJSON TargetInstances where
  toJSON TargetInstances' {..} =
    Core.object
      ( Core.catMaybes
          [ ("tagFilters" Core..=) Core.<$> tagFilters,
            ("ec2TagSet" Core..=) Core.<$> ec2TagSet,
            ("autoScalingGroups" Core..=)
              Core.<$> autoScalingGroups
          ]
      )
