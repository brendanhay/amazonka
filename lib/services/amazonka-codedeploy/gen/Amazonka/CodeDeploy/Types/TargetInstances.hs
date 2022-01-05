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
-- Module      : Amazonka.CodeDeploy.Types.TargetInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.TargetInstances where

import Amazonka.CodeDeploy.Types.EC2TagFilter
import Amazonka.CodeDeploy.Types.EC2TagSet
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the instances to be used in the replacement
-- environment in a blue\/green deployment.
--
-- /See:/ 'newTargetInstances' smart constructor.
data TargetInstances = TargetInstances'
  { -- | Information about the groups of EC2 instance tags that an instance must
    -- be identified by in order for it to be included in the replacement
    -- environment for a blue\/green deployment. Cannot be used in the same
    -- call as @tagFilters@.
    ec2TagSet :: Prelude.Maybe EC2TagSet,
    -- | The tag filter key, type, and value used to identify Amazon EC2
    -- instances in a replacement environment for a blue\/green deployment.
    -- Cannot be used in the same call as @ec2TagSet@.
    tagFilters :: Prelude.Maybe [EC2TagFilter],
    -- | The names of one or more Auto Scaling groups to identify a replacement
    -- environment for a blue\/green deployment.
    autoScalingGroups :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ec2TagSet', 'targetInstances_ec2TagSet' - Information about the groups of EC2 instance tags that an instance must
-- be identified by in order for it to be included in the replacement
-- environment for a blue\/green deployment. Cannot be used in the same
-- call as @tagFilters@.
--
-- 'tagFilters', 'targetInstances_tagFilters' - The tag filter key, type, and value used to identify Amazon EC2
-- instances in a replacement environment for a blue\/green deployment.
-- Cannot be used in the same call as @ec2TagSet@.
--
-- 'autoScalingGroups', 'targetInstances_autoScalingGroups' - The names of one or more Auto Scaling groups to identify a replacement
-- environment for a blue\/green deployment.
newTargetInstances ::
  TargetInstances
newTargetInstances =
  TargetInstances'
    { ec2TagSet = Prelude.Nothing,
      tagFilters = Prelude.Nothing,
      autoScalingGroups = Prelude.Nothing
    }

-- | Information about the groups of EC2 instance tags that an instance must
-- be identified by in order for it to be included in the replacement
-- environment for a blue\/green deployment. Cannot be used in the same
-- call as @tagFilters@.
targetInstances_ec2TagSet :: Lens.Lens' TargetInstances (Prelude.Maybe EC2TagSet)
targetInstances_ec2TagSet = Lens.lens (\TargetInstances' {ec2TagSet} -> ec2TagSet) (\s@TargetInstances' {} a -> s {ec2TagSet = a} :: TargetInstances)

-- | The tag filter key, type, and value used to identify Amazon EC2
-- instances in a replacement environment for a blue\/green deployment.
-- Cannot be used in the same call as @ec2TagSet@.
targetInstances_tagFilters :: Lens.Lens' TargetInstances (Prelude.Maybe [EC2TagFilter])
targetInstances_tagFilters = Lens.lens (\TargetInstances' {tagFilters} -> tagFilters) (\s@TargetInstances' {} a -> s {tagFilters = a} :: TargetInstances) Prelude.. Lens.mapping Lens.coerced

-- | The names of one or more Auto Scaling groups to identify a replacement
-- environment for a blue\/green deployment.
targetInstances_autoScalingGroups :: Lens.Lens' TargetInstances (Prelude.Maybe [Prelude.Text])
targetInstances_autoScalingGroups = Lens.lens (\TargetInstances' {autoScalingGroups} -> autoScalingGroups) (\s@TargetInstances' {} a -> s {autoScalingGroups = a} :: TargetInstances) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON TargetInstances where
  parseJSON =
    Core.withObject
      "TargetInstances"
      ( \x ->
          TargetInstances'
            Prelude.<$> (x Core..:? "ec2TagSet")
            Prelude.<*> (x Core..:? "tagFilters" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "autoScalingGroups"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable TargetInstances where
  hashWithSalt _salt TargetInstances' {..} =
    _salt `Prelude.hashWithSalt` ec2TagSet
      `Prelude.hashWithSalt` tagFilters
      `Prelude.hashWithSalt` autoScalingGroups

instance Prelude.NFData TargetInstances where
  rnf TargetInstances' {..} =
    Prelude.rnf ec2TagSet
      `Prelude.seq` Prelude.rnf tagFilters
      `Prelude.seq` Prelude.rnf autoScalingGroups

instance Core.ToJSON TargetInstances where
  toJSON TargetInstances' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ec2TagSet" Core..=) Prelude.<$> ec2TagSet,
            ("tagFilters" Core..=) Prelude.<$> tagFilters,
            ("autoScalingGroups" Core..=)
              Prelude.<$> autoScalingGroups
          ]
      )
