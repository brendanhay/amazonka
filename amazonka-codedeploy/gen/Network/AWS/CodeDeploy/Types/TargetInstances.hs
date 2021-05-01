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
-- Module      : Network.AWS.CodeDeploy.Types.TargetInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TargetInstances where

import Network.AWS.CodeDeploy.Types.EC2TagFilter
import Network.AWS.CodeDeploy.Types.EC2TagSet
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the instances to be used in the replacement
-- environment in a blue\/green deployment.
--
-- /See:/ 'newTargetInstances' smart constructor.
data TargetInstances = TargetInstances'
  { -- | The tag filter key, type, and value used to identify Amazon EC2
    -- instances in a replacement environment for a blue\/green deployment.
    -- Cannot be used in the same call as @ec2TagSet@.
    tagFilters :: Prelude.Maybe [EC2TagFilter],
    -- | Information about the groups of EC2 instance tags that an instance must
    -- be identified by in order for it to be included in the replacement
    -- environment for a blue\/green deployment. Cannot be used in the same
    -- call as @tagFilters@.
    ec2TagSet :: Prelude.Maybe EC2TagSet,
    -- | The names of one or more Auto Scaling groups to identify a replacement
    -- environment for a blue\/green deployment.
    autoScalingGroups :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { tagFilters = Prelude.Nothing,
      ec2TagSet = Prelude.Nothing,
      autoScalingGroups = Prelude.Nothing
    }

-- | The tag filter key, type, and value used to identify Amazon EC2
-- instances in a replacement environment for a blue\/green deployment.
-- Cannot be used in the same call as @ec2TagSet@.
targetInstances_tagFilters :: Lens.Lens' TargetInstances (Prelude.Maybe [EC2TagFilter])
targetInstances_tagFilters = Lens.lens (\TargetInstances' {tagFilters} -> tagFilters) (\s@TargetInstances' {} a -> s {tagFilters = a} :: TargetInstances) Prelude.. Lens.mapping Prelude._Coerce

-- | Information about the groups of EC2 instance tags that an instance must
-- be identified by in order for it to be included in the replacement
-- environment for a blue\/green deployment. Cannot be used in the same
-- call as @tagFilters@.
targetInstances_ec2TagSet :: Lens.Lens' TargetInstances (Prelude.Maybe EC2TagSet)
targetInstances_ec2TagSet = Lens.lens (\TargetInstances' {ec2TagSet} -> ec2TagSet) (\s@TargetInstances' {} a -> s {ec2TagSet = a} :: TargetInstances)

-- | The names of one or more Auto Scaling groups to identify a replacement
-- environment for a blue\/green deployment.
targetInstances_autoScalingGroups :: Lens.Lens' TargetInstances (Prelude.Maybe [Prelude.Text])
targetInstances_autoScalingGroups = Lens.lens (\TargetInstances' {autoScalingGroups} -> autoScalingGroups) (\s@TargetInstances' {} a -> s {autoScalingGroups = a} :: TargetInstances) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON TargetInstances where
  parseJSON =
    Prelude.withObject
      "TargetInstances"
      ( \x ->
          TargetInstances'
            Prelude.<$> ( x Prelude..:? "tagFilters"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "ec2TagSet")
            Prelude.<*> ( x Prelude..:? "autoScalingGroups"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable TargetInstances

instance Prelude.NFData TargetInstances

instance Prelude.ToJSON TargetInstances where
  toJSON TargetInstances' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("tagFilters" Prelude..=) Prelude.<$> tagFilters,
            ("ec2TagSet" Prelude..=) Prelude.<$> ec2TagSet,
            ("autoScalingGroups" Prelude..=)
              Prelude.<$> autoScalingGroups
          ]
      )
