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
-- Module      : Network.AWS.EMR.Types.InstanceGroupModifyConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceGroupModifyConfig where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types.Configuration
import Network.AWS.EMR.Types.ShrinkPolicy
import qualified Network.AWS.Lens as Lens

-- | Modify the size or configurations of an instance group.
--
-- /See:/ 'newInstanceGroupModifyConfig' smart constructor.
data InstanceGroupModifyConfig = InstanceGroupModifyConfig'
  { -- | A list of new or modified configurations to apply for an instance group.
    configurations :: Core.Maybe [Configuration],
    -- | Policy for customizing shrink operations.
    shrinkPolicy :: Core.Maybe ShrinkPolicy,
    -- | The EC2 InstanceIds to terminate. After you terminate the instances, the
    -- instance group will not return to its original requested size.
    eC2InstanceIdsToTerminate :: Core.Maybe [Core.Text],
    -- | Target size for the instance group.
    instanceCount :: Core.Maybe Core.Int,
    -- | Unique ID of the instance group to modify.
    instanceGroupId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceGroupModifyConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurations', 'instanceGroupModifyConfig_configurations' - A list of new or modified configurations to apply for an instance group.
--
-- 'shrinkPolicy', 'instanceGroupModifyConfig_shrinkPolicy' - Policy for customizing shrink operations.
--
-- 'eC2InstanceIdsToTerminate', 'instanceGroupModifyConfig_eC2InstanceIdsToTerminate' - The EC2 InstanceIds to terminate. After you terminate the instances, the
-- instance group will not return to its original requested size.
--
-- 'instanceCount', 'instanceGroupModifyConfig_instanceCount' - Target size for the instance group.
--
-- 'instanceGroupId', 'instanceGroupModifyConfig_instanceGroupId' - Unique ID of the instance group to modify.
newInstanceGroupModifyConfig ::
  -- | 'instanceGroupId'
  Core.Text ->
  InstanceGroupModifyConfig
newInstanceGroupModifyConfig pInstanceGroupId_ =
  InstanceGroupModifyConfig'
    { configurations =
        Core.Nothing,
      shrinkPolicy = Core.Nothing,
      eC2InstanceIdsToTerminate = Core.Nothing,
      instanceCount = Core.Nothing,
      instanceGroupId = pInstanceGroupId_
    }

-- | A list of new or modified configurations to apply for an instance group.
instanceGroupModifyConfig_configurations :: Lens.Lens' InstanceGroupModifyConfig (Core.Maybe [Configuration])
instanceGroupModifyConfig_configurations = Lens.lens (\InstanceGroupModifyConfig' {configurations} -> configurations) (\s@InstanceGroupModifyConfig' {} a -> s {configurations = a} :: InstanceGroupModifyConfig) Core.. Lens.mapping Lens._Coerce

-- | Policy for customizing shrink operations.
instanceGroupModifyConfig_shrinkPolicy :: Lens.Lens' InstanceGroupModifyConfig (Core.Maybe ShrinkPolicy)
instanceGroupModifyConfig_shrinkPolicy = Lens.lens (\InstanceGroupModifyConfig' {shrinkPolicy} -> shrinkPolicy) (\s@InstanceGroupModifyConfig' {} a -> s {shrinkPolicy = a} :: InstanceGroupModifyConfig)

-- | The EC2 InstanceIds to terminate. After you terminate the instances, the
-- instance group will not return to its original requested size.
instanceGroupModifyConfig_eC2InstanceIdsToTerminate :: Lens.Lens' InstanceGroupModifyConfig (Core.Maybe [Core.Text])
instanceGroupModifyConfig_eC2InstanceIdsToTerminate = Lens.lens (\InstanceGroupModifyConfig' {eC2InstanceIdsToTerminate} -> eC2InstanceIdsToTerminate) (\s@InstanceGroupModifyConfig' {} a -> s {eC2InstanceIdsToTerminate = a} :: InstanceGroupModifyConfig) Core.. Lens.mapping Lens._Coerce

-- | Target size for the instance group.
instanceGroupModifyConfig_instanceCount :: Lens.Lens' InstanceGroupModifyConfig (Core.Maybe Core.Int)
instanceGroupModifyConfig_instanceCount = Lens.lens (\InstanceGroupModifyConfig' {instanceCount} -> instanceCount) (\s@InstanceGroupModifyConfig' {} a -> s {instanceCount = a} :: InstanceGroupModifyConfig)

-- | Unique ID of the instance group to modify.
instanceGroupModifyConfig_instanceGroupId :: Lens.Lens' InstanceGroupModifyConfig Core.Text
instanceGroupModifyConfig_instanceGroupId = Lens.lens (\InstanceGroupModifyConfig' {instanceGroupId} -> instanceGroupId) (\s@InstanceGroupModifyConfig' {} a -> s {instanceGroupId = a} :: InstanceGroupModifyConfig)

instance Core.Hashable InstanceGroupModifyConfig

instance Core.NFData InstanceGroupModifyConfig

instance Core.ToJSON InstanceGroupModifyConfig where
  toJSON InstanceGroupModifyConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Configurations" Core..=) Core.<$> configurations,
            ("ShrinkPolicy" Core..=) Core.<$> shrinkPolicy,
            ("EC2InstanceIdsToTerminate" Core..=)
              Core.<$> eC2InstanceIdsToTerminate,
            ("InstanceCount" Core..=) Core.<$> instanceCount,
            Core.Just
              ("InstanceGroupId" Core..= instanceGroupId)
          ]
      )
