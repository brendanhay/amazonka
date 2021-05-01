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
-- Module      : Network.AWS.EMR.Types.InstanceGroupModifyConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceGroupModifyConfig where

import Network.AWS.EMR.Types.Configuration
import Network.AWS.EMR.Types.ShrinkPolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Modify the size or configurations of an instance group.
--
-- /See:/ 'newInstanceGroupModifyConfig' smart constructor.
data InstanceGroupModifyConfig = InstanceGroupModifyConfig'
  { -- | A list of new or modified configurations to apply for an instance group.
    configurations :: Prelude.Maybe [Configuration],
    -- | Policy for customizing shrink operations.
    shrinkPolicy :: Prelude.Maybe ShrinkPolicy,
    -- | The EC2 InstanceIds to terminate. After you terminate the instances, the
    -- instance group will not return to its original requested size.
    eC2InstanceIdsToTerminate :: Prelude.Maybe [Prelude.Text],
    -- | Target size for the instance group.
    instanceCount :: Prelude.Maybe Prelude.Int,
    -- | Unique ID of the instance group to modify.
    instanceGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  InstanceGroupModifyConfig
newInstanceGroupModifyConfig pInstanceGroupId_ =
  InstanceGroupModifyConfig'
    { configurations =
        Prelude.Nothing,
      shrinkPolicy = Prelude.Nothing,
      eC2InstanceIdsToTerminate = Prelude.Nothing,
      instanceCount = Prelude.Nothing,
      instanceGroupId = pInstanceGroupId_
    }

-- | A list of new or modified configurations to apply for an instance group.
instanceGroupModifyConfig_configurations :: Lens.Lens' InstanceGroupModifyConfig (Prelude.Maybe [Configuration])
instanceGroupModifyConfig_configurations = Lens.lens (\InstanceGroupModifyConfig' {configurations} -> configurations) (\s@InstanceGroupModifyConfig' {} a -> s {configurations = a} :: InstanceGroupModifyConfig) Prelude.. Lens.mapping Prelude._Coerce

-- | Policy for customizing shrink operations.
instanceGroupModifyConfig_shrinkPolicy :: Lens.Lens' InstanceGroupModifyConfig (Prelude.Maybe ShrinkPolicy)
instanceGroupModifyConfig_shrinkPolicy = Lens.lens (\InstanceGroupModifyConfig' {shrinkPolicy} -> shrinkPolicy) (\s@InstanceGroupModifyConfig' {} a -> s {shrinkPolicy = a} :: InstanceGroupModifyConfig)

-- | The EC2 InstanceIds to terminate. After you terminate the instances, the
-- instance group will not return to its original requested size.
instanceGroupModifyConfig_eC2InstanceIdsToTerminate :: Lens.Lens' InstanceGroupModifyConfig (Prelude.Maybe [Prelude.Text])
instanceGroupModifyConfig_eC2InstanceIdsToTerminate = Lens.lens (\InstanceGroupModifyConfig' {eC2InstanceIdsToTerminate} -> eC2InstanceIdsToTerminate) (\s@InstanceGroupModifyConfig' {} a -> s {eC2InstanceIdsToTerminate = a} :: InstanceGroupModifyConfig) Prelude.. Lens.mapping Prelude._Coerce

-- | Target size for the instance group.
instanceGroupModifyConfig_instanceCount :: Lens.Lens' InstanceGroupModifyConfig (Prelude.Maybe Prelude.Int)
instanceGroupModifyConfig_instanceCount = Lens.lens (\InstanceGroupModifyConfig' {instanceCount} -> instanceCount) (\s@InstanceGroupModifyConfig' {} a -> s {instanceCount = a} :: InstanceGroupModifyConfig)

-- | Unique ID of the instance group to modify.
instanceGroupModifyConfig_instanceGroupId :: Lens.Lens' InstanceGroupModifyConfig Prelude.Text
instanceGroupModifyConfig_instanceGroupId = Lens.lens (\InstanceGroupModifyConfig' {instanceGroupId} -> instanceGroupId) (\s@InstanceGroupModifyConfig' {} a -> s {instanceGroupId = a} :: InstanceGroupModifyConfig)

instance Prelude.Hashable InstanceGroupModifyConfig

instance Prelude.NFData InstanceGroupModifyConfig

instance Prelude.ToJSON InstanceGroupModifyConfig where
  toJSON InstanceGroupModifyConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Configurations" Prelude..=)
              Prelude.<$> configurations,
            ("ShrinkPolicy" Prelude..=) Prelude.<$> shrinkPolicy,
            ("EC2InstanceIdsToTerminate" Prelude..=)
              Prelude.<$> eC2InstanceIdsToTerminate,
            ("InstanceCount" Prelude..=)
              Prelude.<$> instanceCount,
            Prelude.Just
              ("InstanceGroupId" Prelude..= instanceGroupId)
          ]
      )
