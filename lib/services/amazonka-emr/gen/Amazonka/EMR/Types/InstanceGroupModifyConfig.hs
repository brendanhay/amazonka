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
-- Module      : Amazonka.EMR.Types.InstanceGroupModifyConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.InstanceGroupModifyConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types.Configuration
import Amazonka.EMR.Types.ReconfigurationType
import Amazonka.EMR.Types.ShrinkPolicy
import qualified Amazonka.Prelude as Prelude

-- | Modify the size or configurations of an instance group.
--
-- /See:/ 'newInstanceGroupModifyConfig' smart constructor.
data InstanceGroupModifyConfig = InstanceGroupModifyConfig'
  { -- | A list of new or modified configurations to apply for an instance group.
    configurations :: Prelude.Maybe [Configuration],
    -- | The EC2 InstanceIds to terminate. After you terminate the instances, the
    -- instance group will not return to its original requested size.
    eC2InstanceIdsToTerminate :: Prelude.Maybe [Prelude.Text],
    -- | Target size for the instance group.
    instanceCount :: Prelude.Maybe Prelude.Int,
    -- | Type of reconfiguration requested. Valid values are MERGE and OVERWRITE.
    reconfigurationType :: Prelude.Maybe ReconfigurationType,
    -- | Policy for customizing shrink operations.
    shrinkPolicy :: Prelude.Maybe ShrinkPolicy,
    -- | Unique ID of the instance group to modify.
    instanceGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'eC2InstanceIdsToTerminate', 'instanceGroupModifyConfig_eC2InstanceIdsToTerminate' - The EC2 InstanceIds to terminate. After you terminate the instances, the
-- instance group will not return to its original requested size.
--
-- 'instanceCount', 'instanceGroupModifyConfig_instanceCount' - Target size for the instance group.
--
-- 'reconfigurationType', 'instanceGroupModifyConfig_reconfigurationType' - Type of reconfiguration requested. Valid values are MERGE and OVERWRITE.
--
-- 'shrinkPolicy', 'instanceGroupModifyConfig_shrinkPolicy' - Policy for customizing shrink operations.
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
      eC2InstanceIdsToTerminate = Prelude.Nothing,
      instanceCount = Prelude.Nothing,
      reconfigurationType = Prelude.Nothing,
      shrinkPolicy = Prelude.Nothing,
      instanceGroupId = pInstanceGroupId_
    }

-- | A list of new or modified configurations to apply for an instance group.
instanceGroupModifyConfig_configurations :: Lens.Lens' InstanceGroupModifyConfig (Prelude.Maybe [Configuration])
instanceGroupModifyConfig_configurations = Lens.lens (\InstanceGroupModifyConfig' {configurations} -> configurations) (\s@InstanceGroupModifyConfig' {} a -> s {configurations = a} :: InstanceGroupModifyConfig) Prelude.. Lens.mapping Lens.coerced

-- | The EC2 InstanceIds to terminate. After you terminate the instances, the
-- instance group will not return to its original requested size.
instanceGroupModifyConfig_eC2InstanceIdsToTerminate :: Lens.Lens' InstanceGroupModifyConfig (Prelude.Maybe [Prelude.Text])
instanceGroupModifyConfig_eC2InstanceIdsToTerminate = Lens.lens (\InstanceGroupModifyConfig' {eC2InstanceIdsToTerminate} -> eC2InstanceIdsToTerminate) (\s@InstanceGroupModifyConfig' {} a -> s {eC2InstanceIdsToTerminate = a} :: InstanceGroupModifyConfig) Prelude.. Lens.mapping Lens.coerced

-- | Target size for the instance group.
instanceGroupModifyConfig_instanceCount :: Lens.Lens' InstanceGroupModifyConfig (Prelude.Maybe Prelude.Int)
instanceGroupModifyConfig_instanceCount = Lens.lens (\InstanceGroupModifyConfig' {instanceCount} -> instanceCount) (\s@InstanceGroupModifyConfig' {} a -> s {instanceCount = a} :: InstanceGroupModifyConfig)

-- | Type of reconfiguration requested. Valid values are MERGE and OVERWRITE.
instanceGroupModifyConfig_reconfigurationType :: Lens.Lens' InstanceGroupModifyConfig (Prelude.Maybe ReconfigurationType)
instanceGroupModifyConfig_reconfigurationType = Lens.lens (\InstanceGroupModifyConfig' {reconfigurationType} -> reconfigurationType) (\s@InstanceGroupModifyConfig' {} a -> s {reconfigurationType = a} :: InstanceGroupModifyConfig)

-- | Policy for customizing shrink operations.
instanceGroupModifyConfig_shrinkPolicy :: Lens.Lens' InstanceGroupModifyConfig (Prelude.Maybe ShrinkPolicy)
instanceGroupModifyConfig_shrinkPolicy = Lens.lens (\InstanceGroupModifyConfig' {shrinkPolicy} -> shrinkPolicy) (\s@InstanceGroupModifyConfig' {} a -> s {shrinkPolicy = a} :: InstanceGroupModifyConfig)

-- | Unique ID of the instance group to modify.
instanceGroupModifyConfig_instanceGroupId :: Lens.Lens' InstanceGroupModifyConfig Prelude.Text
instanceGroupModifyConfig_instanceGroupId = Lens.lens (\InstanceGroupModifyConfig' {instanceGroupId} -> instanceGroupId) (\s@InstanceGroupModifyConfig' {} a -> s {instanceGroupId = a} :: InstanceGroupModifyConfig)

instance Prelude.Hashable InstanceGroupModifyConfig where
  hashWithSalt _salt InstanceGroupModifyConfig' {..} =
    _salt `Prelude.hashWithSalt` configurations
      `Prelude.hashWithSalt` eC2InstanceIdsToTerminate
      `Prelude.hashWithSalt` instanceCount
      `Prelude.hashWithSalt` reconfigurationType
      `Prelude.hashWithSalt` shrinkPolicy
      `Prelude.hashWithSalt` instanceGroupId

instance Prelude.NFData InstanceGroupModifyConfig where
  rnf InstanceGroupModifyConfig' {..} =
    Prelude.rnf configurations
      `Prelude.seq` Prelude.rnf eC2InstanceIdsToTerminate
      `Prelude.seq` Prelude.rnf instanceCount
      `Prelude.seq` Prelude.rnf reconfigurationType
      `Prelude.seq` Prelude.rnf shrinkPolicy
      `Prelude.seq` Prelude.rnf instanceGroupId

instance Data.ToJSON InstanceGroupModifyConfig where
  toJSON InstanceGroupModifyConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Configurations" Data..=)
              Prelude.<$> configurations,
            ("EC2InstanceIdsToTerminate" Data..=)
              Prelude.<$> eC2InstanceIdsToTerminate,
            ("InstanceCount" Data..=) Prelude.<$> instanceCount,
            ("ReconfigurationType" Data..=)
              Prelude.<$> reconfigurationType,
            ("ShrinkPolicy" Data..=) Prelude.<$> shrinkPolicy,
            Prelude.Just
              ("InstanceGroupId" Data..= instanceGroupId)
          ]
      )
