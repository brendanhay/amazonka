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
-- Module      : Network.AWS.EMR.Types.InstanceFleetModifyConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceFleetModifyConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Configuration parameters for an instance fleet modification request.
--
-- The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions.
--
-- /See:/ 'newInstanceFleetModifyConfig' smart constructor.
data InstanceFleetModifyConfig = InstanceFleetModifyConfig'
  { -- | The target capacity of On-Demand units for the instance fleet. For more
    -- information see InstanceFleetConfig$TargetOnDemandCapacity.
    targetOnDemandCapacity :: Core.Maybe Core.Natural,
    -- | The target capacity of Spot units for the instance fleet. For more
    -- information, see InstanceFleetConfig$TargetSpotCapacity.
    targetSpotCapacity :: Core.Maybe Core.Natural,
    -- | A unique identifier for the instance fleet.
    instanceFleetId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceFleetModifyConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetOnDemandCapacity', 'instanceFleetModifyConfig_targetOnDemandCapacity' - The target capacity of On-Demand units for the instance fleet. For more
-- information see InstanceFleetConfig$TargetOnDemandCapacity.
--
-- 'targetSpotCapacity', 'instanceFleetModifyConfig_targetSpotCapacity' - The target capacity of Spot units for the instance fleet. For more
-- information, see InstanceFleetConfig$TargetSpotCapacity.
--
-- 'instanceFleetId', 'instanceFleetModifyConfig_instanceFleetId' - A unique identifier for the instance fleet.
newInstanceFleetModifyConfig ::
  -- | 'instanceFleetId'
  Core.Text ->
  InstanceFleetModifyConfig
newInstanceFleetModifyConfig pInstanceFleetId_ =
  InstanceFleetModifyConfig'
    { targetOnDemandCapacity =
        Core.Nothing,
      targetSpotCapacity = Core.Nothing,
      instanceFleetId = pInstanceFleetId_
    }

-- | The target capacity of On-Demand units for the instance fleet. For more
-- information see InstanceFleetConfig$TargetOnDemandCapacity.
instanceFleetModifyConfig_targetOnDemandCapacity :: Lens.Lens' InstanceFleetModifyConfig (Core.Maybe Core.Natural)
instanceFleetModifyConfig_targetOnDemandCapacity = Lens.lens (\InstanceFleetModifyConfig' {targetOnDemandCapacity} -> targetOnDemandCapacity) (\s@InstanceFleetModifyConfig' {} a -> s {targetOnDemandCapacity = a} :: InstanceFleetModifyConfig)

-- | The target capacity of Spot units for the instance fleet. For more
-- information, see InstanceFleetConfig$TargetSpotCapacity.
instanceFleetModifyConfig_targetSpotCapacity :: Lens.Lens' InstanceFleetModifyConfig (Core.Maybe Core.Natural)
instanceFleetModifyConfig_targetSpotCapacity = Lens.lens (\InstanceFleetModifyConfig' {targetSpotCapacity} -> targetSpotCapacity) (\s@InstanceFleetModifyConfig' {} a -> s {targetSpotCapacity = a} :: InstanceFleetModifyConfig)

-- | A unique identifier for the instance fleet.
instanceFleetModifyConfig_instanceFleetId :: Lens.Lens' InstanceFleetModifyConfig Core.Text
instanceFleetModifyConfig_instanceFleetId = Lens.lens (\InstanceFleetModifyConfig' {instanceFleetId} -> instanceFleetId) (\s@InstanceFleetModifyConfig' {} a -> s {instanceFleetId = a} :: InstanceFleetModifyConfig)

instance Core.Hashable InstanceFleetModifyConfig

instance Core.NFData InstanceFleetModifyConfig

instance Core.ToJSON InstanceFleetModifyConfig where
  toJSON InstanceFleetModifyConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TargetOnDemandCapacity" Core..=)
              Core.<$> targetOnDemandCapacity,
            ("TargetSpotCapacity" Core..=)
              Core.<$> targetSpotCapacity,
            Core.Just
              ("InstanceFleetId" Core..= instanceFleetId)
          ]
      )
