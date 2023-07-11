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
-- Module      : Amazonka.EMR.Types.InstanceFleetModifyConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.InstanceFleetModifyConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration parameters for an instance fleet modification request.
--
-- The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions.
--
-- /See:/ 'newInstanceFleetModifyConfig' smart constructor.
data InstanceFleetModifyConfig = InstanceFleetModifyConfig'
  { -- | The target capacity of On-Demand units for the instance fleet. For more
    -- information see InstanceFleetConfig$TargetOnDemandCapacity.
    targetOnDemandCapacity :: Prelude.Maybe Prelude.Natural,
    -- | The target capacity of Spot units for the instance fleet. For more
    -- information, see InstanceFleetConfig$TargetSpotCapacity.
    targetSpotCapacity :: Prelude.Maybe Prelude.Natural,
    -- | A unique identifier for the instance fleet.
    instanceFleetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  InstanceFleetModifyConfig
newInstanceFleetModifyConfig pInstanceFleetId_ =
  InstanceFleetModifyConfig'
    { targetOnDemandCapacity =
        Prelude.Nothing,
      targetSpotCapacity = Prelude.Nothing,
      instanceFleetId = pInstanceFleetId_
    }

-- | The target capacity of On-Demand units for the instance fleet. For more
-- information see InstanceFleetConfig$TargetOnDemandCapacity.
instanceFleetModifyConfig_targetOnDemandCapacity :: Lens.Lens' InstanceFleetModifyConfig (Prelude.Maybe Prelude.Natural)
instanceFleetModifyConfig_targetOnDemandCapacity = Lens.lens (\InstanceFleetModifyConfig' {targetOnDemandCapacity} -> targetOnDemandCapacity) (\s@InstanceFleetModifyConfig' {} a -> s {targetOnDemandCapacity = a} :: InstanceFleetModifyConfig)

-- | The target capacity of Spot units for the instance fleet. For more
-- information, see InstanceFleetConfig$TargetSpotCapacity.
instanceFleetModifyConfig_targetSpotCapacity :: Lens.Lens' InstanceFleetModifyConfig (Prelude.Maybe Prelude.Natural)
instanceFleetModifyConfig_targetSpotCapacity = Lens.lens (\InstanceFleetModifyConfig' {targetSpotCapacity} -> targetSpotCapacity) (\s@InstanceFleetModifyConfig' {} a -> s {targetSpotCapacity = a} :: InstanceFleetModifyConfig)

-- | A unique identifier for the instance fleet.
instanceFleetModifyConfig_instanceFleetId :: Lens.Lens' InstanceFleetModifyConfig Prelude.Text
instanceFleetModifyConfig_instanceFleetId = Lens.lens (\InstanceFleetModifyConfig' {instanceFleetId} -> instanceFleetId) (\s@InstanceFleetModifyConfig' {} a -> s {instanceFleetId = a} :: InstanceFleetModifyConfig)

instance Prelude.Hashable InstanceFleetModifyConfig where
  hashWithSalt _salt InstanceFleetModifyConfig' {..} =
    _salt
      `Prelude.hashWithSalt` targetOnDemandCapacity
      `Prelude.hashWithSalt` targetSpotCapacity
      `Prelude.hashWithSalt` instanceFleetId

instance Prelude.NFData InstanceFleetModifyConfig where
  rnf InstanceFleetModifyConfig' {..} =
    Prelude.rnf targetOnDemandCapacity
      `Prelude.seq` Prelude.rnf targetSpotCapacity
      `Prelude.seq` Prelude.rnf instanceFleetId

instance Data.ToJSON InstanceFleetModifyConfig where
  toJSON InstanceFleetModifyConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TargetOnDemandCapacity" Data..=)
              Prelude.<$> targetOnDemandCapacity,
            ("TargetSpotCapacity" Data..=)
              Prelude.<$> targetSpotCapacity,
            Prelude.Just
              ("InstanceFleetId" Data..= instanceFleetId)
          ]
      )
