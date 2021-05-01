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
-- Module      : Network.AWS.EMR.Types.ScalingTrigger
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ScalingTrigger where

import Network.AWS.EMR.Types.CloudWatchAlarmDefinition
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The conditions that trigger an automatic scaling activity.
--
-- /See:/ 'newScalingTrigger' smart constructor.
data ScalingTrigger = ScalingTrigger'
  { -- | The definition of a CloudWatch metric alarm. When the defined alarm
    -- conditions are met along with other trigger parameters, scaling activity
    -- begins.
    cloudWatchAlarmDefinition :: CloudWatchAlarmDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ScalingTrigger' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchAlarmDefinition', 'scalingTrigger_cloudWatchAlarmDefinition' - The definition of a CloudWatch metric alarm. When the defined alarm
-- conditions are met along with other trigger parameters, scaling activity
-- begins.
newScalingTrigger ::
  -- | 'cloudWatchAlarmDefinition'
  CloudWatchAlarmDefinition ->
  ScalingTrigger
newScalingTrigger pCloudWatchAlarmDefinition_ =
  ScalingTrigger'
    { cloudWatchAlarmDefinition =
        pCloudWatchAlarmDefinition_
    }

-- | The definition of a CloudWatch metric alarm. When the defined alarm
-- conditions are met along with other trigger parameters, scaling activity
-- begins.
scalingTrigger_cloudWatchAlarmDefinition :: Lens.Lens' ScalingTrigger CloudWatchAlarmDefinition
scalingTrigger_cloudWatchAlarmDefinition = Lens.lens (\ScalingTrigger' {cloudWatchAlarmDefinition} -> cloudWatchAlarmDefinition) (\s@ScalingTrigger' {} a -> s {cloudWatchAlarmDefinition = a} :: ScalingTrigger)

instance Prelude.FromJSON ScalingTrigger where
  parseJSON =
    Prelude.withObject
      "ScalingTrigger"
      ( \x ->
          ScalingTrigger'
            Prelude.<$> (x Prelude..: "CloudWatchAlarmDefinition")
      )

instance Prelude.Hashable ScalingTrigger

instance Prelude.NFData ScalingTrigger

instance Prelude.ToJSON ScalingTrigger where
  toJSON ScalingTrigger' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "CloudWatchAlarmDefinition"
                  Prelude..= cloudWatchAlarmDefinition
              )
          ]
      )
