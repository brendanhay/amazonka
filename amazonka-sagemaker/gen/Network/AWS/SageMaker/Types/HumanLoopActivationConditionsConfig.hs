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
-- Module      : Network.AWS.SageMaker.Types.HumanLoopActivationConditionsConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HumanLoopActivationConditionsConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Defines under what conditions SageMaker creates a human loop. Used
-- within . See for the required format of activation conditions.
--
-- /See:/ 'newHumanLoopActivationConditionsConfig' smart constructor.
data HumanLoopActivationConditionsConfig = HumanLoopActivationConditionsConfig'
  { -- | JSON expressing use-case specific conditions declaratively. If any
    -- condition is matched, atomic tasks are created against the configured
    -- work team. The set of conditions is different for Rekognition and
    -- Textract. For more information about how to structure the JSON, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/a2i-human-fallback-conditions-json-schema.html JSON Schema for Human Loop Activation Conditions in Amazon Augmented AI>
    -- in the /Amazon SageMaker Developer Guide/.
    humanLoopActivationConditions :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HumanLoopActivationConditionsConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'humanLoopActivationConditions', 'humanLoopActivationConditionsConfig_humanLoopActivationConditions' - JSON expressing use-case specific conditions declaratively. If any
-- condition is matched, atomic tasks are created against the configured
-- work team. The set of conditions is different for Rekognition and
-- Textract. For more information about how to structure the JSON, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/a2i-human-fallback-conditions-json-schema.html JSON Schema for Human Loop Activation Conditions in Amazon Augmented AI>
-- in the /Amazon SageMaker Developer Guide/.
newHumanLoopActivationConditionsConfig ::
  -- | 'humanLoopActivationConditions'
  Core.Text ->
  HumanLoopActivationConditionsConfig
newHumanLoopActivationConditionsConfig
  pHumanLoopActivationConditions_ =
    HumanLoopActivationConditionsConfig'
      { humanLoopActivationConditions =
          pHumanLoopActivationConditions_
      }

-- | JSON expressing use-case specific conditions declaratively. If any
-- condition is matched, atomic tasks are created against the configured
-- work team. The set of conditions is different for Rekognition and
-- Textract. For more information about how to structure the JSON, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/a2i-human-fallback-conditions-json-schema.html JSON Schema for Human Loop Activation Conditions in Amazon Augmented AI>
-- in the /Amazon SageMaker Developer Guide/.
humanLoopActivationConditionsConfig_humanLoopActivationConditions :: Lens.Lens' HumanLoopActivationConditionsConfig Core.Text
humanLoopActivationConditionsConfig_humanLoopActivationConditions = Lens.lens (\HumanLoopActivationConditionsConfig' {humanLoopActivationConditions} -> humanLoopActivationConditions) (\s@HumanLoopActivationConditionsConfig' {} a -> s {humanLoopActivationConditions = a} :: HumanLoopActivationConditionsConfig)

instance
  Core.FromJSON
    HumanLoopActivationConditionsConfig
  where
  parseJSON =
    Core.withObject
      "HumanLoopActivationConditionsConfig"
      ( \x ->
          HumanLoopActivationConditionsConfig'
            Core.<$> (x Core..: "HumanLoopActivationConditions")
      )

instance
  Core.Hashable
    HumanLoopActivationConditionsConfig

instance
  Core.NFData
    HumanLoopActivationConditionsConfig

instance
  Core.ToJSON
    HumanLoopActivationConditionsConfig
  where
  toJSON HumanLoopActivationConditionsConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "HumanLoopActivationConditions"
                  Core..= humanLoopActivationConditions
              )
          ]
      )
