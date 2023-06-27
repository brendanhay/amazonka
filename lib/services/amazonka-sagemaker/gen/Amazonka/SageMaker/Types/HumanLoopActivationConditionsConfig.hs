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
-- Module      : Amazonka.SageMaker.Types.HumanLoopActivationConditionsConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.HumanLoopActivationConditionsConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines under what conditions SageMaker creates a human loop. Used
-- within
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateFlowDefinition.html CreateFlowDefinition>.
-- See
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_HumanLoopActivationConditionsConfig.html HumanLoopActivationConditionsConfig>
-- for the required format of activation conditions.
--
-- /See:/ 'newHumanLoopActivationConditionsConfig' smart constructor.
data HumanLoopActivationConditionsConfig = HumanLoopActivationConditionsConfig'
  { -- | JSON expressing use-case specific conditions declaratively. If any
    -- condition is matched, atomic tasks are created against the configured
    -- work team. The set of conditions is different for Rekognition and
    -- Textract. For more information about how to structure the JSON, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/a2i-human-fallback-conditions-json-schema.html JSON Schema for Human Loop Activation Conditions in Amazon Augmented AI>
    -- in the /Amazon SageMaker Developer Guide/.
    humanLoopActivationConditions :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
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
humanLoopActivationConditionsConfig_humanLoopActivationConditions :: Lens.Lens' HumanLoopActivationConditionsConfig Prelude.Text
humanLoopActivationConditionsConfig_humanLoopActivationConditions = Lens.lens (\HumanLoopActivationConditionsConfig' {humanLoopActivationConditions} -> humanLoopActivationConditions) (\s@HumanLoopActivationConditionsConfig' {} a -> s {humanLoopActivationConditions = a} :: HumanLoopActivationConditionsConfig)

instance
  Data.FromJSON
    HumanLoopActivationConditionsConfig
  where
  parseJSON =
    Data.withObject
      "HumanLoopActivationConditionsConfig"
      ( \x ->
          HumanLoopActivationConditionsConfig'
            Prelude.<$> (x Data..: "HumanLoopActivationConditions")
      )

instance
  Prelude.Hashable
    HumanLoopActivationConditionsConfig
  where
  hashWithSalt
    _salt
    HumanLoopActivationConditionsConfig' {..} =
      _salt
        `Prelude.hashWithSalt` humanLoopActivationConditions

instance
  Prelude.NFData
    HumanLoopActivationConditionsConfig
  where
  rnf HumanLoopActivationConditionsConfig' {..} =
    Prelude.rnf humanLoopActivationConditions

instance
  Data.ToJSON
    HumanLoopActivationConditionsConfig
  where
  toJSON HumanLoopActivationConditionsConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "HumanLoopActivationConditions"
                  Data..= humanLoopActivationConditions
              )
          ]
      )
