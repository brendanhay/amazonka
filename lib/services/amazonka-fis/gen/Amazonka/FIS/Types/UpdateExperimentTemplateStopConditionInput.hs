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
-- Module      : Amazonka.FIS.Types.UpdateExperimentTemplateStopConditionInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.UpdateExperimentTemplateStopConditionInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies a stop condition for an experiment. You can define a stop
-- condition as a CloudWatch alarm.
--
-- /See:/ 'newUpdateExperimentTemplateStopConditionInput' smart constructor.
data UpdateExperimentTemplateStopConditionInput = UpdateExperimentTemplateStopConditionInput'
  { -- | The Amazon Resource Name (ARN) of the CloudWatch alarm.
    value :: Prelude.Maybe Prelude.Text,
    -- | The source for the stop condition. Specify @aws:cloudwatch:alarm@ if the
    -- stop condition is defined by a CloudWatch alarm. Specify @none@ if there
    -- is no stop condition.
    source :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateExperimentTemplateStopConditionInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'updateExperimentTemplateStopConditionInput_value' - The Amazon Resource Name (ARN) of the CloudWatch alarm.
--
-- 'source', 'updateExperimentTemplateStopConditionInput_source' - The source for the stop condition. Specify @aws:cloudwatch:alarm@ if the
-- stop condition is defined by a CloudWatch alarm. Specify @none@ if there
-- is no stop condition.
newUpdateExperimentTemplateStopConditionInput ::
  -- | 'source'
  Prelude.Text ->
  UpdateExperimentTemplateStopConditionInput
newUpdateExperimentTemplateStopConditionInput
  pSource_ =
    UpdateExperimentTemplateStopConditionInput'
      { value =
          Prelude.Nothing,
        source = pSource_
      }

-- | The Amazon Resource Name (ARN) of the CloudWatch alarm.
updateExperimentTemplateStopConditionInput_value :: Lens.Lens' UpdateExperimentTemplateStopConditionInput (Prelude.Maybe Prelude.Text)
updateExperimentTemplateStopConditionInput_value = Lens.lens (\UpdateExperimentTemplateStopConditionInput' {value} -> value) (\s@UpdateExperimentTemplateStopConditionInput' {} a -> s {value = a} :: UpdateExperimentTemplateStopConditionInput)

-- | The source for the stop condition. Specify @aws:cloudwatch:alarm@ if the
-- stop condition is defined by a CloudWatch alarm. Specify @none@ if there
-- is no stop condition.
updateExperimentTemplateStopConditionInput_source :: Lens.Lens' UpdateExperimentTemplateStopConditionInput Prelude.Text
updateExperimentTemplateStopConditionInput_source = Lens.lens (\UpdateExperimentTemplateStopConditionInput' {source} -> source) (\s@UpdateExperimentTemplateStopConditionInput' {} a -> s {source = a} :: UpdateExperimentTemplateStopConditionInput)

instance
  Prelude.Hashable
    UpdateExperimentTemplateStopConditionInput
  where
  hashWithSalt
    _salt
    UpdateExperimentTemplateStopConditionInput' {..} =
      _salt `Prelude.hashWithSalt` value
        `Prelude.hashWithSalt` source

instance
  Prelude.NFData
    UpdateExperimentTemplateStopConditionInput
  where
  rnf UpdateExperimentTemplateStopConditionInput' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf source

instance
  Core.ToJSON
    UpdateExperimentTemplateStopConditionInput
  where
  toJSON
    UpdateExperimentTemplateStopConditionInput' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("value" Core..=) Prelude.<$> value,
              Prelude.Just ("source" Core..= source)
            ]
        )
