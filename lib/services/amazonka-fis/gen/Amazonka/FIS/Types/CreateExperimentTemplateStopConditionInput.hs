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
-- Module      : Amazonka.FIS.Types.CreateExperimentTemplateStopConditionInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.CreateExperimentTemplateStopConditionInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a stop condition for an experiment template.
--
-- /See:/ 'newCreateExperimentTemplateStopConditionInput' smart constructor.
data CreateExperimentTemplateStopConditionInput = CreateExperimentTemplateStopConditionInput'
  { -- | The Amazon Resource Name (ARN) of the CloudWatch alarm. This is required
    -- if the source is a CloudWatch alarm.
    value :: Prelude.Maybe Prelude.Text,
    -- | The source for the stop condition. Specify @aws:cloudwatch:alarm@ if the
    -- stop condition is defined by a CloudWatch alarm. Specify @none@ if there
    -- is no stop condition.
    source :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateExperimentTemplateStopConditionInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'createExperimentTemplateStopConditionInput_value' - The Amazon Resource Name (ARN) of the CloudWatch alarm. This is required
-- if the source is a CloudWatch alarm.
--
-- 'source', 'createExperimentTemplateStopConditionInput_source' - The source for the stop condition. Specify @aws:cloudwatch:alarm@ if the
-- stop condition is defined by a CloudWatch alarm. Specify @none@ if there
-- is no stop condition.
newCreateExperimentTemplateStopConditionInput ::
  -- | 'source'
  Prelude.Text ->
  CreateExperimentTemplateStopConditionInput
newCreateExperimentTemplateStopConditionInput
  pSource_ =
    CreateExperimentTemplateStopConditionInput'
      { value =
          Prelude.Nothing,
        source = pSource_
      }

-- | The Amazon Resource Name (ARN) of the CloudWatch alarm. This is required
-- if the source is a CloudWatch alarm.
createExperimentTemplateStopConditionInput_value :: Lens.Lens' CreateExperimentTemplateStopConditionInput (Prelude.Maybe Prelude.Text)
createExperimentTemplateStopConditionInput_value = Lens.lens (\CreateExperimentTemplateStopConditionInput' {value} -> value) (\s@CreateExperimentTemplateStopConditionInput' {} a -> s {value = a} :: CreateExperimentTemplateStopConditionInput)

-- | The source for the stop condition. Specify @aws:cloudwatch:alarm@ if the
-- stop condition is defined by a CloudWatch alarm. Specify @none@ if there
-- is no stop condition.
createExperimentTemplateStopConditionInput_source :: Lens.Lens' CreateExperimentTemplateStopConditionInput Prelude.Text
createExperimentTemplateStopConditionInput_source = Lens.lens (\CreateExperimentTemplateStopConditionInput' {source} -> source) (\s@CreateExperimentTemplateStopConditionInput' {} a -> s {source = a} :: CreateExperimentTemplateStopConditionInput)

instance
  Prelude.Hashable
    CreateExperimentTemplateStopConditionInput
  where
  hashWithSalt
    _salt
    CreateExperimentTemplateStopConditionInput' {..} =
      _salt
        `Prelude.hashWithSalt` value
        `Prelude.hashWithSalt` source

instance
  Prelude.NFData
    CreateExperimentTemplateStopConditionInput
  where
  rnf CreateExperimentTemplateStopConditionInput' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf source

instance
  Data.ToJSON
    CreateExperimentTemplateStopConditionInput
  where
  toJSON
    CreateExperimentTemplateStopConditionInput' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("value" Data..=) Prelude.<$> value,
              Prelude.Just ("source" Data..= source)
            ]
        )
