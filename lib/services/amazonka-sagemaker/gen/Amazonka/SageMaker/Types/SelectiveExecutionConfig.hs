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
-- Module      : Amazonka.SageMaker.Types.SelectiveExecutionConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.SelectiveExecutionConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.SelectedStep

-- | The selective execution configuration applied to the pipeline run.
--
-- /See:/ 'newSelectiveExecutionConfig' smart constructor.
data SelectiveExecutionConfig = SelectiveExecutionConfig'
  { -- | The ARN from a reference execution of the current pipeline. Used to copy
    -- input collaterals needed for the selected steps to run. The execution
    -- status of the pipeline can be either @Failed@ or @Success@.
    sourcePipelineExecutionArn :: Prelude.Text,
    -- | A list of pipeline steps to run. All step(s) in all path(s) between two
    -- selected steps should be included.
    selectedSteps :: Prelude.NonEmpty SelectedStep
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SelectiveExecutionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourcePipelineExecutionArn', 'selectiveExecutionConfig_sourcePipelineExecutionArn' - The ARN from a reference execution of the current pipeline. Used to copy
-- input collaterals needed for the selected steps to run. The execution
-- status of the pipeline can be either @Failed@ or @Success@.
--
-- 'selectedSteps', 'selectiveExecutionConfig_selectedSteps' - A list of pipeline steps to run. All step(s) in all path(s) between two
-- selected steps should be included.
newSelectiveExecutionConfig ::
  -- | 'sourcePipelineExecutionArn'
  Prelude.Text ->
  -- | 'selectedSteps'
  Prelude.NonEmpty SelectedStep ->
  SelectiveExecutionConfig
newSelectiveExecutionConfig
  pSourcePipelineExecutionArn_
  pSelectedSteps_ =
    SelectiveExecutionConfig'
      { sourcePipelineExecutionArn =
          pSourcePipelineExecutionArn_,
        selectedSteps =
          Lens.coerced Lens.# pSelectedSteps_
      }

-- | The ARN from a reference execution of the current pipeline. Used to copy
-- input collaterals needed for the selected steps to run. The execution
-- status of the pipeline can be either @Failed@ or @Success@.
selectiveExecutionConfig_sourcePipelineExecutionArn :: Lens.Lens' SelectiveExecutionConfig Prelude.Text
selectiveExecutionConfig_sourcePipelineExecutionArn = Lens.lens (\SelectiveExecutionConfig' {sourcePipelineExecutionArn} -> sourcePipelineExecutionArn) (\s@SelectiveExecutionConfig' {} a -> s {sourcePipelineExecutionArn = a} :: SelectiveExecutionConfig)

-- | A list of pipeline steps to run. All step(s) in all path(s) between two
-- selected steps should be included.
selectiveExecutionConfig_selectedSteps :: Lens.Lens' SelectiveExecutionConfig (Prelude.NonEmpty SelectedStep)
selectiveExecutionConfig_selectedSteps = Lens.lens (\SelectiveExecutionConfig' {selectedSteps} -> selectedSteps) (\s@SelectiveExecutionConfig' {} a -> s {selectedSteps = a} :: SelectiveExecutionConfig) Prelude.. Lens.coerced

instance Data.FromJSON SelectiveExecutionConfig where
  parseJSON =
    Data.withObject
      "SelectiveExecutionConfig"
      ( \x ->
          SelectiveExecutionConfig'
            Prelude.<$> (x Data..: "SourcePipelineExecutionArn")
            Prelude.<*> (x Data..: "SelectedSteps")
      )

instance Prelude.Hashable SelectiveExecutionConfig where
  hashWithSalt _salt SelectiveExecutionConfig' {..} =
    _salt
      `Prelude.hashWithSalt` sourcePipelineExecutionArn
      `Prelude.hashWithSalt` selectedSteps

instance Prelude.NFData SelectiveExecutionConfig where
  rnf SelectiveExecutionConfig' {..} =
    Prelude.rnf sourcePipelineExecutionArn
      `Prelude.seq` Prelude.rnf selectedSteps

instance Data.ToJSON SelectiveExecutionConfig where
  toJSON SelectiveExecutionConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "SourcePipelineExecutionArn"
                  Data..= sourcePipelineExecutionArn
              ),
            Prelude.Just
              ("SelectedSteps" Data..= selectedSteps)
          ]
      )
