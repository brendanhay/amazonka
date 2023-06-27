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
-- Module      : Amazonka.SageMaker.Types.SelectiveExecutionResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.SelectiveExecutionResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The ARN from an execution of the current pipeline.
--
-- /See:/ 'newSelectiveExecutionResult' smart constructor.
data SelectiveExecutionResult = SelectiveExecutionResult'
  { -- | The ARN from an execution of the current pipeline.
    sourcePipelineExecutionArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SelectiveExecutionResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourcePipelineExecutionArn', 'selectiveExecutionResult_sourcePipelineExecutionArn' - The ARN from an execution of the current pipeline.
newSelectiveExecutionResult ::
  SelectiveExecutionResult
newSelectiveExecutionResult =
  SelectiveExecutionResult'
    { sourcePipelineExecutionArn =
        Prelude.Nothing
    }

-- | The ARN from an execution of the current pipeline.
selectiveExecutionResult_sourcePipelineExecutionArn :: Lens.Lens' SelectiveExecutionResult (Prelude.Maybe Prelude.Text)
selectiveExecutionResult_sourcePipelineExecutionArn = Lens.lens (\SelectiveExecutionResult' {sourcePipelineExecutionArn} -> sourcePipelineExecutionArn) (\s@SelectiveExecutionResult' {} a -> s {sourcePipelineExecutionArn = a} :: SelectiveExecutionResult)

instance Data.FromJSON SelectiveExecutionResult where
  parseJSON =
    Data.withObject
      "SelectiveExecutionResult"
      ( \x ->
          SelectiveExecutionResult'
            Prelude.<$> (x Data..:? "SourcePipelineExecutionArn")
      )

instance Prelude.Hashable SelectiveExecutionResult where
  hashWithSalt _salt SelectiveExecutionResult' {..} =
    _salt
      `Prelude.hashWithSalt` sourcePipelineExecutionArn

instance Prelude.NFData SelectiveExecutionResult where
  rnf SelectiveExecutionResult' {..} =
    Prelude.rnf sourcePipelineExecutionArn
