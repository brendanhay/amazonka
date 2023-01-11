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
-- Module      : Amazonka.SageMaker.Types.InferenceExecutionConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.InferenceExecutionConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.InferenceExecutionMode

-- | Specifies details about how containers in a multi-container endpoint are
-- run.
--
-- /See:/ 'newInferenceExecutionConfig' smart constructor.
data InferenceExecutionConfig = InferenceExecutionConfig'
  { -- | How containers in a multi-container are run. The following values are
    -- valid.
    --
    -- -   @SERIAL@ - Containers run as a serial pipeline.
    --
    -- -   @DIRECT@ - Only the individual container that you specify is run.
    mode :: InferenceExecutionMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InferenceExecutionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mode', 'inferenceExecutionConfig_mode' - How containers in a multi-container are run. The following values are
-- valid.
--
-- -   @SERIAL@ - Containers run as a serial pipeline.
--
-- -   @DIRECT@ - Only the individual container that you specify is run.
newInferenceExecutionConfig ::
  -- | 'mode'
  InferenceExecutionMode ->
  InferenceExecutionConfig
newInferenceExecutionConfig pMode_ =
  InferenceExecutionConfig' {mode = pMode_}

-- | How containers in a multi-container are run. The following values are
-- valid.
--
-- -   @SERIAL@ - Containers run as a serial pipeline.
--
-- -   @DIRECT@ - Only the individual container that you specify is run.
inferenceExecutionConfig_mode :: Lens.Lens' InferenceExecutionConfig InferenceExecutionMode
inferenceExecutionConfig_mode = Lens.lens (\InferenceExecutionConfig' {mode} -> mode) (\s@InferenceExecutionConfig' {} a -> s {mode = a} :: InferenceExecutionConfig)

instance Data.FromJSON InferenceExecutionConfig where
  parseJSON =
    Data.withObject
      "InferenceExecutionConfig"
      ( \x ->
          InferenceExecutionConfig'
            Prelude.<$> (x Data..: "Mode")
      )

instance Prelude.Hashable InferenceExecutionConfig where
  hashWithSalt _salt InferenceExecutionConfig' {..} =
    _salt `Prelude.hashWithSalt` mode

instance Prelude.NFData InferenceExecutionConfig where
  rnf InferenceExecutionConfig' {..} = Prelude.rnf mode

instance Data.ToJSON InferenceExecutionConfig where
  toJSON InferenceExecutionConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Mode" Data..= mode)]
      )
