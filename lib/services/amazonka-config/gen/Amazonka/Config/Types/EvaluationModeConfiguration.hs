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
-- Module      : Amazonka.Config.Types.EvaluationModeConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.EvaluationModeConfiguration where

import Amazonka.Config.Types.EvaluationMode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration object for Config rule evaluation mode. The Supported
-- valid values are Detective or Proactive.
--
-- /See:/ 'newEvaluationModeConfiguration' smart constructor.
data EvaluationModeConfiguration = EvaluationModeConfiguration'
  { -- | The mode of an evaluation. The valid values are Detective or Proactive.
    mode :: Prelude.Maybe EvaluationMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationModeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mode', 'evaluationModeConfiguration_mode' - The mode of an evaluation. The valid values are Detective or Proactive.
newEvaluationModeConfiguration ::
  EvaluationModeConfiguration
newEvaluationModeConfiguration =
  EvaluationModeConfiguration'
    { mode =
        Prelude.Nothing
    }

-- | The mode of an evaluation. The valid values are Detective or Proactive.
evaluationModeConfiguration_mode :: Lens.Lens' EvaluationModeConfiguration (Prelude.Maybe EvaluationMode)
evaluationModeConfiguration_mode = Lens.lens (\EvaluationModeConfiguration' {mode} -> mode) (\s@EvaluationModeConfiguration' {} a -> s {mode = a} :: EvaluationModeConfiguration)

instance Data.FromJSON EvaluationModeConfiguration where
  parseJSON =
    Data.withObject
      "EvaluationModeConfiguration"
      ( \x ->
          EvaluationModeConfiguration'
            Prelude.<$> (x Data..:? "Mode")
      )

instance Prelude.Hashable EvaluationModeConfiguration where
  hashWithSalt _salt EvaluationModeConfiguration' {..} =
    _salt `Prelude.hashWithSalt` mode

instance Prelude.NFData EvaluationModeConfiguration where
  rnf EvaluationModeConfiguration' {..} =
    Prelude.rnf mode

instance Data.ToJSON EvaluationModeConfiguration where
  toJSON EvaluationModeConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Mode" Data..=) Prelude.<$> mode]
      )
