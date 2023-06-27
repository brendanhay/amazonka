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
-- Module      : Amazonka.Greengrass.Types.FunctionExecutionConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.FunctionExecutionConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types.FunctionIsolationMode
import Amazonka.Greengrass.Types.FunctionRunAsConfig
import qualified Amazonka.Prelude as Prelude

-- | Configuration information that specifies how a Lambda function runs.
--
-- /See:/ 'newFunctionExecutionConfig' smart constructor.
data FunctionExecutionConfig = FunctionExecutionConfig'
  { isolationMode :: Prelude.Maybe FunctionIsolationMode,
    runAs :: Prelude.Maybe FunctionRunAsConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FunctionExecutionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isolationMode', 'functionExecutionConfig_isolationMode' - Undocumented member.
--
-- 'runAs', 'functionExecutionConfig_runAs' - Undocumented member.
newFunctionExecutionConfig ::
  FunctionExecutionConfig
newFunctionExecutionConfig =
  FunctionExecutionConfig'
    { isolationMode =
        Prelude.Nothing,
      runAs = Prelude.Nothing
    }

-- | Undocumented member.
functionExecutionConfig_isolationMode :: Lens.Lens' FunctionExecutionConfig (Prelude.Maybe FunctionIsolationMode)
functionExecutionConfig_isolationMode = Lens.lens (\FunctionExecutionConfig' {isolationMode} -> isolationMode) (\s@FunctionExecutionConfig' {} a -> s {isolationMode = a} :: FunctionExecutionConfig)

-- | Undocumented member.
functionExecutionConfig_runAs :: Lens.Lens' FunctionExecutionConfig (Prelude.Maybe FunctionRunAsConfig)
functionExecutionConfig_runAs = Lens.lens (\FunctionExecutionConfig' {runAs} -> runAs) (\s@FunctionExecutionConfig' {} a -> s {runAs = a} :: FunctionExecutionConfig)

instance Data.FromJSON FunctionExecutionConfig where
  parseJSON =
    Data.withObject
      "FunctionExecutionConfig"
      ( \x ->
          FunctionExecutionConfig'
            Prelude.<$> (x Data..:? "IsolationMode")
            Prelude.<*> (x Data..:? "RunAs")
      )

instance Prelude.Hashable FunctionExecutionConfig where
  hashWithSalt _salt FunctionExecutionConfig' {..} =
    _salt
      `Prelude.hashWithSalt` isolationMode
      `Prelude.hashWithSalt` runAs

instance Prelude.NFData FunctionExecutionConfig where
  rnf FunctionExecutionConfig' {..} =
    Prelude.rnf isolationMode
      `Prelude.seq` Prelude.rnf runAs

instance Data.ToJSON FunctionExecutionConfig where
  toJSON FunctionExecutionConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IsolationMode" Data..=) Prelude.<$> isolationMode,
            ("RunAs" Data..=) Prelude.<$> runAs
          ]
      )
