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
-- Module      : Network.AWS.Greengrass.Types.FunctionExecutionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.FunctionExecutionConfig where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types.FunctionIsolationMode
import Network.AWS.Greengrass.Types.FunctionRunAsConfig
import qualified Network.AWS.Lens as Lens

-- | Configuration information that specifies how a Lambda function runs.
--
-- /See:/ 'newFunctionExecutionConfig' smart constructor.
data FunctionExecutionConfig = FunctionExecutionConfig'
  { isolationMode :: Core.Maybe FunctionIsolationMode,
    runAs :: Core.Maybe FunctionRunAsConfig
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      runAs = Core.Nothing
    }

-- | Undocumented member.
functionExecutionConfig_isolationMode :: Lens.Lens' FunctionExecutionConfig (Core.Maybe FunctionIsolationMode)
functionExecutionConfig_isolationMode = Lens.lens (\FunctionExecutionConfig' {isolationMode} -> isolationMode) (\s@FunctionExecutionConfig' {} a -> s {isolationMode = a} :: FunctionExecutionConfig)

-- | Undocumented member.
functionExecutionConfig_runAs :: Lens.Lens' FunctionExecutionConfig (Core.Maybe FunctionRunAsConfig)
functionExecutionConfig_runAs = Lens.lens (\FunctionExecutionConfig' {runAs} -> runAs) (\s@FunctionExecutionConfig' {} a -> s {runAs = a} :: FunctionExecutionConfig)

instance Core.FromJSON FunctionExecutionConfig where
  parseJSON =
    Core.withObject
      "FunctionExecutionConfig"
      ( \x ->
          FunctionExecutionConfig'
            Core.<$> (x Core..:? "IsolationMode")
            Core.<*> (x Core..:? "RunAs")
      )

instance Core.Hashable FunctionExecutionConfig

instance Core.NFData FunctionExecutionConfig

instance Core.ToJSON FunctionExecutionConfig where
  toJSON FunctionExecutionConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("IsolationMode" Core..=) Core.<$> isolationMode,
            ("RunAs" Core..=) Core.<$> runAs
          ]
      )
