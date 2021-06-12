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
-- Module      : Network.AWS.Greengrass.Types.FunctionDefaultExecutionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.FunctionDefaultExecutionConfig where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types.FunctionIsolationMode
import Network.AWS.Greengrass.Types.FunctionRunAsConfig
import qualified Network.AWS.Lens as Lens

-- | Configuration information that specifies how a Lambda function runs.
--
-- /See:/ 'newFunctionDefaultExecutionConfig' smart constructor.
data FunctionDefaultExecutionConfig = FunctionDefaultExecutionConfig'
  { isolationMode :: Core.Maybe FunctionIsolationMode,
    runAs :: Core.Maybe FunctionRunAsConfig
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FunctionDefaultExecutionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isolationMode', 'functionDefaultExecutionConfig_isolationMode' - Undocumented member.
--
-- 'runAs', 'functionDefaultExecutionConfig_runAs' - Undocumented member.
newFunctionDefaultExecutionConfig ::
  FunctionDefaultExecutionConfig
newFunctionDefaultExecutionConfig =
  FunctionDefaultExecutionConfig'
    { isolationMode =
        Core.Nothing,
      runAs = Core.Nothing
    }

-- | Undocumented member.
functionDefaultExecutionConfig_isolationMode :: Lens.Lens' FunctionDefaultExecutionConfig (Core.Maybe FunctionIsolationMode)
functionDefaultExecutionConfig_isolationMode = Lens.lens (\FunctionDefaultExecutionConfig' {isolationMode} -> isolationMode) (\s@FunctionDefaultExecutionConfig' {} a -> s {isolationMode = a} :: FunctionDefaultExecutionConfig)

-- | Undocumented member.
functionDefaultExecutionConfig_runAs :: Lens.Lens' FunctionDefaultExecutionConfig (Core.Maybe FunctionRunAsConfig)
functionDefaultExecutionConfig_runAs = Lens.lens (\FunctionDefaultExecutionConfig' {runAs} -> runAs) (\s@FunctionDefaultExecutionConfig' {} a -> s {runAs = a} :: FunctionDefaultExecutionConfig)

instance Core.FromJSON FunctionDefaultExecutionConfig where
  parseJSON =
    Core.withObject
      "FunctionDefaultExecutionConfig"
      ( \x ->
          FunctionDefaultExecutionConfig'
            Core.<$> (x Core..:? "IsolationMode")
            Core.<*> (x Core..:? "RunAs")
      )

instance Core.Hashable FunctionDefaultExecutionConfig

instance Core.NFData FunctionDefaultExecutionConfig

instance Core.ToJSON FunctionDefaultExecutionConfig where
  toJSON FunctionDefaultExecutionConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("IsolationMode" Core..=) Core.<$> isolationMode,
            ("RunAs" Core..=) Core.<$> runAs
          ]
      )
