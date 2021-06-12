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
-- Module      : Network.AWS.Greengrass.Types.FunctionDefaultConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.FunctionDefaultConfig where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types.FunctionDefaultExecutionConfig
import qualified Network.AWS.Lens as Lens

-- | The default configuration that applies to all Lambda functions in the
-- group. Individual Lambda functions can override these settings.
--
-- /See:/ 'newFunctionDefaultConfig' smart constructor.
data FunctionDefaultConfig = FunctionDefaultConfig'
  { execution :: Core.Maybe FunctionDefaultExecutionConfig
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FunctionDefaultConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'execution', 'functionDefaultConfig_execution' - Undocumented member.
newFunctionDefaultConfig ::
  FunctionDefaultConfig
newFunctionDefaultConfig =
  FunctionDefaultConfig' {execution = Core.Nothing}

-- | Undocumented member.
functionDefaultConfig_execution :: Lens.Lens' FunctionDefaultConfig (Core.Maybe FunctionDefaultExecutionConfig)
functionDefaultConfig_execution = Lens.lens (\FunctionDefaultConfig' {execution} -> execution) (\s@FunctionDefaultConfig' {} a -> s {execution = a} :: FunctionDefaultConfig)

instance Core.FromJSON FunctionDefaultConfig where
  parseJSON =
    Core.withObject
      "FunctionDefaultConfig"
      ( \x ->
          FunctionDefaultConfig'
            Core.<$> (x Core..:? "Execution")
      )

instance Core.Hashable FunctionDefaultConfig

instance Core.NFData FunctionDefaultConfig

instance Core.ToJSON FunctionDefaultConfig where
  toJSON FunctionDefaultConfig' {..} =
    Core.object
      ( Core.catMaybes
          [("Execution" Core..=) Core.<$> execution]
      )
