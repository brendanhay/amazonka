{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.FunctionDefaultConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.FunctionDefaultConfig
  ( FunctionDefaultConfig (..),

    -- * Smart constructor
    mkFunctionDefaultConfig,

    -- * Lenses
    fdcExecution,
  )
where

import qualified Network.AWS.Greengrass.Types.FunctionDefaultExecutionConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The default configuration that applies to all Lambda functions in the group. Individual Lambda functions can override these settings.
--
-- /See:/ 'mkFunctionDefaultConfig' smart constructor.
newtype FunctionDefaultConfig = FunctionDefaultConfig'
  { execution :: Core.Maybe Types.FunctionDefaultExecutionConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'FunctionDefaultConfig' value with any optional fields omitted.
mkFunctionDefaultConfig ::
  FunctionDefaultConfig
mkFunctionDefaultConfig =
  FunctionDefaultConfig' {execution = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'execution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdcExecution :: Lens.Lens' FunctionDefaultConfig (Core.Maybe Types.FunctionDefaultExecutionConfig)
fdcExecution = Lens.field @"execution"
{-# DEPRECATED fdcExecution "Use generic-lens or generic-optics with 'execution' instead." #-}

instance Core.FromJSON FunctionDefaultConfig where
  toJSON FunctionDefaultConfig {..} =
    Core.object
      (Core.catMaybes [("Execution" Core..=) Core.<$> execution])

instance Core.FromJSON FunctionDefaultConfig where
  parseJSON =
    Core.withObject "FunctionDefaultConfig" Core.$
      \x -> FunctionDefaultConfig' Core.<$> (x Core..:? "Execution")
