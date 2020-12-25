{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.FunctionExecutionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.FunctionExecutionConfig
  ( FunctionExecutionConfig (..),

    -- * Smart constructor
    mkFunctionExecutionConfig,

    -- * Lenses
    fecIsolationMode,
    fecRunAs,
  )
where

import qualified Network.AWS.Greengrass.Types.FunctionIsolationMode as Types
import qualified Network.AWS.Greengrass.Types.FunctionRunAsConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration information that specifies how a Lambda function runs.
--
-- /See:/ 'mkFunctionExecutionConfig' smart constructor.
data FunctionExecutionConfig = FunctionExecutionConfig'
  { isolationMode :: Core.Maybe Types.FunctionIsolationMode,
    runAs :: Core.Maybe Types.FunctionRunAsConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FunctionExecutionConfig' value with any optional fields omitted.
mkFunctionExecutionConfig ::
  FunctionExecutionConfig
mkFunctionExecutionConfig =
  FunctionExecutionConfig'
    { isolationMode = Core.Nothing,
      runAs = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'isolationMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fecIsolationMode :: Lens.Lens' FunctionExecutionConfig (Core.Maybe Types.FunctionIsolationMode)
fecIsolationMode = Lens.field @"isolationMode"
{-# DEPRECATED fecIsolationMode "Use generic-lens or generic-optics with 'isolationMode' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'runAs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fecRunAs :: Lens.Lens' FunctionExecutionConfig (Core.Maybe Types.FunctionRunAsConfig)
fecRunAs = Lens.field @"runAs"
{-# DEPRECATED fecRunAs "Use generic-lens or generic-optics with 'runAs' instead." #-}

instance Core.FromJSON FunctionExecutionConfig where
  toJSON FunctionExecutionConfig {..} =
    Core.object
      ( Core.catMaybes
          [ ("IsolationMode" Core..=) Core.<$> isolationMode,
            ("RunAs" Core..=) Core.<$> runAs
          ]
      )

instance Core.FromJSON FunctionExecutionConfig where
  parseJSON =
    Core.withObject "FunctionExecutionConfig" Core.$
      \x ->
        FunctionExecutionConfig'
          Core.<$> (x Core..:? "IsolationMode") Core.<*> (x Core..:? "RunAs")
