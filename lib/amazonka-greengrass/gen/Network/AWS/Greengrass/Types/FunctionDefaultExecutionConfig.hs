{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.FunctionDefaultExecutionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Greengrass.Types.FunctionDefaultExecutionConfig
  ( FunctionDefaultExecutionConfig (..)
  -- * Smart constructor
  , mkFunctionDefaultExecutionConfig
  -- * Lenses
  , fdecIsolationMode
  , fdecRunAs
  ) where

import qualified Network.AWS.Greengrass.Types.FunctionIsolationMode as Types
import qualified Network.AWS.Greengrass.Types.FunctionRunAsConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration information that specifies how a Lambda function runs. 
--
-- /See:/ 'mkFunctionDefaultExecutionConfig' smart constructor.
data FunctionDefaultExecutionConfig = FunctionDefaultExecutionConfig'
  { isolationMode :: Core.Maybe Types.FunctionIsolationMode
  , runAs :: Core.Maybe Types.FunctionRunAsConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FunctionDefaultExecutionConfig' value with any optional fields omitted.
mkFunctionDefaultExecutionConfig
    :: FunctionDefaultExecutionConfig
mkFunctionDefaultExecutionConfig
  = FunctionDefaultExecutionConfig'{isolationMode = Core.Nothing,
                                    runAs = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'isolationMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdecIsolationMode :: Lens.Lens' FunctionDefaultExecutionConfig (Core.Maybe Types.FunctionIsolationMode)
fdecIsolationMode = Lens.field @"isolationMode"
{-# INLINEABLE fdecIsolationMode #-}
{-# DEPRECATED isolationMode "Use generic-lens or generic-optics with 'isolationMode' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'runAs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdecRunAs :: Lens.Lens' FunctionDefaultExecutionConfig (Core.Maybe Types.FunctionRunAsConfig)
fdecRunAs = Lens.field @"runAs"
{-# INLINEABLE fdecRunAs #-}
{-# DEPRECATED runAs "Use generic-lens or generic-optics with 'runAs' instead"  #-}

instance Core.FromJSON FunctionDefaultExecutionConfig where
        toJSON FunctionDefaultExecutionConfig{..}
          = Core.object
              (Core.catMaybes
                 [("IsolationMode" Core..=) Core.<$> isolationMode,
                  ("RunAs" Core..=) Core.<$> runAs])

instance Core.FromJSON FunctionDefaultExecutionConfig where
        parseJSON
          = Core.withObject "FunctionDefaultExecutionConfig" Core.$
              \ x ->
                FunctionDefaultExecutionConfig' Core.<$>
                  (x Core..:? "IsolationMode") Core.<*> x Core..:? "RunAs"
