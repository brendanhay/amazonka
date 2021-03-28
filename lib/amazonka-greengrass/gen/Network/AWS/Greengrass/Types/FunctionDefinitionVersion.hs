{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.FunctionDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Greengrass.Types.FunctionDefinitionVersion
  ( FunctionDefinitionVersion (..)
  -- * Smart constructor
  , mkFunctionDefinitionVersion
  -- * Lenses
  , fdvDefaultConfig
  , fdvFunctions
  ) where

import qualified Network.AWS.Greengrass.Types.Function as Types
import qualified Network.AWS.Greengrass.Types.FunctionDefaultConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a function definition version.
--
-- /See:/ 'mkFunctionDefinitionVersion' smart constructor.
data FunctionDefinitionVersion = FunctionDefinitionVersion'
  { defaultConfig :: Core.Maybe Types.FunctionDefaultConfig
    -- ^ The default configuration that applies to all Lambda functions in this function definition version. Individual Lambda functions can override these settings.
  , functions :: Core.Maybe [Types.Function]
    -- ^ A list of Lambda functions in this function definition version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FunctionDefinitionVersion' value with any optional fields omitted.
mkFunctionDefinitionVersion
    :: FunctionDefinitionVersion
mkFunctionDefinitionVersion
  = FunctionDefinitionVersion'{defaultConfig = Core.Nothing,
                               functions = Core.Nothing}

-- | The default configuration that applies to all Lambda functions in this function definition version. Individual Lambda functions can override these settings.
--
-- /Note:/ Consider using 'defaultConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdvDefaultConfig :: Lens.Lens' FunctionDefinitionVersion (Core.Maybe Types.FunctionDefaultConfig)
fdvDefaultConfig = Lens.field @"defaultConfig"
{-# INLINEABLE fdvDefaultConfig #-}
{-# DEPRECATED defaultConfig "Use generic-lens or generic-optics with 'defaultConfig' instead"  #-}

-- | A list of Lambda functions in this function definition version.
--
-- /Note:/ Consider using 'functions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdvFunctions :: Lens.Lens' FunctionDefinitionVersion (Core.Maybe [Types.Function])
fdvFunctions = Lens.field @"functions"
{-# INLINEABLE fdvFunctions #-}
{-# DEPRECATED functions "Use generic-lens or generic-optics with 'functions' instead"  #-}

instance Core.FromJSON FunctionDefinitionVersion where
        toJSON FunctionDefinitionVersion{..}
          = Core.object
              (Core.catMaybes
                 [("DefaultConfig" Core..=) Core.<$> defaultConfig,
                  ("Functions" Core..=) Core.<$> functions])

instance Core.FromJSON FunctionDefinitionVersion where
        parseJSON
          = Core.withObject "FunctionDefinitionVersion" Core.$
              \ x ->
                FunctionDefinitionVersion' Core.<$>
                  (x Core..:? "DefaultConfig") Core.<*> x Core..:? "Functions"
