{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Recipes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.Recipes
  ( Recipes (..)
  -- * Smart constructor
  , mkRecipes
  -- * Lenses
  , rConfigure
  , rDeploy
  , rSetup
  , rShutdown
  , rUndeploy
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | AWS OpsWorks Stacks supports five lifecycle events: __setup__ , __configuration__ , __deploy__ , __undeploy__ , and __shutdown__ . For each layer, AWS OpsWorks Stacks runs a set of standard recipes for each event. In addition, you can provide custom recipes for any or all layers and events. AWS OpsWorks Stacks runs custom event recipes after the standard recipes. @LayerCustomRecipes@ specifies the custom recipes for a particular layer to be run in response to each of the five events. 
--
-- To specify a recipe, use the cookbook's directory name in the repository followed by two colons and the recipe name, which is the recipe's file name without the .rb extension. For example: phpapp2::dbsetup specifies the dbsetup.rb recipe in the repository's phpapp2 folder.
--
-- /See:/ 'mkRecipes' smart constructor.
data Recipes = Recipes'
  { configure :: Core.Maybe [Core.Text]
    -- ^ An array of custom recipe names to be run following a @configure@ event.
  , deploy :: Core.Maybe [Core.Text]
    -- ^ An array of custom recipe names to be run following a @deploy@ event.
  , setup :: Core.Maybe [Core.Text]
    -- ^ An array of custom recipe names to be run following a @setup@ event.
  , shutdown :: Core.Maybe [Core.Text]
    -- ^ An array of custom recipe names to be run following a @shutdown@ event.
  , undeploy :: Core.Maybe [Core.Text]
    -- ^ An array of custom recipe names to be run following a @undeploy@ event.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Recipes' value with any optional fields omitted.
mkRecipes
    :: Recipes
mkRecipes
  = Recipes'{configure = Core.Nothing, deploy = Core.Nothing,
             setup = Core.Nothing, shutdown = Core.Nothing,
             undeploy = Core.Nothing}

-- | An array of custom recipe names to be run following a @configure@ event.
--
-- /Note:/ Consider using 'configure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rConfigure :: Lens.Lens' Recipes (Core.Maybe [Core.Text])
rConfigure = Lens.field @"configure"
{-# INLINEABLE rConfigure #-}
{-# DEPRECATED configure "Use generic-lens or generic-optics with 'configure' instead"  #-}

-- | An array of custom recipe names to be run following a @deploy@ event.
--
-- /Note:/ Consider using 'deploy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDeploy :: Lens.Lens' Recipes (Core.Maybe [Core.Text])
rDeploy = Lens.field @"deploy"
{-# INLINEABLE rDeploy #-}
{-# DEPRECATED deploy "Use generic-lens or generic-optics with 'deploy' instead"  #-}

-- | An array of custom recipe names to be run following a @setup@ event.
--
-- /Note:/ Consider using 'setup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rSetup :: Lens.Lens' Recipes (Core.Maybe [Core.Text])
rSetup = Lens.field @"setup"
{-# INLINEABLE rSetup #-}
{-# DEPRECATED setup "Use generic-lens or generic-optics with 'setup' instead"  #-}

-- | An array of custom recipe names to be run following a @shutdown@ event.
--
-- /Note:/ Consider using 'shutdown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rShutdown :: Lens.Lens' Recipes (Core.Maybe [Core.Text])
rShutdown = Lens.field @"shutdown"
{-# INLINEABLE rShutdown #-}
{-# DEPRECATED shutdown "Use generic-lens or generic-optics with 'shutdown' instead"  #-}

-- | An array of custom recipe names to be run following a @undeploy@ event.
--
-- /Note:/ Consider using 'undeploy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rUndeploy :: Lens.Lens' Recipes (Core.Maybe [Core.Text])
rUndeploy = Lens.field @"undeploy"
{-# INLINEABLE rUndeploy #-}
{-# DEPRECATED undeploy "Use generic-lens or generic-optics with 'undeploy' instead"  #-}

instance Core.FromJSON Recipes where
        toJSON Recipes{..}
          = Core.object
              (Core.catMaybes
                 [("Configure" Core..=) Core.<$> configure,
                  ("Deploy" Core..=) Core.<$> deploy,
                  ("Setup" Core..=) Core.<$> setup,
                  ("Shutdown" Core..=) Core.<$> shutdown,
                  ("Undeploy" Core..=) Core.<$> undeploy])

instance Core.FromJSON Recipes where
        parseJSON
          = Core.withObject "Recipes" Core.$
              \ x ->
                Recipes' Core.<$>
                  (x Core..:? "Configure") Core.<*> x Core..:? "Deploy" Core.<*>
                    x Core..:? "Setup"
                    Core.<*> x Core..:? "Shutdown"
                    Core.<*> x Core..:? "Undeploy"
