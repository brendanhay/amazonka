{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Recipes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.Recipes
  ( Recipes (..),

    -- * Smart constructor
    mkRecipes,

    -- * Lenses
    rSetup,
    rShutdown,
    rUndeploy,
    rConfigure,
    rDeploy,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | AWS OpsWorks Stacks supports five lifecycle events: __setup__ , __configuration__ , __deploy__ , __undeploy__ , and __shutdown__ . For each layer, AWS OpsWorks Stacks runs a set of standard recipes for each event. In addition, you can provide custom recipes for any or all layers and events. AWS OpsWorks Stacks runs custom event recipes after the standard recipes. @LayerCustomRecipes@ specifies the custom recipes for a particular layer to be run in response to each of the five events.
--
-- To specify a recipe, use the cookbook's directory name in the repository followed by two colons and the recipe name, which is the recipe's file name without the .rb extension. For example: phpapp2::dbsetup specifies the dbsetup.rb recipe in the repository's phpapp2 folder.
--
-- /See:/ 'mkRecipes' smart constructor.
data Recipes = Recipes'
  { -- | An array of custom recipe names to be run following a @setup@ event.
    setup :: Lude.Maybe [Lude.Text],
    -- | An array of custom recipe names to be run following a @shutdown@ event.
    shutdown :: Lude.Maybe [Lude.Text],
    -- | An array of custom recipe names to be run following a @undeploy@ event.
    undeploy :: Lude.Maybe [Lude.Text],
    -- | An array of custom recipe names to be run following a @configure@ event.
    configure :: Lude.Maybe [Lude.Text],
    -- | An array of custom recipe names to be run following a @deploy@ event.
    deploy :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Recipes' with the minimum fields required to make a request.
--
-- * 'setup' - An array of custom recipe names to be run following a @setup@ event.
-- * 'shutdown' - An array of custom recipe names to be run following a @shutdown@ event.
-- * 'undeploy' - An array of custom recipe names to be run following a @undeploy@ event.
-- * 'configure' - An array of custom recipe names to be run following a @configure@ event.
-- * 'deploy' - An array of custom recipe names to be run following a @deploy@ event.
mkRecipes ::
  Recipes
mkRecipes =
  Recipes'
    { setup = Lude.Nothing,
      shutdown = Lude.Nothing,
      undeploy = Lude.Nothing,
      configure = Lude.Nothing,
      deploy = Lude.Nothing
    }

-- | An array of custom recipe names to be run following a @setup@ event.
--
-- /Note:/ Consider using 'setup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rSetup :: Lens.Lens' Recipes (Lude.Maybe [Lude.Text])
rSetup = Lens.lens (setup :: Recipes -> Lude.Maybe [Lude.Text]) (\s a -> s {setup = a} :: Recipes)
{-# DEPRECATED rSetup "Use generic-lens or generic-optics with 'setup' instead." #-}

-- | An array of custom recipe names to be run following a @shutdown@ event.
--
-- /Note:/ Consider using 'shutdown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rShutdown :: Lens.Lens' Recipes (Lude.Maybe [Lude.Text])
rShutdown = Lens.lens (shutdown :: Recipes -> Lude.Maybe [Lude.Text]) (\s a -> s {shutdown = a} :: Recipes)
{-# DEPRECATED rShutdown "Use generic-lens or generic-optics with 'shutdown' instead." #-}

-- | An array of custom recipe names to be run following a @undeploy@ event.
--
-- /Note:/ Consider using 'undeploy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rUndeploy :: Lens.Lens' Recipes (Lude.Maybe [Lude.Text])
rUndeploy = Lens.lens (undeploy :: Recipes -> Lude.Maybe [Lude.Text]) (\s a -> s {undeploy = a} :: Recipes)
{-# DEPRECATED rUndeploy "Use generic-lens or generic-optics with 'undeploy' instead." #-}

-- | An array of custom recipe names to be run following a @configure@ event.
--
-- /Note:/ Consider using 'configure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rConfigure :: Lens.Lens' Recipes (Lude.Maybe [Lude.Text])
rConfigure = Lens.lens (configure :: Recipes -> Lude.Maybe [Lude.Text]) (\s a -> s {configure = a} :: Recipes)
{-# DEPRECATED rConfigure "Use generic-lens or generic-optics with 'configure' instead." #-}

-- | An array of custom recipe names to be run following a @deploy@ event.
--
-- /Note:/ Consider using 'deploy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDeploy :: Lens.Lens' Recipes (Lude.Maybe [Lude.Text])
rDeploy = Lens.lens (deploy :: Recipes -> Lude.Maybe [Lude.Text]) (\s a -> s {deploy = a} :: Recipes)
{-# DEPRECATED rDeploy "Use generic-lens or generic-optics with 'deploy' instead." #-}

instance Lude.FromJSON Recipes where
  parseJSON =
    Lude.withObject
      "Recipes"
      ( \x ->
          Recipes'
            Lude.<$> (x Lude..:? "Setup" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Shutdown" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Undeploy" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Configure" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Deploy" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON Recipes where
  toJSON Recipes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Setup" Lude..=) Lude.<$> setup,
            ("Shutdown" Lude..=) Lude.<$> shutdown,
            ("Undeploy" Lude..=) Lude.<$> undeploy,
            ("Configure" Lude..=) Lude.<$> configure,
            ("Deploy" Lude..=) Lude.<$> deploy
          ]
      )
