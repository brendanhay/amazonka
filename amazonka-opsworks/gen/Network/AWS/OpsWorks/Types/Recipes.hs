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
-- Module      : Network.AWS.OpsWorks.Types.Recipes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.Recipes where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | AWS OpsWorks Stacks supports five lifecycle events: __setup__,
-- __configuration__, __deploy__, __undeploy__, and __shutdown__. For each
-- layer, AWS OpsWorks Stacks runs a set of standard recipes for each
-- event. In addition, you can provide custom recipes for any or all layers
-- and events. AWS OpsWorks Stacks runs custom event recipes after the
-- standard recipes. @LayerCustomRecipes@ specifies the custom recipes for
-- a particular layer to be run in response to each of the five events.
--
-- To specify a recipe, use the cookbook\'s directory name in the
-- repository followed by two colons and the recipe name, which is the
-- recipe\'s file name without the .rb extension. For example:
-- phpapp2::dbsetup specifies the dbsetup.rb recipe in the repository\'s
-- phpapp2 folder.
--
-- /See:/ 'newRecipes' smart constructor.
data Recipes = Recipes'
  { -- | An array of custom recipe names to be run following a @shutdown@ event.
    shutdown :: Core.Maybe [Core.Text],
    -- | An array of custom recipe names to be run following a @configure@ event.
    configure :: Core.Maybe [Core.Text],
    -- | An array of custom recipe names to be run following a @undeploy@ event.
    undeploy :: Core.Maybe [Core.Text],
    -- | An array of custom recipe names to be run following a @setup@ event.
    setup :: Core.Maybe [Core.Text],
    -- | An array of custom recipe names to be run following a @deploy@ event.
    deploy :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Recipes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shutdown', 'recipes_shutdown' - An array of custom recipe names to be run following a @shutdown@ event.
--
-- 'configure', 'recipes_configure' - An array of custom recipe names to be run following a @configure@ event.
--
-- 'undeploy', 'recipes_undeploy' - An array of custom recipe names to be run following a @undeploy@ event.
--
-- 'setup', 'recipes_setup' - An array of custom recipe names to be run following a @setup@ event.
--
-- 'deploy', 'recipes_deploy' - An array of custom recipe names to be run following a @deploy@ event.
newRecipes ::
  Recipes
newRecipes =
  Recipes'
    { shutdown = Core.Nothing,
      configure = Core.Nothing,
      undeploy = Core.Nothing,
      setup = Core.Nothing,
      deploy = Core.Nothing
    }

-- | An array of custom recipe names to be run following a @shutdown@ event.
recipes_shutdown :: Lens.Lens' Recipes (Core.Maybe [Core.Text])
recipes_shutdown = Lens.lens (\Recipes' {shutdown} -> shutdown) (\s@Recipes' {} a -> s {shutdown = a} :: Recipes) Core.. Lens.mapping Lens._Coerce

-- | An array of custom recipe names to be run following a @configure@ event.
recipes_configure :: Lens.Lens' Recipes (Core.Maybe [Core.Text])
recipes_configure = Lens.lens (\Recipes' {configure} -> configure) (\s@Recipes' {} a -> s {configure = a} :: Recipes) Core.. Lens.mapping Lens._Coerce

-- | An array of custom recipe names to be run following a @undeploy@ event.
recipes_undeploy :: Lens.Lens' Recipes (Core.Maybe [Core.Text])
recipes_undeploy = Lens.lens (\Recipes' {undeploy} -> undeploy) (\s@Recipes' {} a -> s {undeploy = a} :: Recipes) Core.. Lens.mapping Lens._Coerce

-- | An array of custom recipe names to be run following a @setup@ event.
recipes_setup :: Lens.Lens' Recipes (Core.Maybe [Core.Text])
recipes_setup = Lens.lens (\Recipes' {setup} -> setup) (\s@Recipes' {} a -> s {setup = a} :: Recipes) Core.. Lens.mapping Lens._Coerce

-- | An array of custom recipe names to be run following a @deploy@ event.
recipes_deploy :: Lens.Lens' Recipes (Core.Maybe [Core.Text])
recipes_deploy = Lens.lens (\Recipes' {deploy} -> deploy) (\s@Recipes' {} a -> s {deploy = a} :: Recipes) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON Recipes where
  parseJSON =
    Core.withObject
      "Recipes"
      ( \x ->
          Recipes'
            Core.<$> (x Core..:? "Shutdown" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Configure" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Undeploy" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Setup" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Deploy" Core..!= Core.mempty)
      )

instance Core.Hashable Recipes

instance Core.NFData Recipes

instance Core.ToJSON Recipes where
  toJSON Recipes' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Shutdown" Core..=) Core.<$> shutdown,
            ("Configure" Core..=) Core.<$> configure,
            ("Undeploy" Core..=) Core.<$> undeploy,
            ("Setup" Core..=) Core.<$> setup,
            ("Deploy" Core..=) Core.<$> deploy
          ]
      )
