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
import qualified Network.AWS.Prelude as Prelude

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
    shutdown :: Prelude.Maybe [Prelude.Text],
    -- | An array of custom recipe names to be run following a @configure@ event.
    configure :: Prelude.Maybe [Prelude.Text],
    -- | An array of custom recipe names to be run following a @undeploy@ event.
    undeploy :: Prelude.Maybe [Prelude.Text],
    -- | An array of custom recipe names to be run following a @setup@ event.
    setup :: Prelude.Maybe [Prelude.Text],
    -- | An array of custom recipe names to be run following a @deploy@ event.
    deploy :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { shutdown = Prelude.Nothing,
      configure = Prelude.Nothing,
      undeploy = Prelude.Nothing,
      setup = Prelude.Nothing,
      deploy = Prelude.Nothing
    }

-- | An array of custom recipe names to be run following a @shutdown@ event.
recipes_shutdown :: Lens.Lens' Recipes (Prelude.Maybe [Prelude.Text])
recipes_shutdown = Lens.lens (\Recipes' {shutdown} -> shutdown) (\s@Recipes' {} a -> s {shutdown = a} :: Recipes) Prelude.. Lens.mapping Lens._Coerce

-- | An array of custom recipe names to be run following a @configure@ event.
recipes_configure :: Lens.Lens' Recipes (Prelude.Maybe [Prelude.Text])
recipes_configure = Lens.lens (\Recipes' {configure} -> configure) (\s@Recipes' {} a -> s {configure = a} :: Recipes) Prelude.. Lens.mapping Lens._Coerce

-- | An array of custom recipe names to be run following a @undeploy@ event.
recipes_undeploy :: Lens.Lens' Recipes (Prelude.Maybe [Prelude.Text])
recipes_undeploy = Lens.lens (\Recipes' {undeploy} -> undeploy) (\s@Recipes' {} a -> s {undeploy = a} :: Recipes) Prelude.. Lens.mapping Lens._Coerce

-- | An array of custom recipe names to be run following a @setup@ event.
recipes_setup :: Lens.Lens' Recipes (Prelude.Maybe [Prelude.Text])
recipes_setup = Lens.lens (\Recipes' {setup} -> setup) (\s@Recipes' {} a -> s {setup = a} :: Recipes) Prelude.. Lens.mapping Lens._Coerce

-- | An array of custom recipe names to be run following a @deploy@ event.
recipes_deploy :: Lens.Lens' Recipes (Prelude.Maybe [Prelude.Text])
recipes_deploy = Lens.lens (\Recipes' {deploy} -> deploy) (\s@Recipes' {} a -> s {deploy = a} :: Recipes) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON Recipes where
  parseJSON =
    Core.withObject
      "Recipes"
      ( \x ->
          Recipes'
            Prelude.<$> (x Core..:? "Shutdown" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Configure" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Undeploy" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Setup" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Deploy" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Recipes

instance Prelude.NFData Recipes

instance Core.ToJSON Recipes where
  toJSON Recipes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Shutdown" Core..=) Prelude.<$> shutdown,
            ("Configure" Core..=) Prelude.<$> configure,
            ("Undeploy" Core..=) Prelude.<$> undeploy,
            ("Setup" Core..=) Prelude.<$> setup,
            ("Deploy" Core..=) Prelude.<$> deploy
          ]
      )
