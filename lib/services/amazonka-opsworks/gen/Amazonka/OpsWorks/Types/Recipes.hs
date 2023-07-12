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
-- Module      : Amazonka.OpsWorks.Types.Recipes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.Recipes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  { -- | An array of custom recipe names to be run following a @configure@ event.
    configure :: Prelude.Maybe [Prelude.Text],
    -- | An array of custom recipe names to be run following a @deploy@ event.
    deploy :: Prelude.Maybe [Prelude.Text],
    -- | An array of custom recipe names to be run following a @setup@ event.
    setup :: Prelude.Maybe [Prelude.Text],
    -- | An array of custom recipe names to be run following a @shutdown@ event.
    shutdown :: Prelude.Maybe [Prelude.Text],
    -- | An array of custom recipe names to be run following a @undeploy@ event.
    undeploy :: Prelude.Maybe [Prelude.Text]
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
-- 'configure', 'recipes_configure' - An array of custom recipe names to be run following a @configure@ event.
--
-- 'deploy', 'recipes_deploy' - An array of custom recipe names to be run following a @deploy@ event.
--
-- 'setup', 'recipes_setup' - An array of custom recipe names to be run following a @setup@ event.
--
-- 'shutdown', 'recipes_shutdown' - An array of custom recipe names to be run following a @shutdown@ event.
--
-- 'undeploy', 'recipes_undeploy' - An array of custom recipe names to be run following a @undeploy@ event.
newRecipes ::
  Recipes
newRecipes =
  Recipes'
    { configure = Prelude.Nothing,
      deploy = Prelude.Nothing,
      setup = Prelude.Nothing,
      shutdown = Prelude.Nothing,
      undeploy = Prelude.Nothing
    }

-- | An array of custom recipe names to be run following a @configure@ event.
recipes_configure :: Lens.Lens' Recipes (Prelude.Maybe [Prelude.Text])
recipes_configure = Lens.lens (\Recipes' {configure} -> configure) (\s@Recipes' {} a -> s {configure = a} :: Recipes) Prelude.. Lens.mapping Lens.coerced

-- | An array of custom recipe names to be run following a @deploy@ event.
recipes_deploy :: Lens.Lens' Recipes (Prelude.Maybe [Prelude.Text])
recipes_deploy = Lens.lens (\Recipes' {deploy} -> deploy) (\s@Recipes' {} a -> s {deploy = a} :: Recipes) Prelude.. Lens.mapping Lens.coerced

-- | An array of custom recipe names to be run following a @setup@ event.
recipes_setup :: Lens.Lens' Recipes (Prelude.Maybe [Prelude.Text])
recipes_setup = Lens.lens (\Recipes' {setup} -> setup) (\s@Recipes' {} a -> s {setup = a} :: Recipes) Prelude.. Lens.mapping Lens.coerced

-- | An array of custom recipe names to be run following a @shutdown@ event.
recipes_shutdown :: Lens.Lens' Recipes (Prelude.Maybe [Prelude.Text])
recipes_shutdown = Lens.lens (\Recipes' {shutdown} -> shutdown) (\s@Recipes' {} a -> s {shutdown = a} :: Recipes) Prelude.. Lens.mapping Lens.coerced

-- | An array of custom recipe names to be run following a @undeploy@ event.
recipes_undeploy :: Lens.Lens' Recipes (Prelude.Maybe [Prelude.Text])
recipes_undeploy = Lens.lens (\Recipes' {undeploy} -> undeploy) (\s@Recipes' {} a -> s {undeploy = a} :: Recipes) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Recipes where
  parseJSON =
    Data.withObject
      "Recipes"
      ( \x ->
          Recipes'
            Prelude.<$> (x Data..:? "Configure" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Deploy" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Setup" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Shutdown" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Undeploy" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Recipes where
  hashWithSalt _salt Recipes' {..} =
    _salt
      `Prelude.hashWithSalt` configure
      `Prelude.hashWithSalt` deploy
      `Prelude.hashWithSalt` setup
      `Prelude.hashWithSalt` shutdown
      `Prelude.hashWithSalt` undeploy

instance Prelude.NFData Recipes where
  rnf Recipes' {..} =
    Prelude.rnf configure
      `Prelude.seq` Prelude.rnf deploy
      `Prelude.seq` Prelude.rnf setup
      `Prelude.seq` Prelude.rnf shutdown
      `Prelude.seq` Prelude.rnf undeploy

instance Data.ToJSON Recipes where
  toJSON Recipes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Configure" Data..=) Prelude.<$> configure,
            ("Deploy" Data..=) Prelude.<$> deploy,
            ("Setup" Data..=) Prelude.<$> setup,
            ("Shutdown" Data..=) Prelude.<$> shutdown,
            ("Undeploy" Data..=) Prelude.<$> undeploy
          ]
      )
