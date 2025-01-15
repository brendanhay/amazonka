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
-- Module      : Amazonka.Evidently.Types.LaunchGroupConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.LaunchGroupConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that defines one launch group in a launch. A launch group is
-- a variation of the feature that you are including in the launch.
--
-- /See:/ 'newLaunchGroupConfig' smart constructor.
data LaunchGroupConfig = LaunchGroupConfig'
  { -- | A description of the launch group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The feature that this launch is using.
    feature :: Prelude.Text,
    -- | A name for this launch group.
    name :: Prelude.Text,
    -- | The feature variation to use for this launch group.
    variation :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchGroupConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'launchGroupConfig_description' - A description of the launch group.
--
-- 'feature', 'launchGroupConfig_feature' - The feature that this launch is using.
--
-- 'name', 'launchGroupConfig_name' - A name for this launch group.
--
-- 'variation', 'launchGroupConfig_variation' - The feature variation to use for this launch group.
newLaunchGroupConfig ::
  -- | 'feature'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'variation'
  Prelude.Text ->
  LaunchGroupConfig
newLaunchGroupConfig pFeature_ pName_ pVariation_ =
  LaunchGroupConfig'
    { description = Prelude.Nothing,
      feature = pFeature_,
      name = pName_,
      variation = pVariation_
    }

-- | A description of the launch group.
launchGroupConfig_description :: Lens.Lens' LaunchGroupConfig (Prelude.Maybe Prelude.Text)
launchGroupConfig_description = Lens.lens (\LaunchGroupConfig' {description} -> description) (\s@LaunchGroupConfig' {} a -> s {description = a} :: LaunchGroupConfig)

-- | The feature that this launch is using.
launchGroupConfig_feature :: Lens.Lens' LaunchGroupConfig Prelude.Text
launchGroupConfig_feature = Lens.lens (\LaunchGroupConfig' {feature} -> feature) (\s@LaunchGroupConfig' {} a -> s {feature = a} :: LaunchGroupConfig)

-- | A name for this launch group.
launchGroupConfig_name :: Lens.Lens' LaunchGroupConfig Prelude.Text
launchGroupConfig_name = Lens.lens (\LaunchGroupConfig' {name} -> name) (\s@LaunchGroupConfig' {} a -> s {name = a} :: LaunchGroupConfig)

-- | The feature variation to use for this launch group.
launchGroupConfig_variation :: Lens.Lens' LaunchGroupConfig Prelude.Text
launchGroupConfig_variation = Lens.lens (\LaunchGroupConfig' {variation} -> variation) (\s@LaunchGroupConfig' {} a -> s {variation = a} :: LaunchGroupConfig)

instance Prelude.Hashable LaunchGroupConfig where
  hashWithSalt _salt LaunchGroupConfig' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` feature
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` variation

instance Prelude.NFData LaunchGroupConfig where
  rnf LaunchGroupConfig' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf feature `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf variation

instance Data.ToJSON LaunchGroupConfig where
  toJSON LaunchGroupConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            Prelude.Just ("feature" Data..= feature),
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("variation" Data..= variation)
          ]
      )
