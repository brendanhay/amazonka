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
-- Module      : Amazonka.Evidently.Types.LaunchGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.LaunchGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that defines one launch group in a launch. A launch group is
-- a variation of the feature that you are including in the launch.
--
-- /See:/ 'newLaunchGroup' smart constructor.
data LaunchGroup = LaunchGroup'
  { -- | A description of the launch group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The feature variation for this launch group. This is a key-value pair.
    featureVariations :: Prelude.HashMap Prelude.Text Prelude.Text,
    -- | The name of the launch group.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'launchGroup_description' - A description of the launch group.
--
-- 'featureVariations', 'launchGroup_featureVariations' - The feature variation for this launch group. This is a key-value pair.
--
-- 'name', 'launchGroup_name' - The name of the launch group.
newLaunchGroup ::
  -- | 'name'
  Prelude.Text ->
  LaunchGroup
newLaunchGroup pName_ =
  LaunchGroup'
    { description = Prelude.Nothing,
      featureVariations = Prelude.mempty,
      name = pName_
    }

-- | A description of the launch group.
launchGroup_description :: Lens.Lens' LaunchGroup (Prelude.Maybe Prelude.Text)
launchGroup_description = Lens.lens (\LaunchGroup' {description} -> description) (\s@LaunchGroup' {} a -> s {description = a} :: LaunchGroup)

-- | The feature variation for this launch group. This is a key-value pair.
launchGroup_featureVariations :: Lens.Lens' LaunchGroup (Prelude.HashMap Prelude.Text Prelude.Text)
launchGroup_featureVariations = Lens.lens (\LaunchGroup' {featureVariations} -> featureVariations) (\s@LaunchGroup' {} a -> s {featureVariations = a} :: LaunchGroup) Prelude.. Lens.coerced

-- | The name of the launch group.
launchGroup_name :: Lens.Lens' LaunchGroup Prelude.Text
launchGroup_name = Lens.lens (\LaunchGroup' {name} -> name) (\s@LaunchGroup' {} a -> s {name = a} :: LaunchGroup)

instance Data.FromJSON LaunchGroup where
  parseJSON =
    Data.withObject
      "LaunchGroup"
      ( \x ->
          LaunchGroup'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> ( x
                            Data..:? "featureVariations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable LaunchGroup where
  hashWithSalt _salt LaunchGroup' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` featureVariations
      `Prelude.hashWithSalt` name

instance Prelude.NFData LaunchGroup where
  rnf LaunchGroup' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf featureVariations `Prelude.seq`
        Prelude.rnf name
