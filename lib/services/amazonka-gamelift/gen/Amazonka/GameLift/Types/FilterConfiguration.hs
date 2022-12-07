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
-- Module      : Amazonka.GameLift.Types.FilterConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.FilterConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of fleet locations where a game session queue can place new game
-- sessions. You can use a filter to temporarily turn off placements for
-- specific locations. For queues that have multi-location fleets, you can
-- use a filter configuration allow placement with some, but not all of
-- these locations.
--
-- Filter configurations are part of a GameSessionQueue.
--
-- /See:/ 'newFilterConfiguration' smart constructor.
data FilterConfiguration = FilterConfiguration'
  { -- | A list of locations to allow game session placement in, in the form of
    -- Amazon Web Services Region codes such as @us-west-2@.
    allowedLocations :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedLocations', 'filterConfiguration_allowedLocations' - A list of locations to allow game session placement in, in the form of
-- Amazon Web Services Region codes such as @us-west-2@.
newFilterConfiguration ::
  FilterConfiguration
newFilterConfiguration =
  FilterConfiguration'
    { allowedLocations =
        Prelude.Nothing
    }

-- | A list of locations to allow game session placement in, in the form of
-- Amazon Web Services Region codes such as @us-west-2@.
filterConfiguration_allowedLocations :: Lens.Lens' FilterConfiguration (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
filterConfiguration_allowedLocations = Lens.lens (\FilterConfiguration' {allowedLocations} -> allowedLocations) (\s@FilterConfiguration' {} a -> s {allowedLocations = a} :: FilterConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON FilterConfiguration where
  parseJSON =
    Data.withObject
      "FilterConfiguration"
      ( \x ->
          FilterConfiguration'
            Prelude.<$> (x Data..:? "AllowedLocations")
      )

instance Prelude.Hashable FilterConfiguration where
  hashWithSalt _salt FilterConfiguration' {..} =
    _salt `Prelude.hashWithSalt` allowedLocations

instance Prelude.NFData FilterConfiguration where
  rnf FilterConfiguration' {..} =
    Prelude.rnf allowedLocations

instance Data.ToJSON FilterConfiguration where
  toJSON FilterConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllowedLocations" Data..=)
              Prelude.<$> allowedLocations
          ]
      )
