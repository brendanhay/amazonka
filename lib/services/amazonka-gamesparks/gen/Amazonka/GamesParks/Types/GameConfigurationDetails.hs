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
-- Module      : Amazonka.GamesParks.Types.GameConfigurationDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GamesParks.Types.GameConfigurationDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GamesParks.Types.Section
import qualified Amazonka.Prelude as Prelude

-- | Details about the game configuration.
--
-- The game configuration is organized into named sections, where the
-- schema of each section is defined by an extension. The schema for these
-- sections can be retrieved using the @GetExtensionVersion@ operation.
--
-- /See:/ 'newGameConfigurationDetails' smart constructor.
data GameConfigurationDetails = GameConfigurationDetails'
  { -- | The date when the game was created.
    created :: Prelude.Maybe Core.POSIX,
    -- | Configuration data, organized by section name.
    sections :: Prelude.Maybe (Prelude.HashMap Prelude.Text Section),
    -- | The date when the game was last modified.
    lastUpdated :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GameConfigurationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'created', 'gameConfigurationDetails_created' - The date when the game was created.
--
-- 'sections', 'gameConfigurationDetails_sections' - Configuration data, organized by section name.
--
-- 'lastUpdated', 'gameConfigurationDetails_lastUpdated' - The date when the game was last modified.
newGameConfigurationDetails ::
  GameConfigurationDetails
newGameConfigurationDetails =
  GameConfigurationDetails'
    { created =
        Prelude.Nothing,
      sections = Prelude.Nothing,
      lastUpdated = Prelude.Nothing
    }

-- | The date when the game was created.
gameConfigurationDetails_created :: Lens.Lens' GameConfigurationDetails (Prelude.Maybe Prelude.UTCTime)
gameConfigurationDetails_created = Lens.lens (\GameConfigurationDetails' {created} -> created) (\s@GameConfigurationDetails' {} a -> s {created = a} :: GameConfigurationDetails) Prelude.. Lens.mapping Core._Time

-- | Configuration data, organized by section name.
gameConfigurationDetails_sections :: Lens.Lens' GameConfigurationDetails (Prelude.Maybe (Prelude.HashMap Prelude.Text Section))
gameConfigurationDetails_sections = Lens.lens (\GameConfigurationDetails' {sections} -> sections) (\s@GameConfigurationDetails' {} a -> s {sections = a} :: GameConfigurationDetails) Prelude.. Lens.mapping Lens.coerced

-- | The date when the game was last modified.
gameConfigurationDetails_lastUpdated :: Lens.Lens' GameConfigurationDetails (Prelude.Maybe Prelude.UTCTime)
gameConfigurationDetails_lastUpdated = Lens.lens (\GameConfigurationDetails' {lastUpdated} -> lastUpdated) (\s@GameConfigurationDetails' {} a -> s {lastUpdated = a} :: GameConfigurationDetails) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON GameConfigurationDetails where
  parseJSON =
    Core.withObject
      "GameConfigurationDetails"
      ( \x ->
          GameConfigurationDetails'
            Prelude.<$> (x Core..:? "Created")
            Prelude.<*> (x Core..:? "Sections" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "LastUpdated")
      )

instance Prelude.Hashable GameConfigurationDetails where
  hashWithSalt _salt GameConfigurationDetails' {..} =
    _salt `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` sections
      `Prelude.hashWithSalt` lastUpdated

instance Prelude.NFData GameConfigurationDetails where
  rnf GameConfigurationDetails' {..} =
    Prelude.rnf created
      `Prelude.seq` Prelude.rnf sections
      `Prelude.seq` Prelude.rnf lastUpdated
