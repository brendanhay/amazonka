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
-- Module      : Amazonka.QuickSight.Types.DefaultPaginatedLayoutConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DefaultPaginatedLayoutConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DefaultSectionBasedLayoutConfiguration

-- | The options that determine the default settings for a paginated layout
-- configuration.
--
-- /See:/ 'newDefaultPaginatedLayoutConfiguration' smart constructor.
data DefaultPaginatedLayoutConfiguration = DefaultPaginatedLayoutConfiguration'
  { -- | The options that determine the default settings for a section-based
    -- layout configuration.
    sectionBased :: Prelude.Maybe DefaultSectionBasedLayoutConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefaultPaginatedLayoutConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sectionBased', 'defaultPaginatedLayoutConfiguration_sectionBased' - The options that determine the default settings for a section-based
-- layout configuration.
newDefaultPaginatedLayoutConfiguration ::
  DefaultPaginatedLayoutConfiguration
newDefaultPaginatedLayoutConfiguration =
  DefaultPaginatedLayoutConfiguration'
    { sectionBased =
        Prelude.Nothing
    }

-- | The options that determine the default settings for a section-based
-- layout configuration.
defaultPaginatedLayoutConfiguration_sectionBased :: Lens.Lens' DefaultPaginatedLayoutConfiguration (Prelude.Maybe DefaultSectionBasedLayoutConfiguration)
defaultPaginatedLayoutConfiguration_sectionBased = Lens.lens (\DefaultPaginatedLayoutConfiguration' {sectionBased} -> sectionBased) (\s@DefaultPaginatedLayoutConfiguration' {} a -> s {sectionBased = a} :: DefaultPaginatedLayoutConfiguration)

instance
  Data.FromJSON
    DefaultPaginatedLayoutConfiguration
  where
  parseJSON =
    Data.withObject
      "DefaultPaginatedLayoutConfiguration"
      ( \x ->
          DefaultPaginatedLayoutConfiguration'
            Prelude.<$> (x Data..:? "SectionBased")
      )

instance
  Prelude.Hashable
    DefaultPaginatedLayoutConfiguration
  where
  hashWithSalt
    _salt
    DefaultPaginatedLayoutConfiguration' {..} =
      _salt `Prelude.hashWithSalt` sectionBased

instance
  Prelude.NFData
    DefaultPaginatedLayoutConfiguration
  where
  rnf DefaultPaginatedLayoutConfiguration' {..} =
    Prelude.rnf sectionBased

instance
  Data.ToJSON
    DefaultPaginatedLayoutConfiguration
  where
  toJSON DefaultPaginatedLayoutConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("SectionBased" Data..=) Prelude.<$> sectionBased]
      )
