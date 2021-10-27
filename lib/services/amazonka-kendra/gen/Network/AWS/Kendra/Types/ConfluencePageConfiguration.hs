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
-- Module      : Network.AWS.Kendra.Types.ConfluencePageConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.ConfluencePageConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.ConfluencePageToIndexFieldMapping
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the page settings for the Confluence data source.
--
-- /See:/ 'newConfluencePageConfiguration' smart constructor.
data ConfluencePageConfiguration = ConfluencePageConfiguration'
  { -- | Defines how page metadata fields should be mapped to index fields.
    -- Before you can map a field, you must first create an index field with a
    -- matching type using the console or the @UpdateIndex@ operation.
    --
    -- If you specify the @PageFieldMappings@ parameter, you must specify at
    -- least one field mapping.
    pageFieldMappings :: Prelude.Maybe (Prelude.NonEmpty ConfluencePageToIndexFieldMapping)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfluencePageConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageFieldMappings', 'confluencePageConfiguration_pageFieldMappings' - Defines how page metadata fields should be mapped to index fields.
-- Before you can map a field, you must first create an index field with a
-- matching type using the console or the @UpdateIndex@ operation.
--
-- If you specify the @PageFieldMappings@ parameter, you must specify at
-- least one field mapping.
newConfluencePageConfiguration ::
  ConfluencePageConfiguration
newConfluencePageConfiguration =
  ConfluencePageConfiguration'
    { pageFieldMappings =
        Prelude.Nothing
    }

-- | Defines how page metadata fields should be mapped to index fields.
-- Before you can map a field, you must first create an index field with a
-- matching type using the console or the @UpdateIndex@ operation.
--
-- If you specify the @PageFieldMappings@ parameter, you must specify at
-- least one field mapping.
confluencePageConfiguration_pageFieldMappings :: Lens.Lens' ConfluencePageConfiguration (Prelude.Maybe (Prelude.NonEmpty ConfluencePageToIndexFieldMapping))
confluencePageConfiguration_pageFieldMappings = Lens.lens (\ConfluencePageConfiguration' {pageFieldMappings} -> pageFieldMappings) (\s@ConfluencePageConfiguration' {} a -> s {pageFieldMappings = a} :: ConfluencePageConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ConfluencePageConfiguration where
  parseJSON =
    Core.withObject
      "ConfluencePageConfiguration"
      ( \x ->
          ConfluencePageConfiguration'
            Prelude.<$> (x Core..:? "PageFieldMappings")
      )

instance Prelude.Hashable ConfluencePageConfiguration

instance Prelude.NFData ConfluencePageConfiguration

instance Core.ToJSON ConfluencePageConfiguration where
  toJSON ConfluencePageConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PageFieldMappings" Core..=)
              Prelude.<$> pageFieldMappings
          ]
      )
