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
-- Module      : Amazonka.Kendra.Types.ConfluencePageConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.ConfluencePageConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.ConfluencePageToIndexFieldMapping
import qualified Amazonka.Prelude as Prelude

-- | Configuration of the page settings for the Confluence data source.
--
-- /See:/ 'newConfluencePageConfiguration' smart constructor.
data ConfluencePageConfiguration = ConfluencePageConfiguration'
  { -- | Maps attributes or field names of Confluence pages to Amazon Kendra
    -- index field names. To create custom fields, use the @UpdateIndex@ API
    -- before you map to Confluence fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The Confluence data source field names must exist in your Confluence
    -- custom metadata.
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
-- 'pageFieldMappings', 'confluencePageConfiguration_pageFieldMappings' - Maps attributes or field names of Confluence pages to Amazon Kendra
-- index field names. To create custom fields, use the @UpdateIndex@ API
-- before you map to Confluence fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Confluence data source field names must exist in your Confluence
-- custom metadata.
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

-- | Maps attributes or field names of Confluence pages to Amazon Kendra
-- index field names. To create custom fields, use the @UpdateIndex@ API
-- before you map to Confluence fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Confluence data source field names must exist in your Confluence
-- custom metadata.
--
-- If you specify the @PageFieldMappings@ parameter, you must specify at
-- least one field mapping.
confluencePageConfiguration_pageFieldMappings :: Lens.Lens' ConfluencePageConfiguration (Prelude.Maybe (Prelude.NonEmpty ConfluencePageToIndexFieldMapping))
confluencePageConfiguration_pageFieldMappings = Lens.lens (\ConfluencePageConfiguration' {pageFieldMappings} -> pageFieldMappings) (\s@ConfluencePageConfiguration' {} a -> s {pageFieldMappings = a} :: ConfluencePageConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ConfluencePageConfiguration where
  parseJSON =
    Data.withObject
      "ConfluencePageConfiguration"
      ( \x ->
          ConfluencePageConfiguration'
            Prelude.<$> (x Data..:? "PageFieldMappings")
      )

instance Prelude.Hashable ConfluencePageConfiguration where
  hashWithSalt _salt ConfluencePageConfiguration' {..} =
    _salt `Prelude.hashWithSalt` pageFieldMappings

instance Prelude.NFData ConfluencePageConfiguration where
  rnf ConfluencePageConfiguration' {..} =
    Prelude.rnf pageFieldMappings

instance Data.ToJSON ConfluencePageConfiguration where
  toJSON ConfluencePageConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PageFieldMappings" Data..=)
              Prelude.<$> pageFieldMappings
          ]
      )
