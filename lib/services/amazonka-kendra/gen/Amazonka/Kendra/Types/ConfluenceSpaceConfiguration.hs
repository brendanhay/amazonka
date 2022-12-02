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
-- Module      : Amazonka.Kendra.Types.ConfluenceSpaceConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.ConfluenceSpaceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.ConfluenceSpaceToIndexFieldMapping
import qualified Amazonka.Prelude as Prelude

-- | Configuration information for indexing Confluence spaces.
--
-- /See:/ 'newConfluenceSpaceConfiguration' smart constructor.
data ConfluenceSpaceConfiguration = ConfluenceSpaceConfiguration'
  { -- | @TRUE@ to index personal spaces. You can add restrictions to items in
    -- personal spaces. If personal spaces are indexed, queries without user
    -- context information may return restricted items from a personal space in
    -- their results. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/user-context-filter.html Filtering on user context>.
    crawlPersonalSpaces :: Prelude.Maybe Prelude.Bool,
    -- | @TRUE@ to index archived spaces.
    crawlArchivedSpaces :: Prelude.Maybe Prelude.Bool,
    -- | A list of space keys of Confluence spaces. If you include a key, the
    -- blogs, documents, and attachments in the space are not indexed. If a
    -- space is in both the @ExcludeSpaces@ and the @IncludeSpaces@ list, the
    -- space is excluded.
    excludeSpaces :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Maps attributes or field names of Confluence spaces to Amazon Kendra
    -- index field names. To create custom fields, use the @UpdateIndex@ API
    -- before you map to Confluence fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The Confluence data source field names must exist in your Confluence
    -- custom metadata.
    --
    -- If you specify the @SpaceFieldMappings@ parameter, you must specify at
    -- least one field mapping.
    spaceFieldMappings :: Prelude.Maybe (Prelude.NonEmpty ConfluenceSpaceToIndexFieldMapping),
    -- | A list of space keys for Confluence spaces. If you include a key, the
    -- blogs, documents, and attachments in the space are indexed. Spaces that
    -- aren\'t in the list aren\'t indexed. A space in the list must exist.
    -- Otherwise, Amazon Kendra logs an error when the data source is
    -- synchronized. If a space is in both the @IncludeSpaces@ and the
    -- @ExcludeSpaces@ list, the space is excluded.
    includeSpaces :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfluenceSpaceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crawlPersonalSpaces', 'confluenceSpaceConfiguration_crawlPersonalSpaces' - @TRUE@ to index personal spaces. You can add restrictions to items in
-- personal spaces. If personal spaces are indexed, queries without user
-- context information may return restricted items from a personal space in
-- their results. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/user-context-filter.html Filtering on user context>.
--
-- 'crawlArchivedSpaces', 'confluenceSpaceConfiguration_crawlArchivedSpaces' - @TRUE@ to index archived spaces.
--
-- 'excludeSpaces', 'confluenceSpaceConfiguration_excludeSpaces' - A list of space keys of Confluence spaces. If you include a key, the
-- blogs, documents, and attachments in the space are not indexed. If a
-- space is in both the @ExcludeSpaces@ and the @IncludeSpaces@ list, the
-- space is excluded.
--
-- 'spaceFieldMappings', 'confluenceSpaceConfiguration_spaceFieldMappings' - Maps attributes or field names of Confluence spaces to Amazon Kendra
-- index field names. To create custom fields, use the @UpdateIndex@ API
-- before you map to Confluence fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Confluence data source field names must exist in your Confluence
-- custom metadata.
--
-- If you specify the @SpaceFieldMappings@ parameter, you must specify at
-- least one field mapping.
--
-- 'includeSpaces', 'confluenceSpaceConfiguration_includeSpaces' - A list of space keys for Confluence spaces. If you include a key, the
-- blogs, documents, and attachments in the space are indexed. Spaces that
-- aren\'t in the list aren\'t indexed. A space in the list must exist.
-- Otherwise, Amazon Kendra logs an error when the data source is
-- synchronized. If a space is in both the @IncludeSpaces@ and the
-- @ExcludeSpaces@ list, the space is excluded.
newConfluenceSpaceConfiguration ::
  ConfluenceSpaceConfiguration
newConfluenceSpaceConfiguration =
  ConfluenceSpaceConfiguration'
    { crawlPersonalSpaces =
        Prelude.Nothing,
      crawlArchivedSpaces = Prelude.Nothing,
      excludeSpaces = Prelude.Nothing,
      spaceFieldMappings = Prelude.Nothing,
      includeSpaces = Prelude.Nothing
    }

-- | @TRUE@ to index personal spaces. You can add restrictions to items in
-- personal spaces. If personal spaces are indexed, queries without user
-- context information may return restricted items from a personal space in
-- their results. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/user-context-filter.html Filtering on user context>.
confluenceSpaceConfiguration_crawlPersonalSpaces :: Lens.Lens' ConfluenceSpaceConfiguration (Prelude.Maybe Prelude.Bool)
confluenceSpaceConfiguration_crawlPersonalSpaces = Lens.lens (\ConfluenceSpaceConfiguration' {crawlPersonalSpaces} -> crawlPersonalSpaces) (\s@ConfluenceSpaceConfiguration' {} a -> s {crawlPersonalSpaces = a} :: ConfluenceSpaceConfiguration)

-- | @TRUE@ to index archived spaces.
confluenceSpaceConfiguration_crawlArchivedSpaces :: Lens.Lens' ConfluenceSpaceConfiguration (Prelude.Maybe Prelude.Bool)
confluenceSpaceConfiguration_crawlArchivedSpaces = Lens.lens (\ConfluenceSpaceConfiguration' {crawlArchivedSpaces} -> crawlArchivedSpaces) (\s@ConfluenceSpaceConfiguration' {} a -> s {crawlArchivedSpaces = a} :: ConfluenceSpaceConfiguration)

-- | A list of space keys of Confluence spaces. If you include a key, the
-- blogs, documents, and attachments in the space are not indexed. If a
-- space is in both the @ExcludeSpaces@ and the @IncludeSpaces@ list, the
-- space is excluded.
confluenceSpaceConfiguration_excludeSpaces :: Lens.Lens' ConfluenceSpaceConfiguration (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
confluenceSpaceConfiguration_excludeSpaces = Lens.lens (\ConfluenceSpaceConfiguration' {excludeSpaces} -> excludeSpaces) (\s@ConfluenceSpaceConfiguration' {} a -> s {excludeSpaces = a} :: ConfluenceSpaceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Maps attributes or field names of Confluence spaces to Amazon Kendra
-- index field names. To create custom fields, use the @UpdateIndex@ API
-- before you map to Confluence fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Confluence data source field names must exist in your Confluence
-- custom metadata.
--
-- If you specify the @SpaceFieldMappings@ parameter, you must specify at
-- least one field mapping.
confluenceSpaceConfiguration_spaceFieldMappings :: Lens.Lens' ConfluenceSpaceConfiguration (Prelude.Maybe (Prelude.NonEmpty ConfluenceSpaceToIndexFieldMapping))
confluenceSpaceConfiguration_spaceFieldMappings = Lens.lens (\ConfluenceSpaceConfiguration' {spaceFieldMappings} -> spaceFieldMappings) (\s@ConfluenceSpaceConfiguration' {} a -> s {spaceFieldMappings = a} :: ConfluenceSpaceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of space keys for Confluence spaces. If you include a key, the
-- blogs, documents, and attachments in the space are indexed. Spaces that
-- aren\'t in the list aren\'t indexed. A space in the list must exist.
-- Otherwise, Amazon Kendra logs an error when the data source is
-- synchronized. If a space is in both the @IncludeSpaces@ and the
-- @ExcludeSpaces@ list, the space is excluded.
confluenceSpaceConfiguration_includeSpaces :: Lens.Lens' ConfluenceSpaceConfiguration (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
confluenceSpaceConfiguration_includeSpaces = Lens.lens (\ConfluenceSpaceConfiguration' {includeSpaces} -> includeSpaces) (\s@ConfluenceSpaceConfiguration' {} a -> s {includeSpaces = a} :: ConfluenceSpaceConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ConfluenceSpaceConfiguration where
  parseJSON =
    Data.withObject
      "ConfluenceSpaceConfiguration"
      ( \x ->
          ConfluenceSpaceConfiguration'
            Prelude.<$> (x Data..:? "CrawlPersonalSpaces")
            Prelude.<*> (x Data..:? "CrawlArchivedSpaces")
            Prelude.<*> (x Data..:? "ExcludeSpaces")
            Prelude.<*> (x Data..:? "SpaceFieldMappings")
            Prelude.<*> (x Data..:? "IncludeSpaces")
      )

instance
  Prelude.Hashable
    ConfluenceSpaceConfiguration
  where
  hashWithSalt _salt ConfluenceSpaceConfiguration' {..} =
    _salt `Prelude.hashWithSalt` crawlPersonalSpaces
      `Prelude.hashWithSalt` crawlArchivedSpaces
      `Prelude.hashWithSalt` excludeSpaces
      `Prelude.hashWithSalt` spaceFieldMappings
      `Prelude.hashWithSalt` includeSpaces

instance Prelude.NFData ConfluenceSpaceConfiguration where
  rnf ConfluenceSpaceConfiguration' {..} =
    Prelude.rnf crawlPersonalSpaces
      `Prelude.seq` Prelude.rnf crawlArchivedSpaces
      `Prelude.seq` Prelude.rnf excludeSpaces
      `Prelude.seq` Prelude.rnf spaceFieldMappings
      `Prelude.seq` Prelude.rnf includeSpaces

instance Data.ToJSON ConfluenceSpaceConfiguration where
  toJSON ConfluenceSpaceConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CrawlPersonalSpaces" Data..=)
              Prelude.<$> crawlPersonalSpaces,
            ("CrawlArchivedSpaces" Data..=)
              Prelude.<$> crawlArchivedSpaces,
            ("ExcludeSpaces" Data..=) Prelude.<$> excludeSpaces,
            ("SpaceFieldMappings" Data..=)
              Prelude.<$> spaceFieldMappings,
            ("IncludeSpaces" Data..=) Prelude.<$> includeSpaces
          ]
      )
