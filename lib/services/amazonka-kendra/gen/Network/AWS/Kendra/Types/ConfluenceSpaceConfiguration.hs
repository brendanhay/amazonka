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
-- Module      : Network.AWS.Kendra.Types.ConfluenceSpaceConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.ConfluenceSpaceConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.ConfluenceSpaceToIndexFieldMapping
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the configuration for indexing Confluence spaces.
--
-- /See:/ 'newConfluenceSpaceConfiguration' smart constructor.
data ConfluenceSpaceConfiguration = ConfluenceSpaceConfiguration'
  { -- | Specifies whether Amazon Kendra should index archived spaces.
    crawlArchivedSpaces :: Prelude.Maybe Prelude.Bool,
    -- | Defines how space metadata fields should be mapped to index fields.
    -- Before you can map a field, you must first create an index field with a
    -- matching type using the console or the @UpdateIndex@ operation.
    --
    -- If you specify the @SpaceFieldMappings@ parameter, you must specify at
    -- least one field mapping.
    spaceFieldMappings :: Prelude.Maybe (Prelude.NonEmpty ConfluenceSpaceToIndexFieldMapping),
    -- | Specifies whether Amazon Kendra should index personal spaces. Users can
    -- add restrictions to items in personal spaces. If personal spaces are
    -- indexed, queries without user context information may return restricted
    -- items from a personal space in their results. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/user-context-filter.html Filtering on user context>.
    crawlPersonalSpaces :: Prelude.Maybe Prelude.Bool,
    -- | A list of space keys for Confluence spaces. If you include a key, the
    -- blogs, documents, and attachments in the space are indexed. Spaces that
    -- aren\'t in the list aren\'t indexed. A space in the list must exist.
    -- Otherwise, Amazon Kendra logs an error when the data source is
    -- synchronized. If a space is in both the @IncludeSpaces@ and the
    -- @ExcludeSpaces@ list, the space is excluded.
    includeSpaces :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of space keys of Confluence spaces. If you include a key, the
    -- blogs, documents, and attachments in the space are not indexed. If a
    -- space is in both the @ExcludeSpaces@ and the @IncludeSpaces@ list, the
    -- space is excluded.
    excludeSpaces :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
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
-- 'crawlArchivedSpaces', 'confluenceSpaceConfiguration_crawlArchivedSpaces' - Specifies whether Amazon Kendra should index archived spaces.
--
-- 'spaceFieldMappings', 'confluenceSpaceConfiguration_spaceFieldMappings' - Defines how space metadata fields should be mapped to index fields.
-- Before you can map a field, you must first create an index field with a
-- matching type using the console or the @UpdateIndex@ operation.
--
-- If you specify the @SpaceFieldMappings@ parameter, you must specify at
-- least one field mapping.
--
-- 'crawlPersonalSpaces', 'confluenceSpaceConfiguration_crawlPersonalSpaces' - Specifies whether Amazon Kendra should index personal spaces. Users can
-- add restrictions to items in personal spaces. If personal spaces are
-- indexed, queries without user context information may return restricted
-- items from a personal space in their results. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/user-context-filter.html Filtering on user context>.
--
-- 'includeSpaces', 'confluenceSpaceConfiguration_includeSpaces' - A list of space keys for Confluence spaces. If you include a key, the
-- blogs, documents, and attachments in the space are indexed. Spaces that
-- aren\'t in the list aren\'t indexed. A space in the list must exist.
-- Otherwise, Amazon Kendra logs an error when the data source is
-- synchronized. If a space is in both the @IncludeSpaces@ and the
-- @ExcludeSpaces@ list, the space is excluded.
--
-- 'excludeSpaces', 'confluenceSpaceConfiguration_excludeSpaces' - A list of space keys of Confluence spaces. If you include a key, the
-- blogs, documents, and attachments in the space are not indexed. If a
-- space is in both the @ExcludeSpaces@ and the @IncludeSpaces@ list, the
-- space is excluded.
newConfluenceSpaceConfiguration ::
  ConfluenceSpaceConfiguration
newConfluenceSpaceConfiguration =
  ConfluenceSpaceConfiguration'
    { crawlArchivedSpaces =
        Prelude.Nothing,
      spaceFieldMappings = Prelude.Nothing,
      crawlPersonalSpaces = Prelude.Nothing,
      includeSpaces = Prelude.Nothing,
      excludeSpaces = Prelude.Nothing
    }

-- | Specifies whether Amazon Kendra should index archived spaces.
confluenceSpaceConfiguration_crawlArchivedSpaces :: Lens.Lens' ConfluenceSpaceConfiguration (Prelude.Maybe Prelude.Bool)
confluenceSpaceConfiguration_crawlArchivedSpaces = Lens.lens (\ConfluenceSpaceConfiguration' {crawlArchivedSpaces} -> crawlArchivedSpaces) (\s@ConfluenceSpaceConfiguration' {} a -> s {crawlArchivedSpaces = a} :: ConfluenceSpaceConfiguration)

-- | Defines how space metadata fields should be mapped to index fields.
-- Before you can map a field, you must first create an index field with a
-- matching type using the console or the @UpdateIndex@ operation.
--
-- If you specify the @SpaceFieldMappings@ parameter, you must specify at
-- least one field mapping.
confluenceSpaceConfiguration_spaceFieldMappings :: Lens.Lens' ConfluenceSpaceConfiguration (Prelude.Maybe (Prelude.NonEmpty ConfluenceSpaceToIndexFieldMapping))
confluenceSpaceConfiguration_spaceFieldMappings = Lens.lens (\ConfluenceSpaceConfiguration' {spaceFieldMappings} -> spaceFieldMappings) (\s@ConfluenceSpaceConfiguration' {} a -> s {spaceFieldMappings = a} :: ConfluenceSpaceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether Amazon Kendra should index personal spaces. Users can
-- add restrictions to items in personal spaces. If personal spaces are
-- indexed, queries without user context information may return restricted
-- items from a personal space in their results. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/user-context-filter.html Filtering on user context>.
confluenceSpaceConfiguration_crawlPersonalSpaces :: Lens.Lens' ConfluenceSpaceConfiguration (Prelude.Maybe Prelude.Bool)
confluenceSpaceConfiguration_crawlPersonalSpaces = Lens.lens (\ConfluenceSpaceConfiguration' {crawlPersonalSpaces} -> crawlPersonalSpaces) (\s@ConfluenceSpaceConfiguration' {} a -> s {crawlPersonalSpaces = a} :: ConfluenceSpaceConfiguration)

-- | A list of space keys for Confluence spaces. If you include a key, the
-- blogs, documents, and attachments in the space are indexed. Spaces that
-- aren\'t in the list aren\'t indexed. A space in the list must exist.
-- Otherwise, Amazon Kendra logs an error when the data source is
-- synchronized. If a space is in both the @IncludeSpaces@ and the
-- @ExcludeSpaces@ list, the space is excluded.
confluenceSpaceConfiguration_includeSpaces :: Lens.Lens' ConfluenceSpaceConfiguration (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
confluenceSpaceConfiguration_includeSpaces = Lens.lens (\ConfluenceSpaceConfiguration' {includeSpaces} -> includeSpaces) (\s@ConfluenceSpaceConfiguration' {} a -> s {includeSpaces = a} :: ConfluenceSpaceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of space keys of Confluence spaces. If you include a key, the
-- blogs, documents, and attachments in the space are not indexed. If a
-- space is in both the @ExcludeSpaces@ and the @IncludeSpaces@ list, the
-- space is excluded.
confluenceSpaceConfiguration_excludeSpaces :: Lens.Lens' ConfluenceSpaceConfiguration (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
confluenceSpaceConfiguration_excludeSpaces = Lens.lens (\ConfluenceSpaceConfiguration' {excludeSpaces} -> excludeSpaces) (\s@ConfluenceSpaceConfiguration' {} a -> s {excludeSpaces = a} :: ConfluenceSpaceConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ConfluenceSpaceConfiguration where
  parseJSON =
    Core.withObject
      "ConfluenceSpaceConfiguration"
      ( \x ->
          ConfluenceSpaceConfiguration'
            Prelude.<$> (x Core..:? "CrawlArchivedSpaces")
            Prelude.<*> (x Core..:? "SpaceFieldMappings")
            Prelude.<*> (x Core..:? "CrawlPersonalSpaces")
            Prelude.<*> (x Core..:? "IncludeSpaces")
            Prelude.<*> (x Core..:? "ExcludeSpaces")
      )

instance
  Prelude.Hashable
    ConfluenceSpaceConfiguration

instance Prelude.NFData ConfluenceSpaceConfiguration

instance Core.ToJSON ConfluenceSpaceConfiguration where
  toJSON ConfluenceSpaceConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CrawlArchivedSpaces" Core..=)
              Prelude.<$> crawlArchivedSpaces,
            ("SpaceFieldMappings" Core..=)
              Prelude.<$> spaceFieldMappings,
            ("CrawlPersonalSpaces" Core..=)
              Prelude.<$> crawlPersonalSpaces,
            ("IncludeSpaces" Core..=) Prelude.<$> includeSpaces,
            ("ExcludeSpaces" Core..=) Prelude.<$> excludeSpaces
          ]
      )
