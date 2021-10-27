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
-- Module      : Network.AWS.Kendra.Types.DocumentMetadataConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.DocumentMetadataConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.DocumentAttributeValueType
import Network.AWS.Kendra.Types.Relevance
import Network.AWS.Kendra.Types.Search
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the properties of a custom index field.
--
-- /See:/ 'newDocumentMetadataConfiguration' smart constructor.
data DocumentMetadataConfiguration = DocumentMetadataConfiguration'
  { -- | Provides manual tuning parameters to determine how the field affects the
    -- search results.
    relevance :: Prelude.Maybe Relevance,
    -- | Provides information about how the field is used during a search.
    search :: Prelude.Maybe Search,
    -- | The name of the index field.
    name :: Prelude.Text,
    -- | The data type of the index field.
    type' :: DocumentAttributeValueType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentMetadataConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relevance', 'documentMetadataConfiguration_relevance' - Provides manual tuning parameters to determine how the field affects the
-- search results.
--
-- 'search', 'documentMetadataConfiguration_search' - Provides information about how the field is used during a search.
--
-- 'name', 'documentMetadataConfiguration_name' - The name of the index field.
--
-- 'type'', 'documentMetadataConfiguration_type' - The data type of the index field.
newDocumentMetadataConfiguration ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  DocumentAttributeValueType ->
  DocumentMetadataConfiguration
newDocumentMetadataConfiguration pName_ pType_ =
  DocumentMetadataConfiguration'
    { relevance =
        Prelude.Nothing,
      search = Prelude.Nothing,
      name = pName_,
      type' = pType_
    }

-- | Provides manual tuning parameters to determine how the field affects the
-- search results.
documentMetadataConfiguration_relevance :: Lens.Lens' DocumentMetadataConfiguration (Prelude.Maybe Relevance)
documentMetadataConfiguration_relevance = Lens.lens (\DocumentMetadataConfiguration' {relevance} -> relevance) (\s@DocumentMetadataConfiguration' {} a -> s {relevance = a} :: DocumentMetadataConfiguration)

-- | Provides information about how the field is used during a search.
documentMetadataConfiguration_search :: Lens.Lens' DocumentMetadataConfiguration (Prelude.Maybe Search)
documentMetadataConfiguration_search = Lens.lens (\DocumentMetadataConfiguration' {search} -> search) (\s@DocumentMetadataConfiguration' {} a -> s {search = a} :: DocumentMetadataConfiguration)

-- | The name of the index field.
documentMetadataConfiguration_name :: Lens.Lens' DocumentMetadataConfiguration Prelude.Text
documentMetadataConfiguration_name = Lens.lens (\DocumentMetadataConfiguration' {name} -> name) (\s@DocumentMetadataConfiguration' {} a -> s {name = a} :: DocumentMetadataConfiguration)

-- | The data type of the index field.
documentMetadataConfiguration_type :: Lens.Lens' DocumentMetadataConfiguration DocumentAttributeValueType
documentMetadataConfiguration_type = Lens.lens (\DocumentMetadataConfiguration' {type'} -> type') (\s@DocumentMetadataConfiguration' {} a -> s {type' = a} :: DocumentMetadataConfiguration)

instance Core.FromJSON DocumentMetadataConfiguration where
  parseJSON =
    Core.withObject
      "DocumentMetadataConfiguration"
      ( \x ->
          DocumentMetadataConfiguration'
            Prelude.<$> (x Core..:? "Relevance")
            Prelude.<*> (x Core..:? "Search")
            Prelude.<*> (x Core..: "Name")
            Prelude.<*> (x Core..: "Type")
      )

instance
  Prelude.Hashable
    DocumentMetadataConfiguration

instance Prelude.NFData DocumentMetadataConfiguration

instance Core.ToJSON DocumentMetadataConfiguration where
  toJSON DocumentMetadataConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Relevance" Core..=) Prelude.<$> relevance,
            ("Search" Core..=) Prelude.<$> search,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Type" Core..= type')
          ]
      )
