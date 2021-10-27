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
-- Module      : Network.AWS.Kendra.Types.ConfluenceBlogConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.ConfluenceBlogConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.ConfluenceBlogToIndexFieldMapping
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the blog settings for the Confluence data source. Blogs are
-- always indexed unless filtered from the index by the @ExclusionPatterns@
-- or @InclusionPatterns@ fields in the @ConfluenceConfiguration@ type.
--
-- /See:/ 'newConfluenceBlogConfiguration' smart constructor.
data ConfluenceBlogConfiguration = ConfluenceBlogConfiguration'
  { -- | Defines how blog metadata fields should be mapped to index fields.
    -- Before you can map a field, you must first create an index field with a
    -- matching type using the console or the @UpdateIndex@ operation.
    --
    -- If you specify the @BlogFieldMappings@ parameter, you must specify at
    -- least one field mapping.
    blogFieldMappings :: Prelude.Maybe (Prelude.NonEmpty ConfluenceBlogToIndexFieldMapping)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfluenceBlogConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blogFieldMappings', 'confluenceBlogConfiguration_blogFieldMappings' - Defines how blog metadata fields should be mapped to index fields.
-- Before you can map a field, you must first create an index field with a
-- matching type using the console or the @UpdateIndex@ operation.
--
-- If you specify the @BlogFieldMappings@ parameter, you must specify at
-- least one field mapping.
newConfluenceBlogConfiguration ::
  ConfluenceBlogConfiguration
newConfluenceBlogConfiguration =
  ConfluenceBlogConfiguration'
    { blogFieldMappings =
        Prelude.Nothing
    }

-- | Defines how blog metadata fields should be mapped to index fields.
-- Before you can map a field, you must first create an index field with a
-- matching type using the console or the @UpdateIndex@ operation.
--
-- If you specify the @BlogFieldMappings@ parameter, you must specify at
-- least one field mapping.
confluenceBlogConfiguration_blogFieldMappings :: Lens.Lens' ConfluenceBlogConfiguration (Prelude.Maybe (Prelude.NonEmpty ConfluenceBlogToIndexFieldMapping))
confluenceBlogConfiguration_blogFieldMappings = Lens.lens (\ConfluenceBlogConfiguration' {blogFieldMappings} -> blogFieldMappings) (\s@ConfluenceBlogConfiguration' {} a -> s {blogFieldMappings = a} :: ConfluenceBlogConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ConfluenceBlogConfiguration where
  parseJSON =
    Core.withObject
      "ConfluenceBlogConfiguration"
      ( \x ->
          ConfluenceBlogConfiguration'
            Prelude.<$> (x Core..:? "BlogFieldMappings")
      )

instance Prelude.Hashable ConfluenceBlogConfiguration

instance Prelude.NFData ConfluenceBlogConfiguration

instance Core.ToJSON ConfluenceBlogConfiguration where
  toJSON ConfluenceBlogConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("BlogFieldMappings" Core..=)
              Prelude.<$> blogFieldMappings
          ]
      )
