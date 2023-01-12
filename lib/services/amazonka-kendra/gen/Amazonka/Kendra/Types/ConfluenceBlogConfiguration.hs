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
-- Module      : Amazonka.Kendra.Types.ConfluenceBlogConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.ConfluenceBlogConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.ConfluenceBlogToIndexFieldMapping
import qualified Amazonka.Prelude as Prelude

-- | Configuration of blog settings for the Confluence data source. Blogs are
-- always indexed unless filtered from the index by the @ExclusionPatterns@
-- or @InclusionPatterns@ fields in the @ConfluenceConfiguration@ object.
--
-- /See:/ 'newConfluenceBlogConfiguration' smart constructor.
data ConfluenceBlogConfiguration = ConfluenceBlogConfiguration'
  { -- | Maps attributes or field names of Confluence blogs to Amazon Kendra
    -- index field names. To create custom fields, use the @UpdateIndex@ API
    -- before you map to Confluence fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The Confluence data source field names must exist in your Confluence
    -- custom metadata.
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
-- 'blogFieldMappings', 'confluenceBlogConfiguration_blogFieldMappings' - Maps attributes or field names of Confluence blogs to Amazon Kendra
-- index field names. To create custom fields, use the @UpdateIndex@ API
-- before you map to Confluence fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Confluence data source field names must exist in your Confluence
-- custom metadata.
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

-- | Maps attributes or field names of Confluence blogs to Amazon Kendra
-- index field names. To create custom fields, use the @UpdateIndex@ API
-- before you map to Confluence fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Confluence data source field names must exist in your Confluence
-- custom metadata.
--
-- If you specify the @BlogFieldMappings@ parameter, you must specify at
-- least one field mapping.
confluenceBlogConfiguration_blogFieldMappings :: Lens.Lens' ConfluenceBlogConfiguration (Prelude.Maybe (Prelude.NonEmpty ConfluenceBlogToIndexFieldMapping))
confluenceBlogConfiguration_blogFieldMappings = Lens.lens (\ConfluenceBlogConfiguration' {blogFieldMappings} -> blogFieldMappings) (\s@ConfluenceBlogConfiguration' {} a -> s {blogFieldMappings = a} :: ConfluenceBlogConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ConfluenceBlogConfiguration where
  parseJSON =
    Data.withObject
      "ConfluenceBlogConfiguration"
      ( \x ->
          ConfluenceBlogConfiguration'
            Prelude.<$> (x Data..:? "BlogFieldMappings")
      )

instance Prelude.Hashable ConfluenceBlogConfiguration where
  hashWithSalt _salt ConfluenceBlogConfiguration' {..} =
    _salt `Prelude.hashWithSalt` blogFieldMappings

instance Prelude.NFData ConfluenceBlogConfiguration where
  rnf ConfluenceBlogConfiguration' {..} =
    Prelude.rnf blogFieldMappings

instance Data.ToJSON ConfluenceBlogConfiguration where
  toJSON ConfluenceBlogConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BlogFieldMappings" Data..=)
              Prelude.<$> blogFieldMappings
          ]
      )
