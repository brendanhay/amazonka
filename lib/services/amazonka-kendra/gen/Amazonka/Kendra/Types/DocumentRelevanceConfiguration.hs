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
-- Module      : Amazonka.Kendra.Types.DocumentRelevanceConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.DocumentRelevanceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.Relevance
import qualified Amazonka.Prelude as Prelude

-- | Overrides the document relevance properties of a custom index field.
--
-- /See:/ 'newDocumentRelevanceConfiguration' smart constructor.
data DocumentRelevanceConfiguration = DocumentRelevanceConfiguration'
  { -- | The name of the index field.
    name :: Prelude.Text,
    -- | Provides information for tuning the relevance of a field in a search.
    -- When a query includes terms that match the field, the results are given
    -- a boost in the response based on these tuning parameters.
    relevance :: Relevance
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentRelevanceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'documentRelevanceConfiguration_name' - The name of the index field.
--
-- 'relevance', 'documentRelevanceConfiguration_relevance' - Provides information for tuning the relevance of a field in a search.
-- When a query includes terms that match the field, the results are given
-- a boost in the response based on these tuning parameters.
newDocumentRelevanceConfiguration ::
  -- | 'name'
  Prelude.Text ->
  -- | 'relevance'
  Relevance ->
  DocumentRelevanceConfiguration
newDocumentRelevanceConfiguration pName_ pRelevance_ =
  DocumentRelevanceConfiguration'
    { name = pName_,
      relevance = pRelevance_
    }

-- | The name of the index field.
documentRelevanceConfiguration_name :: Lens.Lens' DocumentRelevanceConfiguration Prelude.Text
documentRelevanceConfiguration_name = Lens.lens (\DocumentRelevanceConfiguration' {name} -> name) (\s@DocumentRelevanceConfiguration' {} a -> s {name = a} :: DocumentRelevanceConfiguration)

-- | Provides information for tuning the relevance of a field in a search.
-- When a query includes terms that match the field, the results are given
-- a boost in the response based on these tuning parameters.
documentRelevanceConfiguration_relevance :: Lens.Lens' DocumentRelevanceConfiguration Relevance
documentRelevanceConfiguration_relevance = Lens.lens (\DocumentRelevanceConfiguration' {relevance} -> relevance) (\s@DocumentRelevanceConfiguration' {} a -> s {relevance = a} :: DocumentRelevanceConfiguration)

instance
  Prelude.Hashable
    DocumentRelevanceConfiguration
  where
  hashWithSalt
    _salt
    DocumentRelevanceConfiguration' {..} =
      _salt `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` relevance

instance
  Prelude.NFData
    DocumentRelevanceConfiguration
  where
  rnf DocumentRelevanceConfiguration' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf relevance

instance Data.ToJSON DocumentRelevanceConfiguration where
  toJSON DocumentRelevanceConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Relevance" Data..= relevance)
          ]
      )
