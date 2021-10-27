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
-- Module      : Network.AWS.Kendra.Types.DocumentRelevanceConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.DocumentRelevanceConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.Relevance
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Overrides the document relevance properties of a custom index field.
--
-- /See:/ 'newDocumentRelevanceConfiguration' smart constructor.
data DocumentRelevanceConfiguration = DocumentRelevanceConfiguration'
  { -- | The name of the tuning configuration to override document relevance at
    -- the index level.
    name :: Prelude.Text,
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
-- 'name', 'documentRelevanceConfiguration_name' - The name of the tuning configuration to override document relevance at
-- the index level.
--
-- 'relevance', 'documentRelevanceConfiguration_relevance' - Undocumented member.
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

-- | The name of the tuning configuration to override document relevance at
-- the index level.
documentRelevanceConfiguration_name :: Lens.Lens' DocumentRelevanceConfiguration Prelude.Text
documentRelevanceConfiguration_name = Lens.lens (\DocumentRelevanceConfiguration' {name} -> name) (\s@DocumentRelevanceConfiguration' {} a -> s {name = a} :: DocumentRelevanceConfiguration)

-- | Undocumented member.
documentRelevanceConfiguration_relevance :: Lens.Lens' DocumentRelevanceConfiguration Relevance
documentRelevanceConfiguration_relevance = Lens.lens (\DocumentRelevanceConfiguration' {relevance} -> relevance) (\s@DocumentRelevanceConfiguration' {} a -> s {relevance = a} :: DocumentRelevanceConfiguration)

instance
  Prelude.Hashable
    DocumentRelevanceConfiguration

instance
  Prelude.NFData
    DocumentRelevanceConfiguration

instance Core.ToJSON DocumentRelevanceConfiguration where
  toJSON DocumentRelevanceConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Relevance" Core..= relevance)
          ]
      )
