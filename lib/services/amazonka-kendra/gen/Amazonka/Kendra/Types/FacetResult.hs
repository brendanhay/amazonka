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
-- Module      : Amazonka.Kendra.Types.FacetResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.FacetResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import {-# SOURCE #-} Amazonka.Kendra.Types.DocumentAttributeValueCountPair
import Amazonka.Kendra.Types.DocumentAttributeValueType
import qualified Amazonka.Prelude as Prelude

-- | The facet values for the documents in the response.
--
-- /See:/ 'newFacetResult' smart constructor.
data FacetResult = FacetResult'
  { -- | The key for the facet values. This is the same as the
    -- @DocumentAttributeKey@ provided in the query.
    documentAttributeKey :: Prelude.Maybe Prelude.Text,
    -- | An array of key\/value pairs, where the key is the value of the
    -- attribute and the count is the number of documents that share the key
    -- value.
    documentAttributeValueCountPairs :: Prelude.Maybe [DocumentAttributeValueCountPair],
    -- | The data type of the facet value. This is the same as the type defined
    -- for the index field when it was created.
    documentAttributeValueType :: Prelude.Maybe DocumentAttributeValueType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FacetResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentAttributeKey', 'facetResult_documentAttributeKey' - The key for the facet values. This is the same as the
-- @DocumentAttributeKey@ provided in the query.
--
-- 'documentAttributeValueCountPairs', 'facetResult_documentAttributeValueCountPairs' - An array of key\/value pairs, where the key is the value of the
-- attribute and the count is the number of documents that share the key
-- value.
--
-- 'documentAttributeValueType', 'facetResult_documentAttributeValueType' - The data type of the facet value. This is the same as the type defined
-- for the index field when it was created.
newFacetResult ::
  FacetResult
newFacetResult =
  FacetResult'
    { documentAttributeKey =
        Prelude.Nothing,
      documentAttributeValueCountPairs = Prelude.Nothing,
      documentAttributeValueType = Prelude.Nothing
    }

-- | The key for the facet values. This is the same as the
-- @DocumentAttributeKey@ provided in the query.
facetResult_documentAttributeKey :: Lens.Lens' FacetResult (Prelude.Maybe Prelude.Text)
facetResult_documentAttributeKey = Lens.lens (\FacetResult' {documentAttributeKey} -> documentAttributeKey) (\s@FacetResult' {} a -> s {documentAttributeKey = a} :: FacetResult)

-- | An array of key\/value pairs, where the key is the value of the
-- attribute and the count is the number of documents that share the key
-- value.
facetResult_documentAttributeValueCountPairs :: Lens.Lens' FacetResult (Prelude.Maybe [DocumentAttributeValueCountPair])
facetResult_documentAttributeValueCountPairs = Lens.lens (\FacetResult' {documentAttributeValueCountPairs} -> documentAttributeValueCountPairs) (\s@FacetResult' {} a -> s {documentAttributeValueCountPairs = a} :: FacetResult) Prelude.. Lens.mapping Lens.coerced

-- | The data type of the facet value. This is the same as the type defined
-- for the index field when it was created.
facetResult_documentAttributeValueType :: Lens.Lens' FacetResult (Prelude.Maybe DocumentAttributeValueType)
facetResult_documentAttributeValueType = Lens.lens (\FacetResult' {documentAttributeValueType} -> documentAttributeValueType) (\s@FacetResult' {} a -> s {documentAttributeValueType = a} :: FacetResult)

instance Data.FromJSON FacetResult where
  parseJSON =
    Data.withObject
      "FacetResult"
      ( \x ->
          FacetResult'
            Prelude.<$> (x Data..:? "DocumentAttributeKey")
            Prelude.<*> ( x Data..:? "DocumentAttributeValueCountPairs"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "DocumentAttributeValueType")
      )

instance Prelude.Hashable FacetResult where
  hashWithSalt _salt FacetResult' {..} =
    _salt `Prelude.hashWithSalt` documentAttributeKey
      `Prelude.hashWithSalt` documentAttributeValueCountPairs
      `Prelude.hashWithSalt` documentAttributeValueType

instance Prelude.NFData FacetResult where
  rnf FacetResult' {..} =
    Prelude.rnf documentAttributeKey
      `Prelude.seq` Prelude.rnf documentAttributeValueCountPairs
      `Prelude.seq` Prelude.rnf documentAttributeValueType
