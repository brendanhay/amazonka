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
-- Module      : Network.AWS.Kendra.Types.FacetResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.FacetResult where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.DocumentAttributeValueCountPair
import Network.AWS.Kendra.Types.DocumentAttributeValueType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON FacetResult where
  parseJSON =
    Core.withObject
      "FacetResult"
      ( \x ->
          FacetResult'
            Prelude.<$> (x Core..:? "DocumentAttributeKey")
            Prelude.<*> ( x Core..:? "DocumentAttributeValueCountPairs"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "DocumentAttributeValueType")
      )

instance Prelude.Hashable FacetResult

instance Prelude.NFData FacetResult
