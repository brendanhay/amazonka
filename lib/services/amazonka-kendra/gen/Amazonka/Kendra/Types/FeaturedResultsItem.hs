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
-- Module      : Amazonka.Kendra.Types.FeaturedResultsItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.FeaturedResultsItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.AdditionalResultAttribute
import Amazonka.Kendra.Types.DocumentAttribute
import Amazonka.Kendra.Types.QueryResultType
import Amazonka.Kendra.Types.TextWithHighlights
import qualified Amazonka.Prelude as Prelude

-- | A single featured result item. A featured result is displayed at the top
-- of the search results page, placed above all other results for certain
-- queries. If there\'s an exact match of a query, then certain documents
-- are featured in the search results.
--
-- /See:/ 'newFeaturedResultsItem' smart constructor.
data FeaturedResultsItem = FeaturedResultsItem'
  { -- | One or more additional attributes associated with the featured result.
    additionalAttributes :: Prelude.Maybe [AdditionalResultAttribute],
    -- | An array of document attributes assigned to a featured document in the
    -- search results. For example, the document author (@_author@) or the
    -- source URI (@_source_uri@) of the document.
    documentAttributes :: Prelude.Maybe [DocumentAttribute],
    documentExcerpt :: Prelude.Maybe TextWithHighlights,
    -- | The identifier of the featured document.
    documentId :: Prelude.Maybe Prelude.Text,
    documentTitle :: Prelude.Maybe TextWithHighlights,
    -- | The source URI location of the featured document.
    documentURI :: Prelude.Maybe Prelude.Text,
    -- | A token that identifies a particular featured result from a particular
    -- query. Use this token to provide click-through feedback for the result.
    -- For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/submitting-feedback.html Submitting feedback>.
    feedbackToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the featured result.
    id :: Prelude.Maybe Prelude.Text,
    -- | The type of document within the featured result response. For example, a
    -- response could include a question-answer type that\'s relevant to the
    -- query.
    type' :: Prelude.Maybe QueryResultType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FeaturedResultsItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalAttributes', 'featuredResultsItem_additionalAttributes' - One or more additional attributes associated with the featured result.
--
-- 'documentAttributes', 'featuredResultsItem_documentAttributes' - An array of document attributes assigned to a featured document in the
-- search results. For example, the document author (@_author@) or the
-- source URI (@_source_uri@) of the document.
--
-- 'documentExcerpt', 'featuredResultsItem_documentExcerpt' - Undocumented member.
--
-- 'documentId', 'featuredResultsItem_documentId' - The identifier of the featured document.
--
-- 'documentTitle', 'featuredResultsItem_documentTitle' - Undocumented member.
--
-- 'documentURI', 'featuredResultsItem_documentURI' - The source URI location of the featured document.
--
-- 'feedbackToken', 'featuredResultsItem_feedbackToken' - A token that identifies a particular featured result from a particular
-- query. Use this token to provide click-through feedback for the result.
-- For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/submitting-feedback.html Submitting feedback>.
--
-- 'id', 'featuredResultsItem_id' - The identifier of the featured result.
--
-- 'type'', 'featuredResultsItem_type' - The type of document within the featured result response. For example, a
-- response could include a question-answer type that\'s relevant to the
-- query.
newFeaturedResultsItem ::
  FeaturedResultsItem
newFeaturedResultsItem =
  FeaturedResultsItem'
    { additionalAttributes =
        Prelude.Nothing,
      documentAttributes = Prelude.Nothing,
      documentExcerpt = Prelude.Nothing,
      documentId = Prelude.Nothing,
      documentTitle = Prelude.Nothing,
      documentURI = Prelude.Nothing,
      feedbackToken = Prelude.Nothing,
      id = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | One or more additional attributes associated with the featured result.
featuredResultsItem_additionalAttributes :: Lens.Lens' FeaturedResultsItem (Prelude.Maybe [AdditionalResultAttribute])
featuredResultsItem_additionalAttributes = Lens.lens (\FeaturedResultsItem' {additionalAttributes} -> additionalAttributes) (\s@FeaturedResultsItem' {} a -> s {additionalAttributes = a} :: FeaturedResultsItem) Prelude.. Lens.mapping Lens.coerced

-- | An array of document attributes assigned to a featured document in the
-- search results. For example, the document author (@_author@) or the
-- source URI (@_source_uri@) of the document.
featuredResultsItem_documentAttributes :: Lens.Lens' FeaturedResultsItem (Prelude.Maybe [DocumentAttribute])
featuredResultsItem_documentAttributes = Lens.lens (\FeaturedResultsItem' {documentAttributes} -> documentAttributes) (\s@FeaturedResultsItem' {} a -> s {documentAttributes = a} :: FeaturedResultsItem) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
featuredResultsItem_documentExcerpt :: Lens.Lens' FeaturedResultsItem (Prelude.Maybe TextWithHighlights)
featuredResultsItem_documentExcerpt = Lens.lens (\FeaturedResultsItem' {documentExcerpt} -> documentExcerpt) (\s@FeaturedResultsItem' {} a -> s {documentExcerpt = a} :: FeaturedResultsItem)

-- | The identifier of the featured document.
featuredResultsItem_documentId :: Lens.Lens' FeaturedResultsItem (Prelude.Maybe Prelude.Text)
featuredResultsItem_documentId = Lens.lens (\FeaturedResultsItem' {documentId} -> documentId) (\s@FeaturedResultsItem' {} a -> s {documentId = a} :: FeaturedResultsItem)

-- | Undocumented member.
featuredResultsItem_documentTitle :: Lens.Lens' FeaturedResultsItem (Prelude.Maybe TextWithHighlights)
featuredResultsItem_documentTitle = Lens.lens (\FeaturedResultsItem' {documentTitle} -> documentTitle) (\s@FeaturedResultsItem' {} a -> s {documentTitle = a} :: FeaturedResultsItem)

-- | The source URI location of the featured document.
featuredResultsItem_documentURI :: Lens.Lens' FeaturedResultsItem (Prelude.Maybe Prelude.Text)
featuredResultsItem_documentURI = Lens.lens (\FeaturedResultsItem' {documentURI} -> documentURI) (\s@FeaturedResultsItem' {} a -> s {documentURI = a} :: FeaturedResultsItem)

-- | A token that identifies a particular featured result from a particular
-- query. Use this token to provide click-through feedback for the result.
-- For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/submitting-feedback.html Submitting feedback>.
featuredResultsItem_feedbackToken :: Lens.Lens' FeaturedResultsItem (Prelude.Maybe Prelude.Text)
featuredResultsItem_feedbackToken = Lens.lens (\FeaturedResultsItem' {feedbackToken} -> feedbackToken) (\s@FeaturedResultsItem' {} a -> s {feedbackToken = a} :: FeaturedResultsItem)

-- | The identifier of the featured result.
featuredResultsItem_id :: Lens.Lens' FeaturedResultsItem (Prelude.Maybe Prelude.Text)
featuredResultsItem_id = Lens.lens (\FeaturedResultsItem' {id} -> id) (\s@FeaturedResultsItem' {} a -> s {id = a} :: FeaturedResultsItem)

-- | The type of document within the featured result response. For example, a
-- response could include a question-answer type that\'s relevant to the
-- query.
featuredResultsItem_type :: Lens.Lens' FeaturedResultsItem (Prelude.Maybe QueryResultType)
featuredResultsItem_type = Lens.lens (\FeaturedResultsItem' {type'} -> type') (\s@FeaturedResultsItem' {} a -> s {type' = a} :: FeaturedResultsItem)

instance Data.FromJSON FeaturedResultsItem where
  parseJSON =
    Data.withObject
      "FeaturedResultsItem"
      ( \x ->
          FeaturedResultsItem'
            Prelude.<$> ( x
                            Data..:? "AdditionalAttributes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "DocumentAttributes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "DocumentExcerpt")
            Prelude.<*> (x Data..:? "DocumentId")
            Prelude.<*> (x Data..:? "DocumentTitle")
            Prelude.<*> (x Data..:? "DocumentURI")
            Prelude.<*> (x Data..:? "FeedbackToken")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable FeaturedResultsItem where
  hashWithSalt _salt FeaturedResultsItem' {..} =
    _salt
      `Prelude.hashWithSalt` additionalAttributes
      `Prelude.hashWithSalt` documentAttributes
      `Prelude.hashWithSalt` documentExcerpt
      `Prelude.hashWithSalt` documentId
      `Prelude.hashWithSalt` documentTitle
      `Prelude.hashWithSalt` documentURI
      `Prelude.hashWithSalt` feedbackToken
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` type'

instance Prelude.NFData FeaturedResultsItem where
  rnf FeaturedResultsItem' {..} =
    Prelude.rnf additionalAttributes
      `Prelude.seq` Prelude.rnf documentAttributes
      `Prelude.seq` Prelude.rnf documentExcerpt
      `Prelude.seq` Prelude.rnf documentId
      `Prelude.seq` Prelude.rnf documentTitle
      `Prelude.seq` Prelude.rnf documentURI
      `Prelude.seq` Prelude.rnf feedbackToken
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf type'
