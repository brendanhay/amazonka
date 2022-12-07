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
-- Module      : Amazonka.Kendra.Types.QueryResultItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.QueryResultItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.AdditionalResultAttribute
import Amazonka.Kendra.Types.DocumentAttribute
import Amazonka.Kendra.Types.QueryResultType
import Amazonka.Kendra.Types.ScoreAttributes
import Amazonka.Kendra.Types.TextWithHighlights
import qualified Amazonka.Prelude as Prelude

-- | A single query result.
--
-- A query result contains information about a document returned by the
-- query. This includes the original location of the document, a list of
-- attributes assigned to the document, and relevant text from the document
-- that satisfies the query.
--
-- /See:/ 'newQueryResultItem' smart constructor.
data QueryResultItem = QueryResultItem'
  { -- | The type of document.
    type' :: Prelude.Maybe QueryResultType,
    -- | The title of the document. Contains the text of the title and
    -- information for highlighting the relevant terms in the title.
    documentTitle :: Prelude.Maybe TextWithHighlights,
    -- | An array of document attributes assigned to a document in the search
    -- results. For example, the document author (@_author@) or the source URI
    -- (@_source_uri@) of the document.
    documentAttributes :: Prelude.Maybe [DocumentAttribute],
    -- | Indicates the confidence that Amazon Kendra has that a result matches
    -- the query that you provided. Each result is placed into a bin that
    -- indicates the confidence, @VERY_HIGH@, @HIGH@, @MEDIUM@ and @LOW@. You
    -- can use the score to determine if a response meets the confidence needed
    -- for your application.
    --
    -- The field is only set to @LOW@ when the @Type@ field is set to
    -- @DOCUMENT@ and Amazon Kendra is not confident that the result matches
    -- the query.
    scoreAttributes :: Prelude.Maybe ScoreAttributes,
    -- | The unique identifier for the query result.
    id :: Prelude.Maybe Prelude.Text,
    -- | A token that identifies a particular result from a particular query. Use
    -- this token to provide click-through feedback for the result. For more
    -- information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/submitting-feedback.html Submitting feedback>
    -- .
    feedbackToken :: Prelude.Maybe Prelude.Text,
    -- | One or more additional attributes associated with the query result.
    additionalAttributes :: Prelude.Maybe [AdditionalResultAttribute],
    -- | An extract of the text in the document. Contains information about
    -- highlighting the relevant terms in the excerpt.
    documentExcerpt :: Prelude.Maybe TextWithHighlights,
    -- | The unique identifier for the document.
    documentId :: Prelude.Maybe Prelude.Text,
    -- | The URI of the original location of the document.
    documentURI :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryResultItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'queryResultItem_type' - The type of document.
--
-- 'documentTitle', 'queryResultItem_documentTitle' - The title of the document. Contains the text of the title and
-- information for highlighting the relevant terms in the title.
--
-- 'documentAttributes', 'queryResultItem_documentAttributes' - An array of document attributes assigned to a document in the search
-- results. For example, the document author (@_author@) or the source URI
-- (@_source_uri@) of the document.
--
-- 'scoreAttributes', 'queryResultItem_scoreAttributes' - Indicates the confidence that Amazon Kendra has that a result matches
-- the query that you provided. Each result is placed into a bin that
-- indicates the confidence, @VERY_HIGH@, @HIGH@, @MEDIUM@ and @LOW@. You
-- can use the score to determine if a response meets the confidence needed
-- for your application.
--
-- The field is only set to @LOW@ when the @Type@ field is set to
-- @DOCUMENT@ and Amazon Kendra is not confident that the result matches
-- the query.
--
-- 'id', 'queryResultItem_id' - The unique identifier for the query result.
--
-- 'feedbackToken', 'queryResultItem_feedbackToken' - A token that identifies a particular result from a particular query. Use
-- this token to provide click-through feedback for the result. For more
-- information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/submitting-feedback.html Submitting feedback>
-- .
--
-- 'additionalAttributes', 'queryResultItem_additionalAttributes' - One or more additional attributes associated with the query result.
--
-- 'documentExcerpt', 'queryResultItem_documentExcerpt' - An extract of the text in the document. Contains information about
-- highlighting the relevant terms in the excerpt.
--
-- 'documentId', 'queryResultItem_documentId' - The unique identifier for the document.
--
-- 'documentURI', 'queryResultItem_documentURI' - The URI of the original location of the document.
newQueryResultItem ::
  QueryResultItem
newQueryResultItem =
  QueryResultItem'
    { type' = Prelude.Nothing,
      documentTitle = Prelude.Nothing,
      documentAttributes = Prelude.Nothing,
      scoreAttributes = Prelude.Nothing,
      id = Prelude.Nothing,
      feedbackToken = Prelude.Nothing,
      additionalAttributes = Prelude.Nothing,
      documentExcerpt = Prelude.Nothing,
      documentId = Prelude.Nothing,
      documentURI = Prelude.Nothing
    }

-- | The type of document.
queryResultItem_type :: Lens.Lens' QueryResultItem (Prelude.Maybe QueryResultType)
queryResultItem_type = Lens.lens (\QueryResultItem' {type'} -> type') (\s@QueryResultItem' {} a -> s {type' = a} :: QueryResultItem)

-- | The title of the document. Contains the text of the title and
-- information for highlighting the relevant terms in the title.
queryResultItem_documentTitle :: Lens.Lens' QueryResultItem (Prelude.Maybe TextWithHighlights)
queryResultItem_documentTitle = Lens.lens (\QueryResultItem' {documentTitle} -> documentTitle) (\s@QueryResultItem' {} a -> s {documentTitle = a} :: QueryResultItem)

-- | An array of document attributes assigned to a document in the search
-- results. For example, the document author (@_author@) or the source URI
-- (@_source_uri@) of the document.
queryResultItem_documentAttributes :: Lens.Lens' QueryResultItem (Prelude.Maybe [DocumentAttribute])
queryResultItem_documentAttributes = Lens.lens (\QueryResultItem' {documentAttributes} -> documentAttributes) (\s@QueryResultItem' {} a -> s {documentAttributes = a} :: QueryResultItem) Prelude.. Lens.mapping Lens.coerced

-- | Indicates the confidence that Amazon Kendra has that a result matches
-- the query that you provided. Each result is placed into a bin that
-- indicates the confidence, @VERY_HIGH@, @HIGH@, @MEDIUM@ and @LOW@. You
-- can use the score to determine if a response meets the confidence needed
-- for your application.
--
-- The field is only set to @LOW@ when the @Type@ field is set to
-- @DOCUMENT@ and Amazon Kendra is not confident that the result matches
-- the query.
queryResultItem_scoreAttributes :: Lens.Lens' QueryResultItem (Prelude.Maybe ScoreAttributes)
queryResultItem_scoreAttributes = Lens.lens (\QueryResultItem' {scoreAttributes} -> scoreAttributes) (\s@QueryResultItem' {} a -> s {scoreAttributes = a} :: QueryResultItem)

-- | The unique identifier for the query result.
queryResultItem_id :: Lens.Lens' QueryResultItem (Prelude.Maybe Prelude.Text)
queryResultItem_id = Lens.lens (\QueryResultItem' {id} -> id) (\s@QueryResultItem' {} a -> s {id = a} :: QueryResultItem)

-- | A token that identifies a particular result from a particular query. Use
-- this token to provide click-through feedback for the result. For more
-- information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/submitting-feedback.html Submitting feedback>
-- .
queryResultItem_feedbackToken :: Lens.Lens' QueryResultItem (Prelude.Maybe Prelude.Text)
queryResultItem_feedbackToken = Lens.lens (\QueryResultItem' {feedbackToken} -> feedbackToken) (\s@QueryResultItem' {} a -> s {feedbackToken = a} :: QueryResultItem)

-- | One or more additional attributes associated with the query result.
queryResultItem_additionalAttributes :: Lens.Lens' QueryResultItem (Prelude.Maybe [AdditionalResultAttribute])
queryResultItem_additionalAttributes = Lens.lens (\QueryResultItem' {additionalAttributes} -> additionalAttributes) (\s@QueryResultItem' {} a -> s {additionalAttributes = a} :: QueryResultItem) Prelude.. Lens.mapping Lens.coerced

-- | An extract of the text in the document. Contains information about
-- highlighting the relevant terms in the excerpt.
queryResultItem_documentExcerpt :: Lens.Lens' QueryResultItem (Prelude.Maybe TextWithHighlights)
queryResultItem_documentExcerpt = Lens.lens (\QueryResultItem' {documentExcerpt} -> documentExcerpt) (\s@QueryResultItem' {} a -> s {documentExcerpt = a} :: QueryResultItem)

-- | The unique identifier for the document.
queryResultItem_documentId :: Lens.Lens' QueryResultItem (Prelude.Maybe Prelude.Text)
queryResultItem_documentId = Lens.lens (\QueryResultItem' {documentId} -> documentId) (\s@QueryResultItem' {} a -> s {documentId = a} :: QueryResultItem)

-- | The URI of the original location of the document.
queryResultItem_documentURI :: Lens.Lens' QueryResultItem (Prelude.Maybe Prelude.Text)
queryResultItem_documentURI = Lens.lens (\QueryResultItem' {documentURI} -> documentURI) (\s@QueryResultItem' {} a -> s {documentURI = a} :: QueryResultItem)

instance Data.FromJSON QueryResultItem where
  parseJSON =
    Data.withObject
      "QueryResultItem"
      ( \x ->
          QueryResultItem'
            Prelude.<$> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "DocumentTitle")
            Prelude.<*> ( x Data..:? "DocumentAttributes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ScoreAttributes")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "FeedbackToken")
            Prelude.<*> ( x Data..:? "AdditionalAttributes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "DocumentExcerpt")
            Prelude.<*> (x Data..:? "DocumentId")
            Prelude.<*> (x Data..:? "DocumentURI")
      )

instance Prelude.Hashable QueryResultItem where
  hashWithSalt _salt QueryResultItem' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` documentTitle
      `Prelude.hashWithSalt` documentAttributes
      `Prelude.hashWithSalt` scoreAttributes
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` feedbackToken
      `Prelude.hashWithSalt` additionalAttributes
      `Prelude.hashWithSalt` documentExcerpt
      `Prelude.hashWithSalt` documentId
      `Prelude.hashWithSalt` documentURI

instance Prelude.NFData QueryResultItem where
  rnf QueryResultItem' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf documentTitle
      `Prelude.seq` Prelude.rnf documentAttributes
      `Prelude.seq` Prelude.rnf scoreAttributes
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf feedbackToken
      `Prelude.seq` Prelude.rnf additionalAttributes
      `Prelude.seq` Prelude.rnf documentExcerpt
      `Prelude.seq` Prelude.rnf documentId
      `Prelude.seq` Prelude.rnf documentURI
