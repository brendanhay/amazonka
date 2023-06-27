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
-- Module      : Amazonka.KendraRanking.Types.Document
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KendraRanking.Types.Document where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a document from a search service such as OpenSearch
-- (self managed). Amazon Kendra Intelligent Ranking uses this information
-- to rank and score on.
--
-- /See:/ 'newDocument' smart constructor.
data Document = Document'
  { -- | The body text of the search service\'s document.
    body :: Prelude.Maybe Prelude.Text,
    -- | The optional group identifier of the document from the search service.
    -- Documents with the same group identifier are grouped together and
    -- processed as one document within the service.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | The title of the search service\'s document.
    title :: Prelude.Maybe Prelude.Text,
    -- | The body text of the search service\'s document represented as a list of
    -- tokens or words. You must choose to provide @Body@ or @TokenizedBody@.
    -- You cannot provide both.
    tokenizedBody :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The title of the search service\'s document represented as a list of
    -- tokens or words. You must choose to provide @Title@ or @TokenizedTitle@.
    -- You cannot provide both.
    tokenizedTitle :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The identifier of the document from the search service.
    id :: Prelude.Text,
    -- | The original document score or rank from the search service. Amazon
    -- Kendra Intelligent Ranking gives the document a new score or rank based
    -- on its intelligent search algorithms.
    originalScore :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Document' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'body', 'document_body' - The body text of the search service\'s document.
--
-- 'groupId', 'document_groupId' - The optional group identifier of the document from the search service.
-- Documents with the same group identifier are grouped together and
-- processed as one document within the service.
--
-- 'title', 'document_title' - The title of the search service\'s document.
--
-- 'tokenizedBody', 'document_tokenizedBody' - The body text of the search service\'s document represented as a list of
-- tokens or words. You must choose to provide @Body@ or @TokenizedBody@.
-- You cannot provide both.
--
-- 'tokenizedTitle', 'document_tokenizedTitle' - The title of the search service\'s document represented as a list of
-- tokens or words. You must choose to provide @Title@ or @TokenizedTitle@.
-- You cannot provide both.
--
-- 'id', 'document_id' - The identifier of the document from the search service.
--
-- 'originalScore', 'document_originalScore' - The original document score or rank from the search service. Amazon
-- Kendra Intelligent Ranking gives the document a new score or rank based
-- on its intelligent search algorithms.
newDocument ::
  -- | 'id'
  Prelude.Text ->
  -- | 'originalScore'
  Prelude.Double ->
  Document
newDocument pId_ pOriginalScore_ =
  Document'
    { body = Prelude.Nothing,
      groupId = Prelude.Nothing,
      title = Prelude.Nothing,
      tokenizedBody = Prelude.Nothing,
      tokenizedTitle = Prelude.Nothing,
      id = pId_,
      originalScore = pOriginalScore_
    }

-- | The body text of the search service\'s document.
document_body :: Lens.Lens' Document (Prelude.Maybe Prelude.Text)
document_body = Lens.lens (\Document' {body} -> body) (\s@Document' {} a -> s {body = a} :: Document)

-- | The optional group identifier of the document from the search service.
-- Documents with the same group identifier are grouped together and
-- processed as one document within the service.
document_groupId :: Lens.Lens' Document (Prelude.Maybe Prelude.Text)
document_groupId = Lens.lens (\Document' {groupId} -> groupId) (\s@Document' {} a -> s {groupId = a} :: Document)

-- | The title of the search service\'s document.
document_title :: Lens.Lens' Document (Prelude.Maybe Prelude.Text)
document_title = Lens.lens (\Document' {title} -> title) (\s@Document' {} a -> s {title = a} :: Document)

-- | The body text of the search service\'s document represented as a list of
-- tokens or words. You must choose to provide @Body@ or @TokenizedBody@.
-- You cannot provide both.
document_tokenizedBody :: Lens.Lens' Document (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
document_tokenizedBody = Lens.lens (\Document' {tokenizedBody} -> tokenizedBody) (\s@Document' {} a -> s {tokenizedBody = a} :: Document) Prelude.. Lens.mapping Lens.coerced

-- | The title of the search service\'s document represented as a list of
-- tokens or words. You must choose to provide @Title@ or @TokenizedTitle@.
-- You cannot provide both.
document_tokenizedTitle :: Lens.Lens' Document (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
document_tokenizedTitle = Lens.lens (\Document' {tokenizedTitle} -> tokenizedTitle) (\s@Document' {} a -> s {tokenizedTitle = a} :: Document) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the document from the search service.
document_id :: Lens.Lens' Document Prelude.Text
document_id = Lens.lens (\Document' {id} -> id) (\s@Document' {} a -> s {id = a} :: Document)

-- | The original document score or rank from the search service. Amazon
-- Kendra Intelligent Ranking gives the document a new score or rank based
-- on its intelligent search algorithms.
document_originalScore :: Lens.Lens' Document Prelude.Double
document_originalScore = Lens.lens (\Document' {originalScore} -> originalScore) (\s@Document' {} a -> s {originalScore = a} :: Document)

instance Prelude.Hashable Document where
  hashWithSalt _salt Document' {..} =
    _salt
      `Prelude.hashWithSalt` body
      `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` tokenizedBody
      `Prelude.hashWithSalt` tokenizedTitle
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` originalScore

instance Prelude.NFData Document where
  rnf Document' {..} =
    Prelude.rnf body
      `Prelude.seq` Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf tokenizedBody
      `Prelude.seq` Prelude.rnf tokenizedTitle
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf originalScore

instance Data.ToJSON Document where
  toJSON Document' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Body" Data..=) Prelude.<$> body,
            ("GroupId" Data..=) Prelude.<$> groupId,
            ("Title" Data..=) Prelude.<$> title,
            ("TokenizedBody" Data..=) Prelude.<$> tokenizedBody,
            ("TokenizedTitle" Data..=)
              Prelude.<$> tokenizedTitle,
            Prelude.Just ("Id" Data..= id),
            Prelude.Just
              ("OriginalScore" Data..= originalScore)
          ]
      )
