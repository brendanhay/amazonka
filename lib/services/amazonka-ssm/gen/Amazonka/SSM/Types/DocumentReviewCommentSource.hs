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
-- Module      : Amazonka.SSM.Types.DocumentReviewCommentSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.DocumentReviewCommentSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.DocumentReviewCommentType

-- | Information about comments added to a document review request.
--
-- /See:/ 'newDocumentReviewCommentSource' smart constructor.
data DocumentReviewCommentSource = DocumentReviewCommentSource'
  { -- | The content of a comment entered by a user who requests a review of a
    -- new document version, or who reviews the new version.
    content :: Prelude.Maybe Prelude.Text,
    -- | The type of information added to a review request. Currently, only the
    -- value @Comment@ is supported.
    type' :: Prelude.Maybe DocumentReviewCommentType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentReviewCommentSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'content', 'documentReviewCommentSource_content' - The content of a comment entered by a user who requests a review of a
-- new document version, or who reviews the new version.
--
-- 'type'', 'documentReviewCommentSource_type' - The type of information added to a review request. Currently, only the
-- value @Comment@ is supported.
newDocumentReviewCommentSource ::
  DocumentReviewCommentSource
newDocumentReviewCommentSource =
  DocumentReviewCommentSource'
    { content =
        Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The content of a comment entered by a user who requests a review of a
-- new document version, or who reviews the new version.
documentReviewCommentSource_content :: Lens.Lens' DocumentReviewCommentSource (Prelude.Maybe Prelude.Text)
documentReviewCommentSource_content = Lens.lens (\DocumentReviewCommentSource' {content} -> content) (\s@DocumentReviewCommentSource' {} a -> s {content = a} :: DocumentReviewCommentSource)

-- | The type of information added to a review request. Currently, only the
-- value @Comment@ is supported.
documentReviewCommentSource_type :: Lens.Lens' DocumentReviewCommentSource (Prelude.Maybe DocumentReviewCommentType)
documentReviewCommentSource_type = Lens.lens (\DocumentReviewCommentSource' {type'} -> type') (\s@DocumentReviewCommentSource' {} a -> s {type' = a} :: DocumentReviewCommentSource)

instance Data.FromJSON DocumentReviewCommentSource where
  parseJSON =
    Data.withObject
      "DocumentReviewCommentSource"
      ( \x ->
          DocumentReviewCommentSource'
            Prelude.<$> (x Data..:? "Content")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable DocumentReviewCommentSource where
  hashWithSalt _salt DocumentReviewCommentSource' {..} =
    _salt `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` type'

instance Prelude.NFData DocumentReviewCommentSource where
  rnf DocumentReviewCommentSource' {..} =
    Prelude.rnf content `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON DocumentReviewCommentSource where
  toJSON DocumentReviewCommentSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Content" Data..=) Prelude.<$> content,
            ("Type" Data..=) Prelude.<$> type'
          ]
      )
