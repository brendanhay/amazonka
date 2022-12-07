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
-- Module      : Amazonka.SSM.Types.DocumentReviews
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.DocumentReviews where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.DocumentReviewAction
import Amazonka.SSM.Types.DocumentReviewCommentSource

-- | Information about a document approval review.
--
-- /See:/ 'newDocumentReviews' smart constructor.
data DocumentReviews = DocumentReviews'
  { -- | A comment entered by a user in your organization about the document
    -- review request.
    comment :: Prelude.Maybe [DocumentReviewCommentSource],
    -- | The action to take on a document approval review request.
    action :: DocumentReviewAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentReviews' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'documentReviews_comment' - A comment entered by a user in your organization about the document
-- review request.
--
-- 'action', 'documentReviews_action' - The action to take on a document approval review request.
newDocumentReviews ::
  -- | 'action'
  DocumentReviewAction ->
  DocumentReviews
newDocumentReviews pAction_ =
  DocumentReviews'
    { comment = Prelude.Nothing,
      action = pAction_
    }

-- | A comment entered by a user in your organization about the document
-- review request.
documentReviews_comment :: Lens.Lens' DocumentReviews (Prelude.Maybe [DocumentReviewCommentSource])
documentReviews_comment = Lens.lens (\DocumentReviews' {comment} -> comment) (\s@DocumentReviews' {} a -> s {comment = a} :: DocumentReviews) Prelude.. Lens.mapping Lens.coerced

-- | The action to take on a document approval review request.
documentReviews_action :: Lens.Lens' DocumentReviews DocumentReviewAction
documentReviews_action = Lens.lens (\DocumentReviews' {action} -> action) (\s@DocumentReviews' {} a -> s {action = a} :: DocumentReviews)

instance Prelude.Hashable DocumentReviews where
  hashWithSalt _salt DocumentReviews' {..} =
    _salt `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` action

instance Prelude.NFData DocumentReviews where
  rnf DocumentReviews' {..} =
    Prelude.rnf comment
      `Prelude.seq` Prelude.rnf action

instance Data.ToJSON DocumentReviews where
  toJSON DocumentReviews' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Comment" Data..=) Prelude.<$> comment,
            Prelude.Just ("Action" Data..= action)
          ]
      )
