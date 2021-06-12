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
-- Module      : Network.AWS.SSM.Types.DocumentReviews
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentReviews where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.DocumentReviewAction
import Network.AWS.SSM.Types.DocumentReviewCommentSource

-- | Information about a document approval review.
--
-- /See:/ 'newDocumentReviews' smart constructor.
data DocumentReviews = DocumentReviews'
  { -- | A comment entered by a user in your organization about the document
    -- review request.
    comment :: Core.Maybe [DocumentReviewCommentSource],
    -- | The action to take on a document approval review request.
    action :: DocumentReviewAction
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { comment = Core.Nothing,
      action = pAction_
    }

-- | A comment entered by a user in your organization about the document
-- review request.
documentReviews_comment :: Lens.Lens' DocumentReviews (Core.Maybe [DocumentReviewCommentSource])
documentReviews_comment = Lens.lens (\DocumentReviews' {comment} -> comment) (\s@DocumentReviews' {} a -> s {comment = a} :: DocumentReviews) Core.. Lens.mapping Lens._Coerce

-- | The action to take on a document approval review request.
documentReviews_action :: Lens.Lens' DocumentReviews DocumentReviewAction
documentReviews_action = Lens.lens (\DocumentReviews' {action} -> action) (\s@DocumentReviews' {} a -> s {action = a} :: DocumentReviews)

instance Core.Hashable DocumentReviews

instance Core.NFData DocumentReviews

instance Core.ToJSON DocumentReviews where
  toJSON DocumentReviews' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Comment" Core..=) Core.<$> comment,
            Core.Just ("Action" Core..= action)
          ]
      )
