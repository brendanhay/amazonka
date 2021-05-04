{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.DocumentReviewAction
import Network.AWS.SSM.Types.DocumentReviewCommentSource

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
documentReviews_comment = Lens.lens (\DocumentReviews' {comment} -> comment) (\s@DocumentReviews' {} a -> s {comment = a} :: DocumentReviews) Prelude.. Lens.mapping Prelude._Coerce

-- | The action to take on a document approval review request.
documentReviews_action :: Lens.Lens' DocumentReviews DocumentReviewAction
documentReviews_action = Lens.lens (\DocumentReviews' {action} -> action) (\s@DocumentReviews' {} a -> s {action = a} :: DocumentReviews)

instance Prelude.Hashable DocumentReviews

instance Prelude.NFData DocumentReviews

instance Prelude.ToJSON DocumentReviews where
  toJSON DocumentReviews' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Comment" Prelude..=) Prelude.<$> comment,
            Prelude.Just ("Action" Prelude..= action)
          ]
      )
