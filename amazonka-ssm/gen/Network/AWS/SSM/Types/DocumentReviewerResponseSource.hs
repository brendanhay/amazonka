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
-- Module      : Network.AWS.SSM.Types.DocumentReviewerResponseSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentReviewerResponseSource where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.DocumentReviewCommentSource
import Network.AWS.SSM.Types.ReviewStatus

-- | Information about a reviewer\'s response to a document review request.
--
-- /See:/ 'newDocumentReviewerResponseSource' smart constructor.
data DocumentReviewerResponseSource = DocumentReviewerResponseSource'
  { -- | The comment entered by a reviewer as part of their document review
    -- response.
    comment :: Core.Maybe [DocumentReviewCommentSource],
    -- | The date and time that a reviewer last updated a response to a document
    -- review request.
    updatedTime :: Core.Maybe Core.POSIX,
    -- | The date and time that a reviewer entered a response to a document
    -- review request.
    createTime :: Core.Maybe Core.POSIX,
    -- | The current review status of a new custom SSM document created by a
    -- member of your organization, or of the latest version of an existing SSM
    -- document.
    --
    -- Only one version of a document can be in the APPROVED state at a time.
    -- When a new version is approved, the status of the previous version
    -- changes to REJECTED.
    --
    -- Only one version of a document can be in review, or PENDING, at a time.
    reviewStatus :: Core.Maybe ReviewStatus,
    -- | The user in your organization assigned to review a document request.
    reviewer :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DocumentReviewerResponseSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'documentReviewerResponseSource_comment' - The comment entered by a reviewer as part of their document review
-- response.
--
-- 'updatedTime', 'documentReviewerResponseSource_updatedTime' - The date and time that a reviewer last updated a response to a document
-- review request.
--
-- 'createTime', 'documentReviewerResponseSource_createTime' - The date and time that a reviewer entered a response to a document
-- review request.
--
-- 'reviewStatus', 'documentReviewerResponseSource_reviewStatus' - The current review status of a new custom SSM document created by a
-- member of your organization, or of the latest version of an existing SSM
-- document.
--
-- Only one version of a document can be in the APPROVED state at a time.
-- When a new version is approved, the status of the previous version
-- changes to REJECTED.
--
-- Only one version of a document can be in review, or PENDING, at a time.
--
-- 'reviewer', 'documentReviewerResponseSource_reviewer' - The user in your organization assigned to review a document request.
newDocumentReviewerResponseSource ::
  DocumentReviewerResponseSource
newDocumentReviewerResponseSource =
  DocumentReviewerResponseSource'
    { comment =
        Core.Nothing,
      updatedTime = Core.Nothing,
      createTime = Core.Nothing,
      reviewStatus = Core.Nothing,
      reviewer = Core.Nothing
    }

-- | The comment entered by a reviewer as part of their document review
-- response.
documentReviewerResponseSource_comment :: Lens.Lens' DocumentReviewerResponseSource (Core.Maybe [DocumentReviewCommentSource])
documentReviewerResponseSource_comment = Lens.lens (\DocumentReviewerResponseSource' {comment} -> comment) (\s@DocumentReviewerResponseSource' {} a -> s {comment = a} :: DocumentReviewerResponseSource) Core.. Lens.mapping Lens._Coerce

-- | The date and time that a reviewer last updated a response to a document
-- review request.
documentReviewerResponseSource_updatedTime :: Lens.Lens' DocumentReviewerResponseSource (Core.Maybe Core.UTCTime)
documentReviewerResponseSource_updatedTime = Lens.lens (\DocumentReviewerResponseSource' {updatedTime} -> updatedTime) (\s@DocumentReviewerResponseSource' {} a -> s {updatedTime = a} :: DocumentReviewerResponseSource) Core.. Lens.mapping Core._Time

-- | The date and time that a reviewer entered a response to a document
-- review request.
documentReviewerResponseSource_createTime :: Lens.Lens' DocumentReviewerResponseSource (Core.Maybe Core.UTCTime)
documentReviewerResponseSource_createTime = Lens.lens (\DocumentReviewerResponseSource' {createTime} -> createTime) (\s@DocumentReviewerResponseSource' {} a -> s {createTime = a} :: DocumentReviewerResponseSource) Core.. Lens.mapping Core._Time

-- | The current review status of a new custom SSM document created by a
-- member of your organization, or of the latest version of an existing SSM
-- document.
--
-- Only one version of a document can be in the APPROVED state at a time.
-- When a new version is approved, the status of the previous version
-- changes to REJECTED.
--
-- Only one version of a document can be in review, or PENDING, at a time.
documentReviewerResponseSource_reviewStatus :: Lens.Lens' DocumentReviewerResponseSource (Core.Maybe ReviewStatus)
documentReviewerResponseSource_reviewStatus = Lens.lens (\DocumentReviewerResponseSource' {reviewStatus} -> reviewStatus) (\s@DocumentReviewerResponseSource' {} a -> s {reviewStatus = a} :: DocumentReviewerResponseSource)

-- | The user in your organization assigned to review a document request.
documentReviewerResponseSource_reviewer :: Lens.Lens' DocumentReviewerResponseSource (Core.Maybe Core.Text)
documentReviewerResponseSource_reviewer = Lens.lens (\DocumentReviewerResponseSource' {reviewer} -> reviewer) (\s@DocumentReviewerResponseSource' {} a -> s {reviewer = a} :: DocumentReviewerResponseSource)

instance Core.FromJSON DocumentReviewerResponseSource where
  parseJSON =
    Core.withObject
      "DocumentReviewerResponseSource"
      ( \x ->
          DocumentReviewerResponseSource'
            Core.<$> (x Core..:? "Comment" Core..!= Core.mempty)
            Core.<*> (x Core..:? "UpdatedTime")
            Core.<*> (x Core..:? "CreateTime")
            Core.<*> (x Core..:? "ReviewStatus")
            Core.<*> (x Core..:? "Reviewer")
      )

instance Core.Hashable DocumentReviewerResponseSource

instance Core.NFData DocumentReviewerResponseSource
