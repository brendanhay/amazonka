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
-- Module      : Amazonka.SSM.Types.DocumentReviewerResponseSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.DocumentReviewerResponseSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.DocumentReviewCommentSource
import Amazonka.SSM.Types.ReviewStatus

-- | Information about a reviewer\'s response to a document review request.
--
-- /See:/ 'newDocumentReviewerResponseSource' smart constructor.
data DocumentReviewerResponseSource = DocumentReviewerResponseSource'
  { -- | The user in your organization assigned to review a document request.
    reviewer :: Prelude.Maybe Prelude.Text,
    -- | The comment entered by a reviewer as part of their document review
    -- response.
    comment :: Prelude.Maybe [DocumentReviewCommentSource],
    -- | The date and time that a reviewer entered a response to a document
    -- review request.
    createTime :: Prelude.Maybe Core.POSIX,
    -- | The date and time that a reviewer last updated a response to a document
    -- review request.
    updatedTime :: Prelude.Maybe Core.POSIX,
    -- | The current review status of a new custom SSM document created by a
    -- member of your organization, or of the latest version of an existing SSM
    -- document.
    --
    -- Only one version of a document can be in the APPROVED state at a time.
    -- When a new version is approved, the status of the previous version
    -- changes to REJECTED.
    --
    -- Only one version of a document can be in review, or PENDING, at a time.
    reviewStatus :: Prelude.Maybe ReviewStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentReviewerResponseSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reviewer', 'documentReviewerResponseSource_reviewer' - The user in your organization assigned to review a document request.
--
-- 'comment', 'documentReviewerResponseSource_comment' - The comment entered by a reviewer as part of their document review
-- response.
--
-- 'createTime', 'documentReviewerResponseSource_createTime' - The date and time that a reviewer entered a response to a document
-- review request.
--
-- 'updatedTime', 'documentReviewerResponseSource_updatedTime' - The date and time that a reviewer last updated a response to a document
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
newDocumentReviewerResponseSource ::
  DocumentReviewerResponseSource
newDocumentReviewerResponseSource =
  DocumentReviewerResponseSource'
    { reviewer =
        Prelude.Nothing,
      comment = Prelude.Nothing,
      createTime = Prelude.Nothing,
      updatedTime = Prelude.Nothing,
      reviewStatus = Prelude.Nothing
    }

-- | The user in your organization assigned to review a document request.
documentReviewerResponseSource_reviewer :: Lens.Lens' DocumentReviewerResponseSource (Prelude.Maybe Prelude.Text)
documentReviewerResponseSource_reviewer = Lens.lens (\DocumentReviewerResponseSource' {reviewer} -> reviewer) (\s@DocumentReviewerResponseSource' {} a -> s {reviewer = a} :: DocumentReviewerResponseSource)

-- | The comment entered by a reviewer as part of their document review
-- response.
documentReviewerResponseSource_comment :: Lens.Lens' DocumentReviewerResponseSource (Prelude.Maybe [DocumentReviewCommentSource])
documentReviewerResponseSource_comment = Lens.lens (\DocumentReviewerResponseSource' {comment} -> comment) (\s@DocumentReviewerResponseSource' {} a -> s {comment = a} :: DocumentReviewerResponseSource) Prelude.. Lens.mapping Lens.coerced

-- | The date and time that a reviewer entered a response to a document
-- review request.
documentReviewerResponseSource_createTime :: Lens.Lens' DocumentReviewerResponseSource (Prelude.Maybe Prelude.UTCTime)
documentReviewerResponseSource_createTime = Lens.lens (\DocumentReviewerResponseSource' {createTime} -> createTime) (\s@DocumentReviewerResponseSource' {} a -> s {createTime = a} :: DocumentReviewerResponseSource) Prelude.. Lens.mapping Core._Time

-- | The date and time that a reviewer last updated a response to a document
-- review request.
documentReviewerResponseSource_updatedTime :: Lens.Lens' DocumentReviewerResponseSource (Prelude.Maybe Prelude.UTCTime)
documentReviewerResponseSource_updatedTime = Lens.lens (\DocumentReviewerResponseSource' {updatedTime} -> updatedTime) (\s@DocumentReviewerResponseSource' {} a -> s {updatedTime = a} :: DocumentReviewerResponseSource) Prelude.. Lens.mapping Core._Time

-- | The current review status of a new custom SSM document created by a
-- member of your organization, or of the latest version of an existing SSM
-- document.
--
-- Only one version of a document can be in the APPROVED state at a time.
-- When a new version is approved, the status of the previous version
-- changes to REJECTED.
--
-- Only one version of a document can be in review, or PENDING, at a time.
documentReviewerResponseSource_reviewStatus :: Lens.Lens' DocumentReviewerResponseSource (Prelude.Maybe ReviewStatus)
documentReviewerResponseSource_reviewStatus = Lens.lens (\DocumentReviewerResponseSource' {reviewStatus} -> reviewStatus) (\s@DocumentReviewerResponseSource' {} a -> s {reviewStatus = a} :: DocumentReviewerResponseSource)

instance Core.FromJSON DocumentReviewerResponseSource where
  parseJSON =
    Core.withObject
      "DocumentReviewerResponseSource"
      ( \x ->
          DocumentReviewerResponseSource'
            Prelude.<$> (x Core..:? "Reviewer")
            Prelude.<*> (x Core..:? "Comment" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "CreateTime")
            Prelude.<*> (x Core..:? "UpdatedTime")
            Prelude.<*> (x Core..:? "ReviewStatus")
      )

instance
  Prelude.Hashable
    DocumentReviewerResponseSource
  where
  hashWithSalt
    _salt
    DocumentReviewerResponseSource' {..} =
      _salt `Prelude.hashWithSalt` reviewer
        `Prelude.hashWithSalt` comment
        `Prelude.hashWithSalt` createTime
        `Prelude.hashWithSalt` updatedTime
        `Prelude.hashWithSalt` reviewStatus

instance
  Prelude.NFData
    DocumentReviewerResponseSource
  where
  rnf DocumentReviewerResponseSource' {..} =
    Prelude.rnf reviewer
      `Prelude.seq` Prelude.rnf comment
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf updatedTime
      `Prelude.seq` Prelude.rnf reviewStatus
