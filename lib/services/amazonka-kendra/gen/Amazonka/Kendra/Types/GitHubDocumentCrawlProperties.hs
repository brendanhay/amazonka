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
-- Module      : Amazonka.Kendra.Types.GitHubDocumentCrawlProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.GitHubDocumentCrawlProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information to include certain types of
-- GitHub content. You can configure to index repository files only, or
-- also include issues and pull requests, comments, and comment
-- attachments.
--
-- /See:/ 'newGitHubDocumentCrawlProperties' smart constructor.
data GitHubDocumentCrawlProperties = GitHubDocumentCrawlProperties'
  { -- | @TRUE@ to index all comments on pull requests.
    crawlPullRequestComment :: Prelude.Maybe Prelude.Bool,
    -- | @TRUE@ to index all files with a repository.
    crawlRepositoryDocuments :: Prelude.Maybe Prelude.Bool,
    -- | @TRUE@ to include all comment attachments for pull requests.
    crawlPullRequestCommentAttachment :: Prelude.Maybe Prelude.Bool,
    -- | @TRUE@ to index all pull requests within a repository.
    crawlPullRequest :: Prelude.Maybe Prelude.Bool,
    -- | @TRUE@ to index all comments on issues.
    crawlIssueComment :: Prelude.Maybe Prelude.Bool,
    -- | @TRUE@ to include all comment attachments for issues.
    crawlIssueCommentAttachment :: Prelude.Maybe Prelude.Bool,
    -- | @TRUE@ to index all issues within a repository.
    crawlIssue :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GitHubDocumentCrawlProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crawlPullRequestComment', 'gitHubDocumentCrawlProperties_crawlPullRequestComment' - @TRUE@ to index all comments on pull requests.
--
-- 'crawlRepositoryDocuments', 'gitHubDocumentCrawlProperties_crawlRepositoryDocuments' - @TRUE@ to index all files with a repository.
--
-- 'crawlPullRequestCommentAttachment', 'gitHubDocumentCrawlProperties_crawlPullRequestCommentAttachment' - @TRUE@ to include all comment attachments for pull requests.
--
-- 'crawlPullRequest', 'gitHubDocumentCrawlProperties_crawlPullRequest' - @TRUE@ to index all pull requests within a repository.
--
-- 'crawlIssueComment', 'gitHubDocumentCrawlProperties_crawlIssueComment' - @TRUE@ to index all comments on issues.
--
-- 'crawlIssueCommentAttachment', 'gitHubDocumentCrawlProperties_crawlIssueCommentAttachment' - @TRUE@ to include all comment attachments for issues.
--
-- 'crawlIssue', 'gitHubDocumentCrawlProperties_crawlIssue' - @TRUE@ to index all issues within a repository.
newGitHubDocumentCrawlProperties ::
  GitHubDocumentCrawlProperties
newGitHubDocumentCrawlProperties =
  GitHubDocumentCrawlProperties'
    { crawlPullRequestComment =
        Prelude.Nothing,
      crawlRepositoryDocuments = Prelude.Nothing,
      crawlPullRequestCommentAttachment =
        Prelude.Nothing,
      crawlPullRequest = Prelude.Nothing,
      crawlIssueComment = Prelude.Nothing,
      crawlIssueCommentAttachment =
        Prelude.Nothing,
      crawlIssue = Prelude.Nothing
    }

-- | @TRUE@ to index all comments on pull requests.
gitHubDocumentCrawlProperties_crawlPullRequestComment :: Lens.Lens' GitHubDocumentCrawlProperties (Prelude.Maybe Prelude.Bool)
gitHubDocumentCrawlProperties_crawlPullRequestComment = Lens.lens (\GitHubDocumentCrawlProperties' {crawlPullRequestComment} -> crawlPullRequestComment) (\s@GitHubDocumentCrawlProperties' {} a -> s {crawlPullRequestComment = a} :: GitHubDocumentCrawlProperties)

-- | @TRUE@ to index all files with a repository.
gitHubDocumentCrawlProperties_crawlRepositoryDocuments :: Lens.Lens' GitHubDocumentCrawlProperties (Prelude.Maybe Prelude.Bool)
gitHubDocumentCrawlProperties_crawlRepositoryDocuments = Lens.lens (\GitHubDocumentCrawlProperties' {crawlRepositoryDocuments} -> crawlRepositoryDocuments) (\s@GitHubDocumentCrawlProperties' {} a -> s {crawlRepositoryDocuments = a} :: GitHubDocumentCrawlProperties)

-- | @TRUE@ to include all comment attachments for pull requests.
gitHubDocumentCrawlProperties_crawlPullRequestCommentAttachment :: Lens.Lens' GitHubDocumentCrawlProperties (Prelude.Maybe Prelude.Bool)
gitHubDocumentCrawlProperties_crawlPullRequestCommentAttachment = Lens.lens (\GitHubDocumentCrawlProperties' {crawlPullRequestCommentAttachment} -> crawlPullRequestCommentAttachment) (\s@GitHubDocumentCrawlProperties' {} a -> s {crawlPullRequestCommentAttachment = a} :: GitHubDocumentCrawlProperties)

-- | @TRUE@ to index all pull requests within a repository.
gitHubDocumentCrawlProperties_crawlPullRequest :: Lens.Lens' GitHubDocumentCrawlProperties (Prelude.Maybe Prelude.Bool)
gitHubDocumentCrawlProperties_crawlPullRequest = Lens.lens (\GitHubDocumentCrawlProperties' {crawlPullRequest} -> crawlPullRequest) (\s@GitHubDocumentCrawlProperties' {} a -> s {crawlPullRequest = a} :: GitHubDocumentCrawlProperties)

-- | @TRUE@ to index all comments on issues.
gitHubDocumentCrawlProperties_crawlIssueComment :: Lens.Lens' GitHubDocumentCrawlProperties (Prelude.Maybe Prelude.Bool)
gitHubDocumentCrawlProperties_crawlIssueComment = Lens.lens (\GitHubDocumentCrawlProperties' {crawlIssueComment} -> crawlIssueComment) (\s@GitHubDocumentCrawlProperties' {} a -> s {crawlIssueComment = a} :: GitHubDocumentCrawlProperties)

-- | @TRUE@ to include all comment attachments for issues.
gitHubDocumentCrawlProperties_crawlIssueCommentAttachment :: Lens.Lens' GitHubDocumentCrawlProperties (Prelude.Maybe Prelude.Bool)
gitHubDocumentCrawlProperties_crawlIssueCommentAttachment = Lens.lens (\GitHubDocumentCrawlProperties' {crawlIssueCommentAttachment} -> crawlIssueCommentAttachment) (\s@GitHubDocumentCrawlProperties' {} a -> s {crawlIssueCommentAttachment = a} :: GitHubDocumentCrawlProperties)

-- | @TRUE@ to index all issues within a repository.
gitHubDocumentCrawlProperties_crawlIssue :: Lens.Lens' GitHubDocumentCrawlProperties (Prelude.Maybe Prelude.Bool)
gitHubDocumentCrawlProperties_crawlIssue = Lens.lens (\GitHubDocumentCrawlProperties' {crawlIssue} -> crawlIssue) (\s@GitHubDocumentCrawlProperties' {} a -> s {crawlIssue = a} :: GitHubDocumentCrawlProperties)

instance Core.FromJSON GitHubDocumentCrawlProperties where
  parseJSON =
    Core.withObject
      "GitHubDocumentCrawlProperties"
      ( \x ->
          GitHubDocumentCrawlProperties'
            Prelude.<$> (x Core..:? "CrawlPullRequestComment")
            Prelude.<*> (x Core..:? "CrawlRepositoryDocuments")
            Prelude.<*> (x Core..:? "CrawlPullRequestCommentAttachment")
            Prelude.<*> (x Core..:? "CrawlPullRequest")
            Prelude.<*> (x Core..:? "CrawlIssueComment")
            Prelude.<*> (x Core..:? "CrawlIssueCommentAttachment")
            Prelude.<*> (x Core..:? "CrawlIssue")
      )

instance
  Prelude.Hashable
    GitHubDocumentCrawlProperties
  where
  hashWithSalt _salt GitHubDocumentCrawlProperties' {..} =
    _salt
      `Prelude.hashWithSalt` crawlPullRequestComment
      `Prelude.hashWithSalt` crawlRepositoryDocuments
      `Prelude.hashWithSalt` crawlPullRequestCommentAttachment
      `Prelude.hashWithSalt` crawlPullRequest
      `Prelude.hashWithSalt` crawlIssueComment
      `Prelude.hashWithSalt` crawlIssueCommentAttachment
      `Prelude.hashWithSalt` crawlIssue

instance Prelude.NFData GitHubDocumentCrawlProperties where
  rnf GitHubDocumentCrawlProperties' {..} =
    Prelude.rnf crawlPullRequestComment
      `Prelude.seq` Prelude.rnf crawlRepositoryDocuments
      `Prelude.seq` Prelude.rnf crawlPullRequestCommentAttachment
      `Prelude.seq` Prelude.rnf crawlPullRequest
      `Prelude.seq` Prelude.rnf crawlIssueComment
      `Prelude.seq` Prelude.rnf crawlIssueCommentAttachment
      `Prelude.seq` Prelude.rnf crawlIssue

instance Core.ToJSON GitHubDocumentCrawlProperties where
  toJSON GitHubDocumentCrawlProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CrawlPullRequestComment" Core..=)
              Prelude.<$> crawlPullRequestComment,
            ("CrawlRepositoryDocuments" Core..=)
              Prelude.<$> crawlRepositoryDocuments,
            ("CrawlPullRequestCommentAttachment" Core..=)
              Prelude.<$> crawlPullRequestCommentAttachment,
            ("CrawlPullRequest" Core..=)
              Prelude.<$> crawlPullRequest,
            ("CrawlIssueComment" Core..=)
              Prelude.<$> crawlIssueComment,
            ("CrawlIssueCommentAttachment" Core..=)
              Prelude.<$> crawlIssueCommentAttachment,
            ("CrawlIssue" Core..=) Prelude.<$> crawlIssue
          ]
      )
