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
-- Module      : Amazonka.CodeGuruReviewer.Types.CodeReviewType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruReviewer.Types.CodeReviewType where

import Amazonka.CodeGuruReviewer.Types.AnalysisType
import Amazonka.CodeGuruReviewer.Types.RepositoryAnalysis
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The type of a code review. There are two code review types:
--
-- -   @PullRequest@ - A code review that is automatically triggered by a
--     pull request on an associated repository.
--
-- -   @RepositoryAnalysis@ - A code review that analyzes all code under a
--     specified branch in an associated repository. The associated
--     repository is specified using its ARN in
--     <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_CreateCodeReview CreateCodeReview>.
--
-- /See:/ 'newCodeReviewType' smart constructor.
data CodeReviewType = CodeReviewType'
  { -- | They types of analysis performed during a repository analysis or a pull
    -- request review. You can specify either @Security@, @CodeQuality@, or
    -- both.
    analysisTypes :: Prelude.Maybe [AnalysisType],
    -- | A code review that analyzes all code under a specified branch in an
    -- associated repository. The associated repository is specified using its
    -- ARN in
    -- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_CreateCodeReview CreateCodeReview>.
    repositoryAnalysis :: RepositoryAnalysis
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodeReviewType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analysisTypes', 'codeReviewType_analysisTypes' - They types of analysis performed during a repository analysis or a pull
-- request review. You can specify either @Security@, @CodeQuality@, or
-- both.
--
-- 'repositoryAnalysis', 'codeReviewType_repositoryAnalysis' - A code review that analyzes all code under a specified branch in an
-- associated repository. The associated repository is specified using its
-- ARN in
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_CreateCodeReview CreateCodeReview>.
newCodeReviewType ::
  -- | 'repositoryAnalysis'
  RepositoryAnalysis ->
  CodeReviewType
newCodeReviewType pRepositoryAnalysis_ =
  CodeReviewType'
    { analysisTypes = Prelude.Nothing,
      repositoryAnalysis = pRepositoryAnalysis_
    }

-- | They types of analysis performed during a repository analysis or a pull
-- request review. You can specify either @Security@, @CodeQuality@, or
-- both.
codeReviewType_analysisTypes :: Lens.Lens' CodeReviewType (Prelude.Maybe [AnalysisType])
codeReviewType_analysisTypes = Lens.lens (\CodeReviewType' {analysisTypes} -> analysisTypes) (\s@CodeReviewType' {} a -> s {analysisTypes = a} :: CodeReviewType) Prelude.. Lens.mapping Lens.coerced

-- | A code review that analyzes all code under a specified branch in an
-- associated repository. The associated repository is specified using its
-- ARN in
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_CreateCodeReview CreateCodeReview>.
codeReviewType_repositoryAnalysis :: Lens.Lens' CodeReviewType RepositoryAnalysis
codeReviewType_repositoryAnalysis = Lens.lens (\CodeReviewType' {repositoryAnalysis} -> repositoryAnalysis) (\s@CodeReviewType' {} a -> s {repositoryAnalysis = a} :: CodeReviewType)

instance Prelude.Hashable CodeReviewType where
  hashWithSalt _salt CodeReviewType' {..} =
    _salt `Prelude.hashWithSalt` analysisTypes
      `Prelude.hashWithSalt` repositoryAnalysis

instance Prelude.NFData CodeReviewType where
  rnf CodeReviewType' {..} =
    Prelude.rnf analysisTypes
      `Prelude.seq` Prelude.rnf repositoryAnalysis

instance Data.ToJSON CodeReviewType where
  toJSON CodeReviewType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AnalysisTypes" Data..=) Prelude.<$> analysisTypes,
            Prelude.Just
              ("RepositoryAnalysis" Data..= repositoryAnalysis)
          ]
      )
