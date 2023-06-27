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
-- Module      : Amazonka.CodeGuruReviewer.Types.RepositoryAnalysis
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruReviewer.Types.RepositoryAnalysis where

import Amazonka.CodeGuruReviewer.Types.RepositoryHeadSourceCodeType
import Amazonka.CodeGuruReviewer.Types.SourceCodeType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A code review type that analyzes all code under a specified branch in an
-- associated repository. The associated repository is specified using its
-- ARN when you call
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_CreateCodeReview CreateCodeReview>.
--
-- /See:/ 'newRepositoryAnalysis' smart constructor.
data RepositoryAnalysis = RepositoryAnalysis'
  { -- | A
    -- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_SourceCodeType SourceCodeType>
    -- that specifies the tip of a branch in an associated repository.
    repositoryHead :: Prelude.Maybe RepositoryHeadSourceCodeType,
    sourceCodeType :: Prelude.Maybe SourceCodeType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RepositoryAnalysis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryHead', 'repositoryAnalysis_repositoryHead' - A
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_SourceCodeType SourceCodeType>
-- that specifies the tip of a branch in an associated repository.
--
-- 'sourceCodeType', 'repositoryAnalysis_sourceCodeType' - Undocumented member.
newRepositoryAnalysis ::
  RepositoryAnalysis
newRepositoryAnalysis =
  RepositoryAnalysis'
    { repositoryHead =
        Prelude.Nothing,
      sourceCodeType = Prelude.Nothing
    }

-- | A
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_SourceCodeType SourceCodeType>
-- that specifies the tip of a branch in an associated repository.
repositoryAnalysis_repositoryHead :: Lens.Lens' RepositoryAnalysis (Prelude.Maybe RepositoryHeadSourceCodeType)
repositoryAnalysis_repositoryHead = Lens.lens (\RepositoryAnalysis' {repositoryHead} -> repositoryHead) (\s@RepositoryAnalysis' {} a -> s {repositoryHead = a} :: RepositoryAnalysis)

-- | Undocumented member.
repositoryAnalysis_sourceCodeType :: Lens.Lens' RepositoryAnalysis (Prelude.Maybe SourceCodeType)
repositoryAnalysis_sourceCodeType = Lens.lens (\RepositoryAnalysis' {sourceCodeType} -> sourceCodeType) (\s@RepositoryAnalysis' {} a -> s {sourceCodeType = a} :: RepositoryAnalysis)

instance Prelude.Hashable RepositoryAnalysis where
  hashWithSalt _salt RepositoryAnalysis' {..} =
    _salt
      `Prelude.hashWithSalt` repositoryHead
      `Prelude.hashWithSalt` sourceCodeType

instance Prelude.NFData RepositoryAnalysis where
  rnf RepositoryAnalysis' {..} =
    Prelude.rnf repositoryHead
      `Prelude.seq` Prelude.rnf sourceCodeType

instance Data.ToJSON RepositoryAnalysis where
  toJSON RepositoryAnalysis' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RepositoryHead" Data..=)
              Prelude.<$> repositoryHead,
            ("SourceCodeType" Data..=)
              Prelude.<$> sourceCodeType
          ]
      )
