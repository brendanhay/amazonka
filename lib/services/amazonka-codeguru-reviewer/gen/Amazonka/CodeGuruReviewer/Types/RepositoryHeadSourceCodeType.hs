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
-- Module      : Amazonka.CodeGuruReviewer.Types.RepositoryHeadSourceCodeType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruReviewer.Types.RepositoryHeadSourceCodeType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_SourceCodeType SourceCodeType>
-- that specifies the tip of a branch in an associated repository.
--
-- /See:/ 'newRepositoryHeadSourceCodeType' smart constructor.
data RepositoryHeadSourceCodeType = RepositoryHeadSourceCodeType'
  { -- | The name of the branch in an associated repository. The
    -- @RepositoryHeadSourceCodeType@ specifies the tip of this branch.
    branchName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RepositoryHeadSourceCodeType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'branchName', 'repositoryHeadSourceCodeType_branchName' - The name of the branch in an associated repository. The
-- @RepositoryHeadSourceCodeType@ specifies the tip of this branch.
newRepositoryHeadSourceCodeType ::
  -- | 'branchName'
  Prelude.Text ->
  RepositoryHeadSourceCodeType
newRepositoryHeadSourceCodeType pBranchName_ =
  RepositoryHeadSourceCodeType'
    { branchName =
        pBranchName_
    }

-- | The name of the branch in an associated repository. The
-- @RepositoryHeadSourceCodeType@ specifies the tip of this branch.
repositoryHeadSourceCodeType_branchName :: Lens.Lens' RepositoryHeadSourceCodeType Prelude.Text
repositoryHeadSourceCodeType_branchName = Lens.lens (\RepositoryHeadSourceCodeType' {branchName} -> branchName) (\s@RepositoryHeadSourceCodeType' {} a -> s {branchName = a} :: RepositoryHeadSourceCodeType)

instance Data.FromJSON RepositoryHeadSourceCodeType where
  parseJSON =
    Data.withObject
      "RepositoryHeadSourceCodeType"
      ( \x ->
          RepositoryHeadSourceCodeType'
            Prelude.<$> (x Data..: "BranchName")
      )

instance
  Prelude.Hashable
    RepositoryHeadSourceCodeType
  where
  hashWithSalt _salt RepositoryHeadSourceCodeType' {..} =
    _salt `Prelude.hashWithSalt` branchName

instance Prelude.NFData RepositoryHeadSourceCodeType where
  rnf RepositoryHeadSourceCodeType' {..} =
    Prelude.rnf branchName

instance Data.ToJSON RepositoryHeadSourceCodeType where
  toJSON RepositoryHeadSourceCodeType' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("BranchName" Data..= branchName)]
      )
