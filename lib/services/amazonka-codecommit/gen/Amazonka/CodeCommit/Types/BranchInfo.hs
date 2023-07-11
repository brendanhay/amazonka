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
-- Module      : Amazonka.CodeCommit.Types.BranchInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.BranchInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns information about a branch.
--
-- /See:/ 'newBranchInfo' smart constructor.
data BranchInfo = BranchInfo'
  { -- | The name of the branch.
    branchName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the last commit made to the branch.
    commitId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BranchInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'branchName', 'branchInfo_branchName' - The name of the branch.
--
-- 'commitId', 'branchInfo_commitId' - The ID of the last commit made to the branch.
newBranchInfo ::
  BranchInfo
newBranchInfo =
  BranchInfo'
    { branchName = Prelude.Nothing,
      commitId = Prelude.Nothing
    }

-- | The name of the branch.
branchInfo_branchName :: Lens.Lens' BranchInfo (Prelude.Maybe Prelude.Text)
branchInfo_branchName = Lens.lens (\BranchInfo' {branchName} -> branchName) (\s@BranchInfo' {} a -> s {branchName = a} :: BranchInfo)

-- | The ID of the last commit made to the branch.
branchInfo_commitId :: Lens.Lens' BranchInfo (Prelude.Maybe Prelude.Text)
branchInfo_commitId = Lens.lens (\BranchInfo' {commitId} -> commitId) (\s@BranchInfo' {} a -> s {commitId = a} :: BranchInfo)

instance Data.FromJSON BranchInfo where
  parseJSON =
    Data.withObject
      "BranchInfo"
      ( \x ->
          BranchInfo'
            Prelude.<$> (x Data..:? "branchName")
            Prelude.<*> (x Data..:? "commitId")
      )

instance Prelude.Hashable BranchInfo where
  hashWithSalt _salt BranchInfo' {..} =
    _salt
      `Prelude.hashWithSalt` branchName
      `Prelude.hashWithSalt` commitId

instance Prelude.NFData BranchInfo where
  rnf BranchInfo' {..} =
    Prelude.rnf branchName
      `Prelude.seq` Prelude.rnf commitId
