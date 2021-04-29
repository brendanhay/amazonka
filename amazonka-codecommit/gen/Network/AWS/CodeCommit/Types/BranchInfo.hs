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
-- Module      : Network.AWS.CodeCommit.Types.BranchInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.BranchInfo where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Returns information about a branch.
--
-- /See:/ 'newBranchInfo' smart constructor.
data BranchInfo = BranchInfo'
  { -- | The ID of the last commit made to the branch.
    commitId :: Prelude.Maybe Prelude.Text,
    -- | The name of the branch.
    branchName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BranchInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commitId', 'branchInfo_commitId' - The ID of the last commit made to the branch.
--
-- 'branchName', 'branchInfo_branchName' - The name of the branch.
newBranchInfo ::
  BranchInfo
newBranchInfo =
  BranchInfo'
    { commitId = Prelude.Nothing,
      branchName = Prelude.Nothing
    }

-- | The ID of the last commit made to the branch.
branchInfo_commitId :: Lens.Lens' BranchInfo (Prelude.Maybe Prelude.Text)
branchInfo_commitId = Lens.lens (\BranchInfo' {commitId} -> commitId) (\s@BranchInfo' {} a -> s {commitId = a} :: BranchInfo)

-- | The name of the branch.
branchInfo_branchName :: Lens.Lens' BranchInfo (Prelude.Maybe Prelude.Text)
branchInfo_branchName = Lens.lens (\BranchInfo' {branchName} -> branchName) (\s@BranchInfo' {} a -> s {branchName = a} :: BranchInfo)

instance Prelude.FromJSON BranchInfo where
  parseJSON =
    Prelude.withObject
      "BranchInfo"
      ( \x ->
          BranchInfo'
            Prelude.<$> (x Prelude..:? "commitId")
            Prelude.<*> (x Prelude..:? "branchName")
      )

instance Prelude.Hashable BranchInfo

instance Prelude.NFData BranchInfo
