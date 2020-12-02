{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.BranchInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.BranchInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about a branch.
--
--
--
-- /See:/ 'branchInfo' smart constructor.
data BranchInfo = BranchInfo'
  { _biCommitId :: !(Maybe Text),
    _biBranchName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BranchInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'biCommitId' - The ID of the last commit made to the branch.
--
-- * 'biBranchName' - The name of the branch.
branchInfo ::
  BranchInfo
branchInfo =
  BranchInfo' {_biCommitId = Nothing, _biBranchName = Nothing}

-- | The ID of the last commit made to the branch.
biCommitId :: Lens' BranchInfo (Maybe Text)
biCommitId = lens _biCommitId (\s a -> s {_biCommitId = a})

-- | The name of the branch.
biBranchName :: Lens' BranchInfo (Maybe Text)
biBranchName = lens _biBranchName (\s a -> s {_biBranchName = a})

instance FromJSON BranchInfo where
  parseJSON =
    withObject
      "BranchInfo"
      ( \x ->
          BranchInfo' <$> (x .:? "commitId") <*> (x .:? "branchName")
      )

instance Hashable BranchInfo

instance NFData BranchInfo
