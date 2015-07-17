{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- AWS CodeCommit
--
-- This is the /AWS CodeCommit API Reference/. This reference provides
-- descriptions of the AWS CodeCommit API.
--
-- You can use the AWS CodeCommit API to work with the following objects:
--
-- -   Repositories
-- -   Branches
-- -   Commits
--
-- For information about how to use AWS CodeCommit, see the /AWS CodeCommit
-- User Guide/.
module Network.AWS.CodeCommit
    ( module Export
    ) where

import           Network.AWS.CodeCommit.BatchGetRepositories        as Export
import           Network.AWS.CodeCommit.CreateBranch                as Export
import           Network.AWS.CodeCommit.CreateRepository            as Export
import           Network.AWS.CodeCommit.DeleteRepository            as Export
import           Network.AWS.CodeCommit.GetBranch                   as Export
import           Network.AWS.CodeCommit.GetRepository               as Export
import           Network.AWS.CodeCommit.ListBranches                as Export
import           Network.AWS.CodeCommit.ListRepositories            as Export
import           Network.AWS.CodeCommit.Types                       as Export
import           Network.AWS.CodeCommit.Types.Product               as Export
import           Network.AWS.CodeCommit.Types.Sum                   as Export
import           Network.AWS.CodeCommit.UpdateDefaultBranch         as Export
import           Network.AWS.CodeCommit.UpdateRepositoryDescription as Export
import           Network.AWS.CodeCommit.UpdateRepositoryName        as Export
import           Network.AWS.CodeCommit.Waiters                     as Export
