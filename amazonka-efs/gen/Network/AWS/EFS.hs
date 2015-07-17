{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Amazon Elastic File System
module Network.AWS.EFS
    ( module Export
    ) where

import           Network.AWS.EFS.CreateFileSystem                  as Export
import           Network.AWS.EFS.CreateMountTarget                 as Export
import           Network.AWS.EFS.CreateTags                        as Export
import           Network.AWS.EFS.DeleteFileSystem                  as Export
import           Network.AWS.EFS.DeleteMountTarget                 as Export
import           Network.AWS.EFS.DeleteTags                        as Export
import           Network.AWS.EFS.DescribeFileSystems               as Export
import           Network.AWS.EFS.DescribeMountTargets              as Export
import           Network.AWS.EFS.DescribeMountTargetSecurityGroups as Export
import           Network.AWS.EFS.DescribeTags                      as Export
import           Network.AWS.EFS.ModifyMountTargetSecurityGroups   as Export
import           Network.AWS.EFS.Types                             as Export
import           Network.AWS.EFS.Types.Product                     as Export
import           Network.AWS.EFS.Types.Sum                         as Export
import           Network.AWS.EFS.Waiters                           as Export
