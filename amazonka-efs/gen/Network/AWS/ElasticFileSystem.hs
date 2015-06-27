-- Module      : Network.AWS.ElasticFileSystem
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Amazon Elastic File System
module Network.AWS.ElasticFileSystem
    ( module Export
    ) where

import           Network.AWS.ElasticFileSystem.CreateFileSystem                  as Export
import           Network.AWS.ElasticFileSystem.CreateMountTarget                 as Export
import           Network.AWS.ElasticFileSystem.CreateTags                        as Export
import           Network.AWS.ElasticFileSystem.DeleteFileSystem                  as Export
import           Network.AWS.ElasticFileSystem.DeleteMountTarget                 as Export
import           Network.AWS.ElasticFileSystem.DeleteTags                        as Export
import           Network.AWS.ElasticFileSystem.DescribeFileSystems               as Export
import           Network.AWS.ElasticFileSystem.DescribeMountTargets              as Export
import           Network.AWS.ElasticFileSystem.DescribeMountTargetSecurityGroups as Export
import           Network.AWS.ElasticFileSystem.DescribeTags                      as Export
import           Network.AWS.ElasticFileSystem.ModifyMountTargetSecurityGroups   as Export
import           Network.AWS.ElasticFileSystem.Types                             as Export
import           Network.AWS.ElasticFileSystem.Waiters                           as Export
