{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.EC2.V2014_06_15.ModifySnapshotAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds or removes permission settings for the specified snapshot. You may add
-- or remove specified AWS account IDs from a snapshot's list of create volume
-- permissions, but you cannot do both in a single API call. If you need to
-- both add and remove account IDs for a snapshot, you must use multiple API
-- calls. For more information on modifying snapshot permissions, see Sharing
-- Snapshots in the Amazon Elastic Compute Cloud User Guide. Snapshots with
-- AWS Marketplace product codes cannot be made public. Example This example
-- makes the snap-1a2b3c4d snapshot public, and gives the account with ID
-- 111122223333 permission to create volumes from the snapshot.
-- https://ec2.amazonaws.com/?Action=ModifySnapshotAttribute
-- &amp;snapshotId=snap-1a2b3c4d
-- &amp;CreateVolumePermission.Add.1.UserId=111122223333
-- &amp;CreateVolumePermission.Add.1.Group=all &amp;AUTHPARAMS
-- &lt;ModifySnapshotAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-05-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/ModifySnapshotAttributeResponse&gt;
-- Example This example makes the snap-1a2b3c4d snapshot public, and removes
-- the account with ID 111122223333 from the list of users with permission to
-- create volumes from the snapshot.
-- https://ec2.amazonaws.com/?Action=ModifySnapshotAttribute
-- &amp;snapshotId=snap-1a2b3c4d
-- &amp;CreateVolumePermission.Remove.1.UserId=111122223333
-- &amp;CreateVolumePermission.Add.1.Group=all &amp;AUTHPARAMS
-- &lt;ModifySnapshotAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-05-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/ModifySnapshotAttributeResponse&gt;.
module Network.AWS.EC2.V2014_06_15.ModifySnapshotAttribute where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ModifySnapshotAttribute' request.
modifySnapshotAttribute :: Text -- ^ '_msasSnapshotId'
                        -> ModifySnapshotAttribute
modifySnapshotAttribute p1 = ModifySnapshotAttribute
    { _msasSnapshotId = p1
    , _msasDryRun = Nothing
    , _msasCreateVolumePermission = Nothing
    , _msasGroupNames = mempty
    , _msasAttribute = Nothing
    , _msasUserIds = mempty
    , _msasOperationType = Nothing
    }

data ModifySnapshotAttribute = ModifySnapshotAttribute
    { _msasSnapshotId :: Text
      -- ^ The ID of the snapshot.
    , _msasDryRun :: Maybe Bool
      -- ^ 
    , _msasCreateVolumePermission :: Maybe CreateVolumePermissionModifications
      -- ^ A JSON representation of the snapshot attribute modification.
    , _msasGroupNames :: [Text]
      -- ^ The group to modify for the snapshot.
    , _msasAttribute :: Maybe SnapshotAttributeName
      -- ^ The snapshot attribute to modify.
    , _msasUserIds :: [Text]
      -- ^ The account ID to modify for the snapshot.
    , _msasOperationType :: Maybe Text
      -- ^ The type of operation to perform to the attribute.
    } deriving (Show, Generic)

makeLenses ''ModifySnapshotAttribute

instance ToQuery ModifySnapshotAttribute where
    toQuery = genericQuery def

data ModifySnapshotAttributeResponse = ModifySnapshotAttributeResponse
    deriving (Eq, Show, Generic)

makeLenses ''ModifySnapshotAttributeResponse

instance AWSRequest ModifySnapshotAttribute where
    type Sv ModifySnapshotAttribute = EC2
    type Rs ModifySnapshotAttribute = ModifySnapshotAttributeResponse

    request = post "ModifySnapshotAttribute"
    response _ = nullaryResponse ModifySnapshotAttributeResponse
