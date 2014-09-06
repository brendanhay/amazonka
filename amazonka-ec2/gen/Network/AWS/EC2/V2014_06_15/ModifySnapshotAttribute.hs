{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.EC2.V2014_06_15.ModifySnapshotAttribute
    (
    -- * Request
      ModifySnapshotAttribute
    -- ** Request constructor
    , mkModifySnapshotAttribute
    -- ** Request lenses
    , msaSnapshotId
    , msaAttribute
    , msaOperationType
    , msaUserIds
    , msaGroupNames
    , msaCreateVolumePermission

    -- * Response
    , ModifySnapshotAttributeResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | 
data ModifySnapshotAttribute = ModifySnapshotAttribute
    { _msaSnapshotId :: Text
    , _msaAttribute :: Maybe SnapshotAttributeName
    , _msaOperationType :: Maybe Text
    , _msaUserIds :: [Text]
    , _msaGroupNames :: [Text]
    , _msaCreateVolumePermission :: Maybe CreateVolumePermissionModifications
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifySnapshotAttribute' request.
mkModifySnapshotAttribute :: Text -- ^ 'msaSnapshotId'
                          -> ModifySnapshotAttribute
mkModifySnapshotAttribute p1 = ModifySnapshotAttribute
    { _msaSnapshotId = p1
    , _msaAttribute = Nothing
    , _msaOperationType = Nothing
    , _msaUserIds = mempty
    , _msaGroupNames = mempty
    , _msaCreateVolumePermission = Nothing
    }
{-# INLINE mkModifySnapshotAttribute #-}

-- | The ID of the snapshot.
msaSnapshotId :: Lens' ModifySnapshotAttribute Text
msaSnapshotId = lens _msaSnapshotId (\s a -> s { _msaSnapshotId = a })
{-# INLINE msaSnapshotId #-}

-- | The snapshot attribute to modify.
msaAttribute :: Lens' ModifySnapshotAttribute (Maybe SnapshotAttributeName)
msaAttribute = lens _msaAttribute (\s a -> s { _msaAttribute = a })
{-# INLINE msaAttribute #-}

-- | The type of operation to perform to the attribute.
msaOperationType :: Lens' ModifySnapshotAttribute (Maybe Text)
msaOperationType =
    lens _msaOperationType (\s a -> s { _msaOperationType = a })
{-# INLINE msaOperationType #-}

-- | The account ID to modify for the snapshot.
msaUserIds :: Lens' ModifySnapshotAttribute [Text]
msaUserIds = lens _msaUserIds (\s a -> s { _msaUserIds = a })
{-# INLINE msaUserIds #-}

-- | The group to modify for the snapshot.
msaGroupNames :: Lens' ModifySnapshotAttribute [Text]
msaGroupNames = lens _msaGroupNames (\s a -> s { _msaGroupNames = a })
{-# INLINE msaGroupNames #-}

-- | A JSON representation of the snapshot attribute modification.
msaCreateVolumePermission :: Lens' ModifySnapshotAttribute (Maybe CreateVolumePermissionModifications)
msaCreateVolumePermission =
    lens _msaCreateVolumePermission
         (\s a -> s { _msaCreateVolumePermission = a })
{-# INLINE msaCreateVolumePermission #-}

instance ToQuery ModifySnapshotAttribute where
    toQuery = genericQuery def

data ModifySnapshotAttributeResponse = ModifySnapshotAttributeResponse
    deriving (Eq, Show, Generic)

instance AWSRequest ModifySnapshotAttribute where
    type Sv ModifySnapshotAttribute = EC2
    type Rs ModifySnapshotAttribute = ModifySnapshotAttributeResponse

    request = post "ModifySnapshotAttribute"
    response _ = nullaryResponse ModifySnapshotAttributeResponse
