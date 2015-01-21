{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ModifySnapshotAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Adds or removes permission settings for the specified snapshot. You may add
-- or remove specified AWS account IDs from a snapshot's list of create volume
-- permissions, but you cannot do both in a single API call. If you need to both
-- add and remove account IDs for a snapshot, you must use multiple API calls.
--
-- For more information on modifying snapshot permissions, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-modifying-snapshot-permissions.html Sharing Snapshots>
-- in the /Amazon Elastic Compute Cloud User Guide for Linux/.
--
-- Snapshots with AWS Marketplace product codes cannot be made public.
--
--
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifySnapshotAttribute.html>
module Network.AWS.EC2.ModifySnapshotAttribute
    (
    -- * Request
      ModifySnapshotAttribute
    -- ** Request constructor
    , modifySnapshotAttribute
    -- ** Request lenses
    , msaAttribute
    , msaCreateVolumePermission
    , msaDryRun
    , msaGroupNames
    , msaOperationType
    , msaSnapshotId
    , msaUserIds

    -- * Response
    , ModifySnapshotAttributeResponse
    -- ** Response constructor
    , modifySnapshotAttributeResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data ModifySnapshotAttribute = ModifySnapshotAttribute
    { _msaAttribute              :: Maybe SnapshotAttributeName
    , _msaCreateVolumePermission :: Maybe CreateVolumePermissionModifications
    , _msaDryRun                 :: Maybe Bool
    , _msaGroupNames             :: List "GroupName" Text
    , _msaOperationType          :: Maybe Text
    , _msaSnapshotId             :: Text
    , _msaUserIds                :: List "UserId" Text
    } deriving (Eq, Read, Show)

-- | 'ModifySnapshotAttribute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'msaAttribute' @::@ 'Maybe' 'SnapshotAttributeName'
--
-- * 'msaCreateVolumePermission' @::@ 'Maybe' 'CreateVolumePermissionModifications'
--
-- * 'msaDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'msaGroupNames' @::@ ['Text']
--
-- * 'msaOperationType' @::@ 'Maybe' 'Text'
--
-- * 'msaSnapshotId' @::@ 'Text'
--
-- * 'msaUserIds' @::@ ['Text']
--
modifySnapshotAttribute :: Text -- ^ 'msaSnapshotId'
                        -> ModifySnapshotAttribute
modifySnapshotAttribute p1 = ModifySnapshotAttribute
    { _msaSnapshotId             = p1
    , _msaDryRun                 = Nothing
    , _msaAttribute              = Nothing
    , _msaOperationType          = Nothing
    , _msaUserIds                = mempty
    , _msaGroupNames             = mempty
    , _msaCreateVolumePermission = Nothing
    }

-- | The snapshot attribute to modify.
msaAttribute :: Lens' ModifySnapshotAttribute (Maybe SnapshotAttributeName)
msaAttribute = lens _msaAttribute (\s a -> s { _msaAttribute = a })

-- | A JSON representation of the snapshot attribute modification.
msaCreateVolumePermission :: Lens' ModifySnapshotAttribute (Maybe CreateVolumePermissionModifications)
msaCreateVolumePermission =
    lens _msaCreateVolumePermission
        (\s a -> s { _msaCreateVolumePermission = a })

msaDryRun :: Lens' ModifySnapshotAttribute (Maybe Bool)
msaDryRun = lens _msaDryRun (\s a -> s { _msaDryRun = a })

-- | The group to modify for the snapshot.
msaGroupNames :: Lens' ModifySnapshotAttribute [Text]
msaGroupNames = lens _msaGroupNames (\s a -> s { _msaGroupNames = a }) . _List

-- | The type of operation to perform to the attribute.
msaOperationType :: Lens' ModifySnapshotAttribute (Maybe Text)
msaOperationType = lens _msaOperationType (\s a -> s { _msaOperationType = a })

-- | The ID of the snapshot.
msaSnapshotId :: Lens' ModifySnapshotAttribute Text
msaSnapshotId = lens _msaSnapshotId (\s a -> s { _msaSnapshotId = a })

-- | The account ID to modify for the snapshot.
msaUserIds :: Lens' ModifySnapshotAttribute [Text]
msaUserIds = lens _msaUserIds (\s a -> s { _msaUserIds = a }) . _List

data ModifySnapshotAttributeResponse = ModifySnapshotAttributeResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'ModifySnapshotAttributeResponse' constructor.
modifySnapshotAttributeResponse :: ModifySnapshotAttributeResponse
modifySnapshotAttributeResponse = ModifySnapshotAttributeResponse

instance ToPath ModifySnapshotAttribute where
    toPath = const "/"

instance ToQuery ModifySnapshotAttribute where
    toQuery ModifySnapshotAttribute{..} = mconcat
        [ "Attribute"              =? _msaAttribute
        , "CreateVolumePermission" =? _msaCreateVolumePermission
        , "dryRun"                 =? _msaDryRun
        , "UserGroup"              `toQueryList` _msaGroupNames
        , "OperationType"          =? _msaOperationType
        , "SnapshotId"             =? _msaSnapshotId
        , "UserId"                 `toQueryList` _msaUserIds
        ]

instance ToHeaders ModifySnapshotAttribute

instance AWSRequest ModifySnapshotAttribute where
    type Sv ModifySnapshotAttribute = EC2
    type Rs ModifySnapshotAttribute = ModifySnapshotAttributeResponse

    request  = post "ModifySnapshotAttribute"
    response = nullResponse ModifySnapshotAttributeResponse
