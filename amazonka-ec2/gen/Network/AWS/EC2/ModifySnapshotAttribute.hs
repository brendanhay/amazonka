{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifySnapshotAttribute
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Adds or removes permission settings for the specified snapshot. You may
-- add or remove specified AWS account IDs from a snapshot\'s list of
-- create volume permissions, but you cannot do both in a single API call.
-- If you need to both add and remove account IDs for a snapshot, you must
-- use multiple API calls.
--
-- For more information on modifying snapshot permissions, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-modifying-snapshot-permissions.html Sharing Snapshots>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- Snapshots with AWS Marketplace product codes cannot be made public.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifySnapshotAttribute.html>
module Network.AWS.EC2.ModifySnapshotAttribute
    (
    -- * Request
      ModifySnapshotAttribute
    -- ** Request constructor
    , modifySnapshotAttribute
    -- ** Request lenses
    , msarqAttribute
    , msarqUserIds
    , msarqCreateVolumePermission
    , msarqGroupNames
    , msarqOperationType
    , msarqDryRun
    , msarqSnapshotId

    -- * Response
    , ModifySnapshotAttributeResponse
    -- ** Response constructor
    , modifySnapshotAttributeResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'modifySnapshotAttribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'msarqAttribute'
--
-- * 'msarqUserIds'
--
-- * 'msarqCreateVolumePermission'
--
-- * 'msarqGroupNames'
--
-- * 'msarqOperationType'
--
-- * 'msarqDryRun'
--
-- * 'msarqSnapshotId'
data ModifySnapshotAttribute = ModifySnapshotAttribute'
    { _msarqAttribute              :: !(Maybe ModifySnapshotAttributeName)
    , _msarqUserIds                :: !(Maybe [Text])
    , _msarqCreateVolumePermission :: !(Maybe CreateVolumePermissionModifications)
    , _msarqGroupNames             :: !(Maybe [Text])
    , _msarqOperationType          :: !(Maybe Text)
    , _msarqDryRun                 :: !(Maybe Bool)
    , _msarqSnapshotId             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifySnapshotAttribute' smart constructor.
modifySnapshotAttribute :: Text -> ModifySnapshotAttribute
modifySnapshotAttribute pSnapshotId_ =
    ModifySnapshotAttribute'
    { _msarqAttribute = Nothing
    , _msarqUserIds = Nothing
    , _msarqCreateVolumePermission = Nothing
    , _msarqGroupNames = Nothing
    , _msarqOperationType = Nothing
    , _msarqDryRun = Nothing
    , _msarqSnapshotId = pSnapshotId_
    }

-- | The snapshot attribute to modify.
msarqAttribute :: Lens' ModifySnapshotAttribute (Maybe ModifySnapshotAttributeName)
msarqAttribute = lens _msarqAttribute (\ s a -> s{_msarqAttribute = a});

-- | The account ID to modify for the snapshot.
msarqUserIds :: Lens' ModifySnapshotAttribute [Text]
msarqUserIds = lens _msarqUserIds (\ s a -> s{_msarqUserIds = a}) . _Default;

-- | A JSON representation of the snapshot attribute modification.
msarqCreateVolumePermission :: Lens' ModifySnapshotAttribute (Maybe CreateVolumePermissionModifications)
msarqCreateVolumePermission = lens _msarqCreateVolumePermission (\ s a -> s{_msarqCreateVolumePermission = a});

-- | The group to modify for the snapshot.
msarqGroupNames :: Lens' ModifySnapshotAttribute [Text]
msarqGroupNames = lens _msarqGroupNames (\ s a -> s{_msarqGroupNames = a}) . _Default;

-- | The type of operation to perform to the attribute.
msarqOperationType :: Lens' ModifySnapshotAttribute (Maybe Text)
msarqOperationType = lens _msarqOperationType (\ s a -> s{_msarqOperationType = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
msarqDryRun :: Lens' ModifySnapshotAttribute (Maybe Bool)
msarqDryRun = lens _msarqDryRun (\ s a -> s{_msarqDryRun = a});

-- | The ID of the snapshot.
msarqSnapshotId :: Lens' ModifySnapshotAttribute Text
msarqSnapshotId = lens _msarqSnapshotId (\ s a -> s{_msarqSnapshotId = a});

instance AWSRequest ModifySnapshotAttribute where
        type Sv ModifySnapshotAttribute = EC2
        type Rs ModifySnapshotAttribute =
             ModifySnapshotAttributeResponse
        request = post
        response
          = receiveNull ModifySnapshotAttributeResponse'

instance ToHeaders ModifySnapshotAttribute where
        toHeaders = const mempty

instance ToPath ModifySnapshotAttribute where
        toPath = const "/"

instance ToQuery ModifySnapshotAttribute where
        toQuery ModifySnapshotAttribute'{..}
          = mconcat
              ["Action" =:
                 ("ModifySnapshotAttribute" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "Attribute" =: _msarqAttribute,
               toQuery (toQueryList "UserId" <$> _msarqUserIds),
               "CreateVolumePermission" =:
                 _msarqCreateVolumePermission,
               toQuery
                 (toQueryList "GroupName" <$> _msarqGroupNames),
               "OperationType" =: _msarqOperationType,
               "DryRun" =: _msarqDryRun,
               "SnapshotId" =: _msarqSnapshotId]

-- | /See:/ 'modifySnapshotAttributeResponse' smart constructor.
data ModifySnapshotAttributeResponse =
    ModifySnapshotAttributeResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifySnapshotAttributeResponse' smart constructor.
modifySnapshotAttributeResponse :: ModifySnapshotAttributeResponse
modifySnapshotAttributeResponse = ModifySnapshotAttributeResponse'
