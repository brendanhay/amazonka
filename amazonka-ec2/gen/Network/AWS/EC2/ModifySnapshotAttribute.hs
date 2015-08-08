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
-- Stability   : auto-generated
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
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifySnapshotAttribute.html AWS API Reference> for ModifySnapshotAttribute.
module Network.AWS.EC2.ModifySnapshotAttribute
    (
    -- * Creating a Request
      ModifySnapshotAttribute
    , modifySnapshotAttribute
    -- * Request Lenses
    , msaAttribute
    , msaUserIds
    , msaCreateVolumePermission
    , msaGroupNames
    , msaOperationType
    , msaDryRun
    , msaSnapshotId

    -- * Destructuring the Response
    , ModifySnapshotAttributeResponse
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
-- * 'msaAttribute'
--
-- * 'msaUserIds'
--
-- * 'msaCreateVolumePermission'
--
-- * 'msaGroupNames'
--
-- * 'msaOperationType'
--
-- * 'msaDryRun'
--
-- * 'msaSnapshotId'
data ModifySnapshotAttribute = ModifySnapshotAttribute'
    { _msaAttribute              :: !(Maybe SnapshotAttributeName)
    , _msaUserIds                :: !(Maybe [Text])
    , _msaCreateVolumePermission :: !(Maybe CreateVolumePermissionModifications)
    , _msaGroupNames             :: !(Maybe [Text])
    , _msaOperationType          :: !(Maybe Text)
    , _msaDryRun                 :: !(Maybe Bool)
    , _msaSnapshotId             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifySnapshotAttribute' smart constructor.
modifySnapshotAttribute :: Text -> ModifySnapshotAttribute
modifySnapshotAttribute pSnapshotId_ =
    ModifySnapshotAttribute'
    { _msaAttribute = Nothing
    , _msaUserIds = Nothing
    , _msaCreateVolumePermission = Nothing
    , _msaGroupNames = Nothing
    , _msaOperationType = Nothing
    , _msaDryRun = Nothing
    , _msaSnapshotId = pSnapshotId_
    }

-- | The snapshot attribute to modify.
--
-- Only volume creation permissions may be modified at the customer level.
msaAttribute :: Lens' ModifySnapshotAttribute (Maybe SnapshotAttributeName)
msaAttribute = lens _msaAttribute (\ s a -> s{_msaAttribute = a});

-- | The account ID to modify for the snapshot.
msaUserIds :: Lens' ModifySnapshotAttribute [Text]
msaUserIds = lens _msaUserIds (\ s a -> s{_msaUserIds = a}) . _Default . _Coerce;

-- | A JSON representation of the snapshot attribute modification.
msaCreateVolumePermission :: Lens' ModifySnapshotAttribute (Maybe CreateVolumePermissionModifications)
msaCreateVolumePermission = lens _msaCreateVolumePermission (\ s a -> s{_msaCreateVolumePermission = a});

-- | The group to modify for the snapshot.
msaGroupNames :: Lens' ModifySnapshotAttribute [Text]
msaGroupNames = lens _msaGroupNames (\ s a -> s{_msaGroupNames = a}) . _Default . _Coerce;

-- | The type of operation to perform to the attribute.
msaOperationType :: Lens' ModifySnapshotAttribute (Maybe Text)
msaOperationType = lens _msaOperationType (\ s a -> s{_msaOperationType = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
msaDryRun :: Lens' ModifySnapshotAttribute (Maybe Bool)
msaDryRun = lens _msaDryRun (\ s a -> s{_msaDryRun = a});

-- | The ID of the snapshot.
msaSnapshotId :: Lens' ModifySnapshotAttribute Text
msaSnapshotId = lens _msaSnapshotId (\ s a -> s{_msaSnapshotId = a});

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
               "Attribute" =: _msaAttribute,
               toQuery (toQueryList "UserId" <$> _msaUserIds),
               "CreateVolumePermission" =:
                 _msaCreateVolumePermission,
               toQuery (toQueryList "GroupName" <$> _msaGroupNames),
               "OperationType" =: _msaOperationType,
               "DryRun" =: _msaDryRun,
               "SnapshotId" =: _msaSnapshotId]

-- | /See:/ 'modifySnapshotAttributeResponse' smart constructor.
data ModifySnapshotAttributeResponse =
    ModifySnapshotAttributeResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifySnapshotAttributeResponse' smart constructor.
modifySnapshotAttributeResponse :: ModifySnapshotAttributeResponse
modifySnapshotAttributeResponse = ModifySnapshotAttributeResponse'
