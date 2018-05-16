{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifySnapshotAttribute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or removes permission settings for the specified snapshot. You may add or remove specified AWS account IDs from a snapshot's list of create volume permissions, but you cannot do both in a single API call. If you need to both add and remove account IDs for a snapshot, you must use multiple API calls.
--
--
-- For more information on modifying snapshot permissions, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-modifying-snapshot-permissions.html Sharing Snapshots> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.ModifySnapshotAttribute
    (
    -- * Creating a Request
      modifySnapshotAttribute
    , ModifySnapshotAttribute
    -- * Request Lenses
    , msaAttribute
    , msaCreateVolumePermission
    , msaUserIds
    , msaGroupNames
    , msaOperationType
    , msaDryRun
    , msaSnapshotId

    -- * Destructuring the Response
    , modifySnapshotAttributeResponse
    , ModifySnapshotAttributeResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for ModifySnapshotAttribute.
--
--
--
-- /See:/ 'modifySnapshotAttribute' smart constructor.
data ModifySnapshotAttribute = ModifySnapshotAttribute'
  { _msaAttribute              :: !(Maybe SnapshotAttributeName)
  , _msaCreateVolumePermission :: !(Maybe CreateVolumePermissionModifications)
  , _msaUserIds                :: !(Maybe [Text])
  , _msaGroupNames             :: !(Maybe [Text])
  , _msaOperationType          :: !(Maybe OperationType)
  , _msaDryRun                 :: !(Maybe Bool)
  , _msaSnapshotId             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifySnapshotAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msaAttribute' - The snapshot attribute to modify.
--
-- * 'msaCreateVolumePermission' - A JSON representation of the snapshot attribute modification.
--
-- * 'msaUserIds' - The account ID to modify for the snapshot.
--
-- * 'msaGroupNames' - The group to modify for the snapshot.
--
-- * 'msaOperationType' - The type of operation to perform to the attribute.
--
-- * 'msaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'msaSnapshotId' - The ID of the snapshot.
modifySnapshotAttribute
    :: Text -- ^ 'msaSnapshotId'
    -> ModifySnapshotAttribute
modifySnapshotAttribute pSnapshotId_ =
  ModifySnapshotAttribute'
    { _msaAttribute = Nothing
    , _msaCreateVolumePermission = Nothing
    , _msaUserIds = Nothing
    , _msaGroupNames = Nothing
    , _msaOperationType = Nothing
    , _msaDryRun = Nothing
    , _msaSnapshotId = pSnapshotId_
    }


-- | The snapshot attribute to modify.
msaAttribute :: Lens' ModifySnapshotAttribute (Maybe SnapshotAttributeName)
msaAttribute = lens _msaAttribute (\ s a -> s{_msaAttribute = a})

-- | A JSON representation of the snapshot attribute modification.
msaCreateVolumePermission :: Lens' ModifySnapshotAttribute (Maybe CreateVolumePermissionModifications)
msaCreateVolumePermission = lens _msaCreateVolumePermission (\ s a -> s{_msaCreateVolumePermission = a})

-- | The account ID to modify for the snapshot.
msaUserIds :: Lens' ModifySnapshotAttribute [Text]
msaUserIds = lens _msaUserIds (\ s a -> s{_msaUserIds = a}) . _Default . _Coerce

-- | The group to modify for the snapshot.
msaGroupNames :: Lens' ModifySnapshotAttribute [Text]
msaGroupNames = lens _msaGroupNames (\ s a -> s{_msaGroupNames = a}) . _Default . _Coerce

-- | The type of operation to perform to the attribute.
msaOperationType :: Lens' ModifySnapshotAttribute (Maybe OperationType)
msaOperationType = lens _msaOperationType (\ s a -> s{_msaOperationType = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
msaDryRun :: Lens' ModifySnapshotAttribute (Maybe Bool)
msaDryRun = lens _msaDryRun (\ s a -> s{_msaDryRun = a})

-- | The ID of the snapshot.
msaSnapshotId :: Lens' ModifySnapshotAttribute Text
msaSnapshotId = lens _msaSnapshotId (\ s a -> s{_msaSnapshotId = a})

instance AWSRequest ModifySnapshotAttribute where
        type Rs ModifySnapshotAttribute =
             ModifySnapshotAttributeResponse
        request = postQuery ec2
        response
          = receiveNull ModifySnapshotAttributeResponse'

instance Hashable ModifySnapshotAttribute where

instance NFData ModifySnapshotAttribute where

instance ToHeaders ModifySnapshotAttribute where
        toHeaders = const mempty

instance ToPath ModifySnapshotAttribute where
        toPath = const "/"

instance ToQuery ModifySnapshotAttribute where
        toQuery ModifySnapshotAttribute'{..}
          = mconcat
              ["Action" =:
                 ("ModifySnapshotAttribute" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "Attribute" =: _msaAttribute,
               "CreateVolumePermission" =:
                 _msaCreateVolumePermission,
               toQuery (toQueryList "UserId" <$> _msaUserIds),
               toQuery (toQueryList "UserGroup" <$> _msaGroupNames),
               "OperationType" =: _msaOperationType,
               "DryRun" =: _msaDryRun,
               "SnapshotId" =: _msaSnapshotId]

-- | /See:/ 'modifySnapshotAttributeResponse' smart constructor.
data ModifySnapshotAttributeResponse =
  ModifySnapshotAttributeResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifySnapshotAttributeResponse' with the minimum fields required to make a request.
--
modifySnapshotAttributeResponse
    :: ModifySnapshotAttributeResponse
modifySnapshotAttributeResponse = ModifySnapshotAttributeResponse'


instance NFData ModifySnapshotAttributeResponse where
