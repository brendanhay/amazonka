{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ResetSnapshotAttribute
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Resets permission settings for the specified snapshot.
--
-- For more information on modifying snapshot permissions, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-modifying-snapshot-permissions.html Sharing Snapshots>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ResetSnapshotAttribute.html>
module Network.AWS.EC2.ResetSnapshotAttribute
    (
    -- * Request
      ResetSnapshotAttribute
    -- ** Request constructor
    , resetSnapshotAttribute
    -- ** Request lenses
    , rsarqDryRun
    , rsarqSnapshotId
    , rsarqAttribute

    -- * Response
    , ResetSnapshotAttributeResponse
    -- ** Response constructor
    , resetSnapshotAttributeResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'resetSnapshotAttribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsarqDryRun'
--
-- * 'rsarqSnapshotId'
--
-- * 'rsarqAttribute'
data ResetSnapshotAttribute = ResetSnapshotAttribute'
    { _rsarqDryRun     :: !(Maybe Bool)
    , _rsarqSnapshotId :: !Text
    , _rsarqAttribute  :: !ModifySnapshotAttributeName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ResetSnapshotAttribute' smart constructor.
resetSnapshotAttribute :: Text -> ModifySnapshotAttributeName -> ResetSnapshotAttribute
resetSnapshotAttribute pSnapshotId_ pAttribute_ =
    ResetSnapshotAttribute'
    { _rsarqDryRun = Nothing
    , _rsarqSnapshotId = pSnapshotId_
    , _rsarqAttribute = pAttribute_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
rsarqDryRun :: Lens' ResetSnapshotAttribute (Maybe Bool)
rsarqDryRun = lens _rsarqDryRun (\ s a -> s{_rsarqDryRun = a});

-- | The ID of the snapshot.
rsarqSnapshotId :: Lens' ResetSnapshotAttribute Text
rsarqSnapshotId = lens _rsarqSnapshotId (\ s a -> s{_rsarqSnapshotId = a});

-- | The attribute to reset. Currently, only the attribute for permission to
-- create volumes can be reset.
rsarqAttribute :: Lens' ResetSnapshotAttribute ModifySnapshotAttributeName
rsarqAttribute = lens _rsarqAttribute (\ s a -> s{_rsarqAttribute = a});

instance AWSRequest ResetSnapshotAttribute where
        type Sv ResetSnapshotAttribute = EC2
        type Rs ResetSnapshotAttribute =
             ResetSnapshotAttributeResponse
        request = post
        response
          = receiveNull ResetSnapshotAttributeResponse'

instance ToHeaders ResetSnapshotAttribute where
        toHeaders = const mempty

instance ToPath ResetSnapshotAttribute where
        toPath = const "/"

instance ToQuery ResetSnapshotAttribute where
        toQuery ResetSnapshotAttribute'{..}
          = mconcat
              ["Action" =:
                 ("ResetSnapshotAttribute" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _rsarqDryRun,
               "SnapshotId" =: _rsarqSnapshotId,
               "Attribute" =: _rsarqAttribute]

-- | /See:/ 'resetSnapshotAttributeResponse' smart constructor.
data ResetSnapshotAttributeResponse =
    ResetSnapshotAttributeResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ResetSnapshotAttributeResponse' smart constructor.
resetSnapshotAttributeResponse :: ResetSnapshotAttributeResponse
resetSnapshotAttributeResponse = ResetSnapshotAttributeResponse'
