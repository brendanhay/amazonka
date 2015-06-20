{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.ResetSnapshotAttribute
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

-- | Resets permission settings for the specified snapshot.
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
    , rsaDryRun
    , rsaSnapshotId
    , rsaAttribute

    -- * Response
    , ResetSnapshotAttributeResponse
    -- ** Response constructor
    , resetSnapshotAttributeResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'resetSnapshotAttribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsaDryRun'
--
-- * 'rsaSnapshotId'
--
-- * 'rsaAttribute'
data ResetSnapshotAttribute = ResetSnapshotAttribute'{_rsaDryRun :: Maybe Bool, _rsaSnapshotId :: Text, _rsaAttribute :: SnapshotAttributeName} deriving (Eq, Read, Show)

-- | 'ResetSnapshotAttribute' smart constructor.
resetSnapshotAttribute :: Text -> SnapshotAttributeName -> ResetSnapshotAttribute
resetSnapshotAttribute pSnapshotId pAttribute = ResetSnapshotAttribute'{_rsaDryRun = Nothing, _rsaSnapshotId = pSnapshotId, _rsaAttribute = pAttribute};

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
rsaDryRun :: Lens' ResetSnapshotAttribute (Maybe Bool)
rsaDryRun = lens _rsaDryRun (\ s a -> s{_rsaDryRun = a});

-- | The ID of the snapshot.
rsaSnapshotId :: Lens' ResetSnapshotAttribute Text
rsaSnapshotId = lens _rsaSnapshotId (\ s a -> s{_rsaSnapshotId = a});

-- | The attribute to reset (currently only the attribute for permission to
-- create volumes can be reset).
rsaAttribute :: Lens' ResetSnapshotAttribute SnapshotAttributeName
rsaAttribute = lens _rsaAttribute (\ s a -> s{_rsaAttribute = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

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
               "DryRun" =: _rsaDryRun,
               "SnapshotId" =: _rsaSnapshotId,
               "Attribute" =: _rsaAttribute]

-- | /See:/ 'resetSnapshotAttributeResponse' smart constructor.
data ResetSnapshotAttributeResponse = ResetSnapshotAttributeResponse' deriving (Eq, Read, Show)

-- | 'ResetSnapshotAttributeResponse' smart constructor.
resetSnapshotAttributeResponse :: ResetSnapshotAttributeResponse
resetSnapshotAttributeResponse = ResetSnapshotAttributeResponse';
