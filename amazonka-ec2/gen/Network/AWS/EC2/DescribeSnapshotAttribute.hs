{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.DescribeSnapshotAttribute
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

-- | Describes the specified attribute of the specified snapshot. You can
-- specify only one attribute at a time.
--
-- For more information about EBS snapshots, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSSnapshots.html Amazon EBS Snapshots>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshotAttribute.html>
module Network.AWS.EC2.DescribeSnapshotAttribute
    (
    -- * Request
      DescribeSnapshotAttribute
    -- ** Request constructor
    , describeSnapshotAttribute
    -- ** Request lenses
    , dsaDryRun
    , dsaSnapshotId
    , dsaAttribute

    -- * Response
    , DescribeSnapshotAttributeResponse
    -- ** Response constructor
    , describeSnapshotAttributeResponse
    -- ** Response lenses
    , dsarCreateVolumePermissions
    , dsarProductCodes
    , dsarSnapshotId
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.EC2.Types

-- | /See:/ 'describeSnapshotAttribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsaDryRun'
--
-- * 'dsaSnapshotId'
--
-- * 'dsaAttribute'
data DescribeSnapshotAttribute = DescribeSnapshotAttribute'{_dsaDryRun :: Maybe Bool, _dsaSnapshotId :: Text, _dsaAttribute :: SnapshotAttributeName} deriving (Eq, Read, Show)

-- | 'DescribeSnapshotAttribute' smart constructor.
describeSnapshotAttribute :: Text -> SnapshotAttributeName -> DescribeSnapshotAttribute
describeSnapshotAttribute pSnapshotId pAttribute = DescribeSnapshotAttribute'{_dsaDryRun = Nothing, _dsaSnapshotId = pSnapshotId, _dsaAttribute = pAttribute};

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dsaDryRun :: Lens' DescribeSnapshotAttribute (Maybe Bool)
dsaDryRun = lens _dsaDryRun (\ s a -> s{_dsaDryRun = a});

-- | The ID of the EBS snapshot.
dsaSnapshotId :: Lens' DescribeSnapshotAttribute Text
dsaSnapshotId = lens _dsaSnapshotId (\ s a -> s{_dsaSnapshotId = a});

-- | The snapshot attribute you would like to view.
dsaAttribute :: Lens' DescribeSnapshotAttribute SnapshotAttributeName
dsaAttribute = lens _dsaAttribute (\ s a -> s{_dsaAttribute = a});

instance AWSRequest DescribeSnapshotAttribute where
        type Sv DescribeSnapshotAttribute = EC2
        type Rs DescribeSnapshotAttribute =
             DescribeSnapshotAttributeResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeSnapshotAttributeResponse' <$>
                   parseXMLList "item" x <*> parseXMLList "item" x <*>
                     x .@? "snapshotId")

instance ToHeaders DescribeSnapshotAttribute where
        toHeaders = const mempty

instance ToPath DescribeSnapshotAttribute where
        toPath = const "/"

instance ToQuery DescribeSnapshotAttribute where
        toQuery DescribeSnapshotAttribute'{..}
          = mconcat
              ["Action" =:
                 ("DescribeSnapshotAttribute" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _dsaDryRun,
               "SnapshotId" =: _dsaSnapshotId,
               "Attribute" =: _dsaAttribute]

-- | /See:/ 'describeSnapshotAttributeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsarCreateVolumePermissions'
--
-- * 'dsarProductCodes'
--
-- * 'dsarSnapshotId'
data DescribeSnapshotAttributeResponse = DescribeSnapshotAttributeResponse'{_dsarCreateVolumePermissions :: Maybe [CreateVolumePermission], _dsarProductCodes :: Maybe [ProductCode], _dsarSnapshotId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeSnapshotAttributeResponse' smart constructor.
describeSnapshotAttributeResponse :: DescribeSnapshotAttributeResponse
describeSnapshotAttributeResponse = DescribeSnapshotAttributeResponse'{_dsarCreateVolumePermissions = Nothing, _dsarProductCodes = Nothing, _dsarSnapshotId = Nothing};

-- | A list of permissions for creating volumes from the snapshot.
dsarCreateVolumePermissions :: Lens' DescribeSnapshotAttributeResponse (Maybe [CreateVolumePermission])
dsarCreateVolumePermissions = lens _dsarCreateVolumePermissions (\ s a -> s{_dsarCreateVolumePermissions = a});

-- | A list of product codes.
dsarProductCodes :: Lens' DescribeSnapshotAttributeResponse (Maybe [ProductCode])
dsarProductCodes = lens _dsarProductCodes (\ s a -> s{_dsarProductCodes = a});

-- | The ID of the EBS snapshot.
dsarSnapshotId :: Lens' DescribeSnapshotAttributeResponse (Maybe Text)
dsarSnapshotId = lens _dsarSnapshotId (\ s a -> s{_dsarSnapshotId = a});
