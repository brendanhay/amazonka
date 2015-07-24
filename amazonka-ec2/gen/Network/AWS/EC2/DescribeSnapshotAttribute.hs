{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeSnapshotAttribute
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified attribute of the specified snapshot. You can
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
    , dsarsCreateVolumePermissions
    , dsarsProductCodes
    , dsarsSnapshotId
    , dsarsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeSnapshotAttribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsaDryRun'
--
-- * 'dsaSnapshotId'
--
-- * 'dsaAttribute'
data DescribeSnapshotAttribute = DescribeSnapshotAttribute'
    { _dsaDryRun     :: !(Maybe Bool)
    , _dsaSnapshotId :: !Text
    , _dsaAttribute  :: !SnapshotAttributeName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSnapshotAttribute' smart constructor.
describeSnapshotAttribute :: Text -> SnapshotAttributeName -> DescribeSnapshotAttribute
describeSnapshotAttribute pSnapshotId_ pAttribute_ =
    DescribeSnapshotAttribute'
    { _dsaDryRun = Nothing
    , _dsaSnapshotId = pSnapshotId_
    , _dsaAttribute = pAttribute_
    }

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
        request = post "DescribeSnapshotAttribute"
        response
          = receiveXML
              (\ s h x ->
                 DescribeSnapshotAttributeResponse' <$>
                   (x .@? "createVolumePermission" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*>
                     (x .@? "productCodes" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (x .@? "snapshotId")
                     <*> (pure (fromEnum s)))

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
-- * 'dsarsCreateVolumePermissions'
--
-- * 'dsarsProductCodes'
--
-- * 'dsarsSnapshotId'
--
-- * 'dsarsStatus'
data DescribeSnapshotAttributeResponse = DescribeSnapshotAttributeResponse'
    { _dsarsCreateVolumePermissions :: !(Maybe [CreateVolumePermission])
    , _dsarsProductCodes            :: !(Maybe [ProductCode])
    , _dsarsSnapshotId              :: !(Maybe Text)
    , _dsarsStatus                  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSnapshotAttributeResponse' smart constructor.
describeSnapshotAttributeResponse :: Int -> DescribeSnapshotAttributeResponse
describeSnapshotAttributeResponse pStatus_ =
    DescribeSnapshotAttributeResponse'
    { _dsarsCreateVolumePermissions = Nothing
    , _dsarsProductCodes = Nothing
    , _dsarsSnapshotId = Nothing
    , _dsarsStatus = pStatus_
    }

-- | A list of permissions for creating volumes from the snapshot.
dsarsCreateVolumePermissions :: Lens' DescribeSnapshotAttributeResponse [CreateVolumePermission]
dsarsCreateVolumePermissions = lens _dsarsCreateVolumePermissions (\ s a -> s{_dsarsCreateVolumePermissions = a}) . _Default;

-- | A list of product codes.
dsarsProductCodes :: Lens' DescribeSnapshotAttributeResponse [ProductCode]
dsarsProductCodes = lens _dsarsProductCodes (\ s a -> s{_dsarsProductCodes = a}) . _Default;

-- | The ID of the EBS snapshot.
dsarsSnapshotId :: Lens' DescribeSnapshotAttributeResponse (Maybe Text)
dsarsSnapshotId = lens _dsarsSnapshotId (\ s a -> s{_dsarsSnapshotId = a});

-- | FIXME: Undocumented member.
dsarsStatus :: Lens' DescribeSnapshotAttributeResponse Int
dsarsStatus = lens _dsarsStatus (\ s a -> s{_dsarsStatus = a});
