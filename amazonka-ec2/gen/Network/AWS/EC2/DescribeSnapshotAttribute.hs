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
    , dsarqDryRun
    , dsarqSnapshotId
    , dsarqAttribute

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
-- * 'dsarqDryRun'
--
-- * 'dsarqSnapshotId'
--
-- * 'dsarqAttribute'
data DescribeSnapshotAttribute = DescribeSnapshotAttribute'
    { _dsarqDryRun     :: !(Maybe Bool)
    , _dsarqSnapshotId :: !Text
    , _dsarqAttribute  :: !SnapshotAttributeName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSnapshotAttribute' smart constructor.
describeSnapshotAttribute :: Text -> SnapshotAttributeName -> DescribeSnapshotAttribute
describeSnapshotAttribute pSnapshotId pAttribute =
    DescribeSnapshotAttribute'
    { _dsarqDryRun = Nothing
    , _dsarqSnapshotId = pSnapshotId
    , _dsarqAttribute = pAttribute
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dsarqDryRun :: Lens' DescribeSnapshotAttribute (Maybe Bool)
dsarqDryRun = lens _dsarqDryRun (\ s a -> s{_dsarqDryRun = a});

-- | The ID of the EBS snapshot.
dsarqSnapshotId :: Lens' DescribeSnapshotAttribute Text
dsarqSnapshotId = lens _dsarqSnapshotId (\ s a -> s{_dsarqSnapshotId = a});

-- | The snapshot attribute you would like to view.
dsarqAttribute :: Lens' DescribeSnapshotAttribute SnapshotAttributeName
dsarqAttribute = lens _dsarqAttribute (\ s a -> s{_dsarqAttribute = a});

instance AWSRequest DescribeSnapshotAttribute where
        type Sv DescribeSnapshotAttribute = EC2
        type Rs DescribeSnapshotAttribute =
             DescribeSnapshotAttributeResponse
        request = post
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
               "DryRun" =: _dsarqDryRun,
               "SnapshotId" =: _dsarqSnapshotId,
               "Attribute" =: _dsarqAttribute]

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
describeSnapshotAttributeResponse pStatus =
    DescribeSnapshotAttributeResponse'
    { _dsarsCreateVolumePermissions = Nothing
    , _dsarsProductCodes = Nothing
    , _dsarsSnapshotId = Nothing
    , _dsarsStatus = pStatus
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
