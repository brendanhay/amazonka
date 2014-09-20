{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeSnapshotAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the specified attribute of the specified snapshot. You can
-- specify only one attribute at a time. For more information about Amazon EBS
-- snapshots, see Amazon EBS Snapshots in the Amazon Elastic Compute Cloud
-- User Guide. Example This example describes permissions for a snapshot with
-- the ID of snap-1a2b3c4d.
-- https://ec2.amazonaws.com/?Action=DescribeSnapshotAttribute
-- &amp;SnapshotId=snap-1a2b3c4d &amp;Attribute=createVolumePermission
-- &amp;AUTHPARAMS 59dbff89-35bd-4eac-99ed-be587EXAMPLE snap-1a2b3c4d all.
module Network.AWS.EC2.DescribeSnapshotAttribute
    (
    -- * Request
      DescribeSnapshotAttribute
    -- ** Request constructor
    , describeSnapshotAttribute
    -- ** Request lenses
    , dsaSnapshotId
    , dsaAttribute

    -- * Response
    , DescribeSnapshotAttributeResponse
    -- ** Response constructor
    , describeSnapshotAttributeResponse
    -- ** Response lenses
    , dsarSnapshotId
    , dsarCreateVolumePermissions
    , dsarProductCodes
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data DescribeSnapshotAttribute = DescribeSnapshotAttribute
    { _dsaSnapshotId :: Text
    , _dsaAttribute :: SnapshotAttributeName
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeSnapshotAttribute' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SnapshotId ::@ @Text@
--
-- * @Attribute ::@ @SnapshotAttributeName@
--
describeSnapshotAttribute :: Text -- ^ 'dsaSnapshotId'
                          -> SnapshotAttributeName -- ^ 'dsaAttribute'
                          -> DescribeSnapshotAttribute
describeSnapshotAttribute p1 p2 = DescribeSnapshotAttribute
    { _dsaSnapshotId = p1
    , _dsaAttribute = p2
    }

-- | The ID of the Amazon EBS snapshot.
dsaSnapshotId :: Lens' DescribeSnapshotAttribute Text
dsaSnapshotId = lens _dsaSnapshotId (\s a -> s { _dsaSnapshotId = a })

-- | The snapshot attribute you would like to view.
dsaAttribute :: Lens' DescribeSnapshotAttribute SnapshotAttributeName
dsaAttribute = lens _dsaAttribute (\s a -> s { _dsaAttribute = a })

instance ToQuery DescribeSnapshotAttribute where
    toQuery = genericQuery def

data DescribeSnapshotAttributeResponse = DescribeSnapshotAttributeResponse
    { _dsarSnapshotId :: Maybe Text
    , _dsarCreateVolumePermissions :: [CreateVolumePermission]
    , _dsarProductCodes :: [ProductCode]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeSnapshotAttributeResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SnapshotId ::@ @Maybe Text@
--
-- * @CreateVolumePermissions ::@ @[CreateVolumePermission]@
--
-- * @ProductCodes ::@ @[ProductCode]@
--
describeSnapshotAttributeResponse :: DescribeSnapshotAttributeResponse
describeSnapshotAttributeResponse = DescribeSnapshotAttributeResponse
    { _dsarSnapshotId = Nothing
    , _dsarCreateVolumePermissions = mempty
    , _dsarProductCodes = mempty
    }

-- | The ID of the Amazon EBS snapshot.
dsarSnapshotId :: Lens' DescribeSnapshotAttributeResponse (Maybe Text)
dsarSnapshotId = lens _dsarSnapshotId (\s a -> s { _dsarSnapshotId = a })

-- | A list of permissions for creating volumes from the snapshot.
dsarCreateVolumePermissions :: Lens' DescribeSnapshotAttributeResponse [CreateVolumePermission]
dsarCreateVolumePermissions =
    lens _dsarCreateVolumePermissions
         (\s a -> s { _dsarCreateVolumePermissions = a })

-- | A list of product codes.
dsarProductCodes :: Lens' DescribeSnapshotAttributeResponse [ProductCode]
dsarProductCodes =
    lens _dsarProductCodes (\s a -> s { _dsarProductCodes = a })

instance FromXML DescribeSnapshotAttributeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeSnapshotAttribute where
    type Sv DescribeSnapshotAttribute = EC2
    type Rs DescribeSnapshotAttribute = DescribeSnapshotAttributeResponse

    request = post "DescribeSnapshotAttribute"
    response _ = xmlResponse
