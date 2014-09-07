{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeSnapshotAttribute
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
module Network.AWS.EC2.V2014_06_15.DescribeSnapshotAttribute
    (
    -- * Request
      DescribeSnapshotAttribute
    -- ** Request constructor
    , mkDescribeSnapshotAttribute
    -- ** Request lenses
    , dsaSnapshotId
    , dsaAttribute

    -- * Response
    , DescribeSnapshotAttributeResponse
    -- ** Response lenses
    , dsarsSnapshotId
    , dsarsCreateVolumePermissions
    , dsarsProductCodes
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | 
data DescribeSnapshotAttribute = DescribeSnapshotAttribute
    { _dsaSnapshotId :: Text
    , _dsaAttribute :: SnapshotAttributeName
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeSnapshotAttribute' request.
mkDescribeSnapshotAttribute :: Text -- ^ 'dsaSnapshotId'
                            -> SnapshotAttributeName -- ^ 'dsaAttribute'
                            -> DescribeSnapshotAttribute
mkDescribeSnapshotAttribute p1 p2 = DescribeSnapshotAttribute
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

-- | 
data DescribeSnapshotAttributeResponse = DescribeSnapshotAttributeResponse
    { _dsarsSnapshotId :: Maybe Text
    , _dsarsCreateVolumePermissions :: [CreateVolumePermission]
    , _dsarsProductCodes :: [ProductCode]
    } deriving (Show, Generic)

-- | The ID of the Amazon EBS snapshot.
dsarsSnapshotId :: Lens' DescribeSnapshotAttributeResponse (Maybe Text)
dsarsSnapshotId = lens _dsarsSnapshotId (\s a -> s { _dsarsSnapshotId = a })

-- | A list of permissions for creating volumes from the snapshot.
dsarsCreateVolumePermissions :: Lens' DescribeSnapshotAttributeResponse [CreateVolumePermission]
dsarsCreateVolumePermissions =
    lens _dsarsCreateVolumePermissions
         (\s a -> s { _dsarsCreateVolumePermissions = a })

-- | A list of product codes.
dsarsProductCodes :: Lens' DescribeSnapshotAttributeResponse [ProductCode]
dsarsProductCodes =
    lens _dsarsProductCodes (\s a -> s { _dsarsProductCodes = a })

instance FromXML DescribeSnapshotAttributeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeSnapshotAttribute where
    type Sv DescribeSnapshotAttribute = EC2
    type Rs DescribeSnapshotAttribute = DescribeSnapshotAttributeResponse

    request = post "DescribeSnapshotAttribute"
    response _ = xmlResponse
