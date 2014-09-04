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
    , mkDescribeSnapshotAttributeRequest
    -- ** Request lenses
    , dsarSnapshotId
    , dsarAttribute

    -- * Response
    , DescribeSnapshotAttributeResponse
    -- ** Response lenses
    , dsasSnapshotId
    , dsasCreateVolumePermissions
    , dsasProductCodes
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeSnapshotAttribute' request.
mkDescribeSnapshotAttributeRequest :: Text -- ^ 'dsarSnapshotId'
                                   -> SnapshotAttributeName -- ^ 'dsarAttribute'
                                   -> DescribeSnapshotAttribute
mkDescribeSnapshotAttributeRequest p1 p2 = DescribeSnapshotAttribute
    { _dsarSnapshotId = p1
    , _dsarAttribute = p2
    }
{-# INLINE mkDescribeSnapshotAttributeRequest #-}

data DescribeSnapshotAttribute = DescribeSnapshotAttribute
    { _dsarSnapshotId :: Text
      -- ^ The ID of the Amazon EBS snapshot.
    , _dsarAttribute :: SnapshotAttributeName
      -- ^ The snapshot attribute you would like to view.
    } deriving (Show, Generic)

-- | The ID of the Amazon EBS snapshot.
dsarSnapshotId :: Lens' DescribeSnapshotAttribute (Text)
dsarSnapshotId = lens _dsarSnapshotId (\s a -> s { _dsarSnapshotId = a })
{-# INLINE dsarSnapshotId #-}

-- | The snapshot attribute you would like to view.
dsarAttribute :: Lens' DescribeSnapshotAttribute (SnapshotAttributeName)
dsarAttribute = lens _dsarAttribute (\s a -> s { _dsarAttribute = a })
{-# INLINE dsarAttribute #-}

instance ToQuery DescribeSnapshotAttribute where
    toQuery = genericQuery def

data DescribeSnapshotAttributeResponse = DescribeSnapshotAttributeResponse
    { _dsasSnapshotId :: Maybe Text
      -- ^ The ID of the Amazon EBS snapshot.
    , _dsasCreateVolumePermissions :: [CreateVolumePermission]
      -- ^ A list of permissions for creating volumes from the snapshot.
    , _dsasProductCodes :: [ProductCode]
      -- ^ A list of product codes.
    } deriving (Show, Generic)

-- | The ID of the Amazon EBS snapshot.
dsasSnapshotId :: Lens' DescribeSnapshotAttributeResponse (Maybe Text)
dsasSnapshotId = lens _dsasSnapshotId (\s a -> s { _dsasSnapshotId = a })
{-# INLINE dsasSnapshotId #-}

-- | A list of permissions for creating volumes from the snapshot.
dsasCreateVolumePermissions :: Lens' DescribeSnapshotAttributeResponse ([CreateVolumePermission])
dsasCreateVolumePermissions = lens _dsasCreateVolumePermissions (\s a -> s { _dsasCreateVolumePermissions = a })
{-# INLINE dsasCreateVolumePermissions #-}

-- | A list of product codes.
dsasProductCodes :: Lens' DescribeSnapshotAttributeResponse ([ProductCode])
dsasProductCodes = lens _dsasProductCodes (\s a -> s { _dsasProductCodes = a })
{-# INLINE dsasProductCodes #-}

instance FromXML DescribeSnapshotAttributeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeSnapshotAttribute where
    type Sv DescribeSnapshotAttribute = EC2
    type Rs DescribeSnapshotAttribute = DescribeSnapshotAttributeResponse

    request = post "DescribeSnapshotAttribute"
    response _ = xmlResponse
