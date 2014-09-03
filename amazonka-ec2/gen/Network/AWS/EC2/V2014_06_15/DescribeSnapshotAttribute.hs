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
    , describeSnapshotAttribute
    -- ** Request lenses
    , dsarAttribute
    , dsarSnapshotId

    -- * Response
    , DescribeSnapshotAttributeResponse
    -- ** Response lenses
    , dsasCreateVolumePermissions
    , dsasProductCodes
    , dsasSnapshotId
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeSnapshotAttribute' request.
describeSnapshotAttribute :: SnapshotAttributeName -- ^ 'dsarAttribute'
                          -> Text -- ^ 'dsarSnapshotId'
                          -> DescribeSnapshotAttribute
describeSnapshotAttribute p1 p2 = DescribeSnapshotAttribute
    { _dsarAttribute = p1
    , _dsarSnapshotId = p2
    }

data DescribeSnapshotAttribute = DescribeSnapshotAttribute
    { _dsarAttribute :: SnapshotAttributeName
      -- ^ The snapshot attribute you would like to view.
    , _dsarSnapshotId :: Text
      -- ^ The ID of the Amazon EBS snapshot.
    } deriving (Show, Generic)

-- | The snapshot attribute you would like to view.
dsarAttribute
    :: Functor f
    => (SnapshotAttributeName
    -> f (SnapshotAttributeName))
    -> DescribeSnapshotAttribute
    -> f DescribeSnapshotAttribute
dsarAttribute f x =
    (\y -> x { _dsarAttribute = y })
       <$> f (_dsarAttribute x)
{-# INLINE dsarAttribute #-}

-- | The ID of the Amazon EBS snapshot.
dsarSnapshotId
    :: Functor f
    => (Text
    -> f (Text))
    -> DescribeSnapshotAttribute
    -> f DescribeSnapshotAttribute
dsarSnapshotId f x =
    (\y -> x { _dsarSnapshotId = y })
       <$> f (_dsarSnapshotId x)
{-# INLINE dsarSnapshotId #-}

instance ToQuery DescribeSnapshotAttribute where
    toQuery = genericQuery def

data DescribeSnapshotAttributeResponse = DescribeSnapshotAttributeResponse
    { _dsasCreateVolumePermissions :: [CreateVolumePermission]
      -- ^ A list of permissions for creating volumes from the snapshot.
    , _dsasProductCodes :: [ProductCode]
      -- ^ A list of product codes.
    , _dsasSnapshotId :: Maybe Text
      -- ^ The ID of the Amazon EBS snapshot.
    } deriving (Show, Generic)

-- | A list of permissions for creating volumes from the snapshot.
dsasCreateVolumePermissions
    :: Functor f
    => ([CreateVolumePermission]
    -> f ([CreateVolumePermission]))
    -> DescribeSnapshotAttributeResponse
    -> f DescribeSnapshotAttributeResponse
dsasCreateVolumePermissions f x =
    (\y -> x { _dsasCreateVolumePermissions = y })
       <$> f (_dsasCreateVolumePermissions x)
{-# INLINE dsasCreateVolumePermissions #-}

-- | A list of product codes.
dsasProductCodes
    :: Functor f
    => ([ProductCode]
    -> f ([ProductCode]))
    -> DescribeSnapshotAttributeResponse
    -> f DescribeSnapshotAttributeResponse
dsasProductCodes f x =
    (\y -> x { _dsasProductCodes = y })
       <$> f (_dsasProductCodes x)
{-# INLINE dsasProductCodes #-}

-- | The ID of the Amazon EBS snapshot.
dsasSnapshotId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeSnapshotAttributeResponse
    -> f DescribeSnapshotAttributeResponse
dsasSnapshotId f x =
    (\y -> x { _dsasSnapshotId = y })
       <$> f (_dsasSnapshotId x)
{-# INLINE dsasSnapshotId #-}

instance FromXML DescribeSnapshotAttributeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeSnapshotAttribute where
    type Sv DescribeSnapshotAttribute = EC2
    type Rs DescribeSnapshotAttribute = DescribeSnapshotAttributeResponse

    request = post "DescribeSnapshotAttribute"
    response _ = xmlResponse
