{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.CopySnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Copies a point-in-time snapshot of an Amazon EBS volume and stores it in
-- Amazon S3. You can copy the snapshot within the same region or from one
-- region to another. You can use the snapshot to create Amazon EBS volumes or
-- Amazon Machine Images (AMIs). The snapshot is copied to the regional
-- endpoint that you send the HTTP request to. Copies of encrypted Amazon EBS
-- snapshots remain encrypted. Copies of unencrypted snapshots remain
-- unencrypted. For more information, see Copying an Amazon EBS Snapshot in
-- the Amazon Elastic Compute Cloud User Guide. Example This example request
-- copies the snapshot in the us-west-1 region with the ID snap-1a2b3c4d.
-- https://ec2.amazonaws.com/?Action=CopySnapshot &amp;SourceRegion=us-west-1
-- &amp;SourceSnapshotId=snap-1a2b3c4d &amp;Description=My_snapshot
-- &amp;AUTHPARAMS &lt;CopySnapshotResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-05-01/"&gt;
-- &lt;requestId&gt;60bc441d-fa2c-494d-b155-5d6a3EXAMPLE&lt;/requestId&gt;
-- &lt;snapshotId&gt;snap-2a2b3c4d&lt;/snapshotId&gt;
-- &lt;/CopySnapshotResponse&gt;.
module Network.AWS.EC2.V2014_06_15.CopySnapshot
    (
    -- * Request
      CopySnapshot
    -- ** Request constructor
    , copySnapshot
    -- ** Request lenses
    , csrSourceRegion
    , csrSourceSnapshotId
    , csrDescription
    , csrDestinationRegion
    , csrPresignedUrl

    -- * Response
    , CopySnapshotResponse
    -- ** Response lenses
    , cssSnapshotId
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CopySnapshot' request.
copySnapshot :: Text -- ^ 'csrSourceRegion'
             -> Text -- ^ 'csrSourceSnapshotId'
             -> CopySnapshot
copySnapshot p1 p2 = CopySnapshot
    { _csrSourceRegion = p1
    , _csrSourceSnapshotId = p2
    , _csrDescription = Nothing
    , _csrDestinationRegion = Nothing
    , _csrPresignedUrl = Nothing
    }

data CopySnapshot = CopySnapshot
    { _csrSourceRegion :: Text
      -- ^ The ID of the region that contains the snapshot to be copied.
    , _csrSourceSnapshotId :: Text
      -- ^ The ID of the Amazon EBS snapshot to copy.
    , _csrDescription :: Maybe Text
      -- ^ A description for the new Amazon EBS snapshot.
    , _csrDestinationRegion :: Maybe Text
      -- ^ The destination region of the snapshot copy operation. This
      -- parameter is required in the PresignedUrl.
    , _csrPresignedUrl :: Maybe Text
      -- ^ The pre-signed URL that facilitates copying an encrypted
      -- snapshot. This parameter is only required when copying an
      -- encrypted snapshot with the Amazon EC2 Query API; it is available
      -- as an optional parameter in all other cases. The PresignedUrl
      -- should use the snapshot source endpoint, the CopySnapshot action,
      -- and include the SourceRegion, SourceSnapshotId, and
      -- DestinationRegion parameters. The PresignedUrl must be signed
      -- using AWS Signature Version 4. Because Amazon EBS snapshots are
      -- stored in Amazon S3, the signing algorithm for this parameter
      -- uses the same logic that is described in Authenticating Requests
      -- by Using Query Parameters (AWS Signature Version 4) in the Amazon
      -- Simple Storage Service API Reference. An invalid or improperly
      -- signed PresignedUrl will cause the copy operation to fail
      -- asynchronously, and the snapshot will move to an error state.
    } deriving (Show, Generic)

-- | The ID of the region that contains the snapshot to be copied.
csrSourceRegion
    :: Functor f
    => (Text
    -> f (Text))
    -> CopySnapshot
    -> f CopySnapshot
csrSourceRegion f x =
    (\y -> x { _csrSourceRegion = y })
       <$> f (_csrSourceRegion x)
{-# INLINE csrSourceRegion #-}

-- | The ID of the Amazon EBS snapshot to copy.
csrSourceSnapshotId
    :: Functor f
    => (Text
    -> f (Text))
    -> CopySnapshot
    -> f CopySnapshot
csrSourceSnapshotId f x =
    (\y -> x { _csrSourceSnapshotId = y })
       <$> f (_csrSourceSnapshotId x)
{-# INLINE csrSourceSnapshotId #-}

-- | A description for the new Amazon EBS snapshot.
csrDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CopySnapshot
    -> f CopySnapshot
csrDescription f x =
    (\y -> x { _csrDescription = y })
       <$> f (_csrDescription x)
{-# INLINE csrDescription #-}

-- | The destination region of the snapshot copy operation. This parameter is
-- required in the PresignedUrl.
csrDestinationRegion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CopySnapshot
    -> f CopySnapshot
csrDestinationRegion f x =
    (\y -> x { _csrDestinationRegion = y })
       <$> f (_csrDestinationRegion x)
{-# INLINE csrDestinationRegion #-}

-- | The pre-signed URL that facilitates copying an encrypted snapshot. This
-- parameter is only required when copying an encrypted snapshot with the
-- Amazon EC2 Query API; it is available as an optional parameter in all other
-- cases. The PresignedUrl should use the snapshot source endpoint, the
-- CopySnapshot action, and include the SourceRegion, SourceSnapshotId, and
-- DestinationRegion parameters. The PresignedUrl must be signed using AWS
-- Signature Version 4. Because Amazon EBS snapshots are stored in Amazon S3,
-- the signing algorithm for this parameter uses the same logic that is
-- described in Authenticating Requests by Using Query Parameters (AWS
-- Signature Version 4) in the Amazon Simple Storage Service API Reference. An
-- invalid or improperly signed PresignedUrl will cause the copy operation to
-- fail asynchronously, and the snapshot will move to an error state.
csrPresignedUrl
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CopySnapshot
    -> f CopySnapshot
csrPresignedUrl f x =
    (\y -> x { _csrPresignedUrl = y })
       <$> f (_csrPresignedUrl x)
{-# INLINE csrPresignedUrl #-}

instance ToQuery CopySnapshot where
    toQuery = genericQuery def

data CopySnapshotResponse = CopySnapshotResponse
    { _cssSnapshotId :: Maybe Text
      -- ^ The ID of the new snapshot.
    } deriving (Show, Generic)

-- | The ID of the new snapshot.
cssSnapshotId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CopySnapshotResponse
    -> f CopySnapshotResponse
cssSnapshotId f x =
    (\y -> x { _cssSnapshotId = y })
       <$> f (_cssSnapshotId x)
{-# INLINE cssSnapshotId #-}

instance FromXML CopySnapshotResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CopySnapshot where
    type Sv CopySnapshot = EC2
    type Rs CopySnapshot = CopySnapshotResponse

    request = post "CopySnapshot"
    response _ = xmlResponse
