{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.DeleteSnapshotSchedule
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation deletes a snapshot of a volume. You can take snapshots of
-- your gateway volumes on a scheduled or ad-hoc basis. This API enables you
-- to delete a snapshot schedule for a volume. For more information, see
-- Working with Snapshots. In the DeleteSnapshotSchedule request, you identify
-- the volume by providing its Amazon Resource Name (ARN). To list or delete a
-- snapshot, you must use the Amazon EC2 API. in Amazon Elastic Compute Cloud
-- API Reference. Example Request The following example... POST / HTTP/1.1
-- Host: storagegateway.us-east-1.amazonaws.com Content-Type:
-- application/x-amz-json-1.1 Authorization: AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20120425/us-east-1/storagegateway/aws4_request,
-- SignedHeaders=content-type;host;x-amz-date;x-amz-target,
-- Signature=9cd5a3584d1d67d57e61f120f35102d6b3649066abdd4bf4bbcf05bd9f2f8fe2
-- x-amz-date: 20120912T120000Z x-amz-target:
-- StorageGateway_20120630.DeleteSnapshotSchedule { "VolumeARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-1122AABB"
-- } HTTP/1.1 200 OK x-amzn-RequestId:
-- gur28r2rqlgb8vvs0mq17hlgij1q8glle1qeu3kpgg6f0kstauu0 Date: Wed, 12 Sep 2012
-- 12:00:02 GMT Content-Type: application/x-amz-json-1.1 Content-length: 137 {
-- "VolumeARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-1122AABB"
-- }.
module Network.AWS.StorageGateway.DeleteSnapshotSchedule
    (
    -- * Request
      DeleteSnapshotSchedule
    -- ** Request constructor
    , deleteSnapshotSchedule
    -- ** Request lenses
    , dssVolumeARN

    -- * Response
    , DeleteSnapshotScheduleResponse
    -- ** Response constructor
    , deleteSnapshotScheduleResponse
    -- ** Response lenses
    , dssrVolumeARN
    ) where

import Network.AWS.StorageGateway.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

newtype DeleteSnapshotSchedule = DeleteSnapshotSchedule
    { _dssVolumeARN :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteSnapshotSchedule' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VolumeARN ::@ @Text@
--
deleteSnapshotSchedule :: Text -- ^ 'dssVolumeARN'
                       -> DeleteSnapshotSchedule
deleteSnapshotSchedule p1 = DeleteSnapshotSchedule
    { _dssVolumeARN = p1
    }

dssVolumeARN :: Lens' DeleteSnapshotSchedule Text
dssVolumeARN = lens _dssVolumeARN (\s a -> s { _dssVolumeARN = a })

instance ToPath DeleteSnapshotSchedule

instance ToQuery DeleteSnapshotSchedule

instance ToHeaders DeleteSnapshotSchedule

instance ToJSON DeleteSnapshotSchedule

newtype DeleteSnapshotScheduleResponse = DeleteSnapshotScheduleResponse
    { _dssrVolumeARN :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteSnapshotScheduleResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VolumeARN ::@ @Maybe Text@
--
deleteSnapshotScheduleResponse :: DeleteSnapshotScheduleResponse
deleteSnapshotScheduleResponse = DeleteSnapshotScheduleResponse
    { _dssrVolumeARN = Nothing
    }

dssrVolumeARN :: Lens' DeleteSnapshotScheduleResponse (Maybe Text)
dssrVolumeARN = lens _dssrVolumeARN (\s a -> s { _dssrVolumeARN = a })

instance FromJSON DeleteSnapshotScheduleResponse

instance AWSRequest DeleteSnapshotSchedule where
    type Sv DeleteSnapshotSchedule = StorageGateway
    type Rs DeleteSnapshotSchedule = DeleteSnapshotScheduleResponse

    request = get
    response _ = jsonResponse
