{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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

-- | This operation deletes a snapshot of a volume.
--
-- You can take snapshots of your gateway volumes on a scheduled or ad-hoc
-- basis. This API enables you to delete a snapshot schedule for a volume. For
-- more information, see <http://docs.aws.amazon.com/storagegateway/latest/userguide/WorkingWithSnapshots.html Working with Snapshots>. In the 'DeleteSnapshotSchedule'
-- request, you identify the volume by providing its Amazon Resource Name (ARN).
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DeleteSnapshotSchedule.html>
module Network.AWS.StorageGateway.DeleteSnapshotSchedule
    (
    -- * Request
      DeleteSnapshotSchedule
    -- ** Request constructor
    , deleteSnapshotSchedule
    -- ** Request lenses
    , dss1VolumeARN

    -- * Response
    , DeleteSnapshotScheduleResponse
    -- ** Response constructor
    , deleteSnapshotScheduleResponse
    -- ** Response lenses
    , dssr1VolumeARN
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

newtype DeleteSnapshotSchedule = DeleteSnapshotSchedule
    { _dss1VolumeARN :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'DeleteSnapshotSchedule' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dss1VolumeARN' @::@ 'Text'
--
deleteSnapshotSchedule :: Text -- ^ 'dss1VolumeARN'
                       -> DeleteSnapshotSchedule
deleteSnapshotSchedule p1 = DeleteSnapshotSchedule
    { _dss1VolumeARN = p1
    }

dss1VolumeARN :: Lens' DeleteSnapshotSchedule Text
dss1VolumeARN = lens _dss1VolumeARN (\s a -> s { _dss1VolumeARN = a })

newtype DeleteSnapshotScheduleResponse = DeleteSnapshotScheduleResponse
    { _dssr1VolumeARN :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

-- | 'DeleteSnapshotScheduleResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dssr1VolumeARN' @::@ 'Maybe' 'Text'
--
deleteSnapshotScheduleResponse :: DeleteSnapshotScheduleResponse
deleteSnapshotScheduleResponse = DeleteSnapshotScheduleResponse
    { _dssr1VolumeARN = Nothing
    }

dssr1VolumeARN :: Lens' DeleteSnapshotScheduleResponse (Maybe Text)
dssr1VolumeARN = lens _dssr1VolumeARN (\s a -> s { _dssr1VolumeARN = a })

instance ToPath DeleteSnapshotSchedule where
    toPath = const "/"

instance ToQuery DeleteSnapshotSchedule where
    toQuery = const mempty

instance ToHeaders DeleteSnapshotSchedule

instance ToJSON DeleteSnapshotSchedule where
    toJSON DeleteSnapshotSchedule{..} = object
        [ "VolumeARN" .= _dss1VolumeARN
        ]

instance AWSRequest DeleteSnapshotSchedule where
    type Sv DeleteSnapshotSchedule = StorageGateway
    type Rs DeleteSnapshotSchedule = DeleteSnapshotScheduleResponse

    request  = post "DeleteSnapshotSchedule"
    response = jsonResponse

instance FromJSON DeleteSnapshotScheduleResponse where
    parseJSON = withObject "DeleteSnapshotScheduleResponse" $ \o -> DeleteSnapshotScheduleResponse
        <$> o .:? "VolumeARN"
