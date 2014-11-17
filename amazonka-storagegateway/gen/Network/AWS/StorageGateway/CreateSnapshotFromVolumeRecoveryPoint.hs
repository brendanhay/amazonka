{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.CreateSnapshotFromVolumeRecoveryPoint
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation initiates a snapshot of a gateway from a volume recovery
-- point. This operation is supported only for the gateway-cached volume
-- architecture (see ). A volume recovery point is a point in time at which
-- all data of the volume is consistent and from which you can create a
-- snapshot. To get a list of volume recovery point for gateway-cached
-- volumes, use ListVolumeRecoveryPoints. In the
-- CreateSnapshotFromVolumeRecoveryPoint request, you identify the volume by
-- providing its Amazon Resource Name (ARN). You must also provide a
-- description for the snapshot. When AWS Storage Gateway takes a snapshot of
-- the specified volume, the snapshot and its description appear in the AWS
-- Storage Gateway console. In response, AWS Storage Gateway returns you a
-- snapshot ID. You can use this snapshot ID to check the snapshot progress or
-- later use it when you want to create a volume from a snapshot.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_CreateSnapshotFromVolumeRecoveryPoint.html>
module Network.AWS.StorageGateway.CreateSnapshotFromVolumeRecoveryPoint
    (
    -- * Request
      CreateSnapshotFromVolumeRecoveryPoint
    -- ** Request constructor
    , createSnapshotFromVolumeRecoveryPoint
    -- ** Request lenses
    , csfvrpSnapshotDescription
    , csfvrpVolumeARN

    -- * Response
    , CreateSnapshotFromVolumeRecoveryPointResponse
    -- ** Response constructor
    , createSnapshotFromVolumeRecoveryPointResponse
    -- ** Response lenses
    , csfvrprSnapshotId
    , csfvrprVolumeARN
    , csfvrprVolumeRecoveryPointTime
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

data CreateSnapshotFromVolumeRecoveryPoint = CreateSnapshotFromVolumeRecoveryPoint
    { _csfvrpSnapshotDescription :: Text
    , _csfvrpVolumeARN           :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateSnapshotFromVolumeRecoveryPoint' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csfvrpSnapshotDescription' @::@ 'Text'
--
-- * 'csfvrpVolumeARN' @::@ 'Text'
--
createSnapshotFromVolumeRecoveryPoint :: Text -- ^ 'csfvrpVolumeARN'
                                      -> Text -- ^ 'csfvrpSnapshotDescription'
                                      -> CreateSnapshotFromVolumeRecoveryPoint
createSnapshotFromVolumeRecoveryPoint p1 p2 = CreateSnapshotFromVolumeRecoveryPoint
    { _csfvrpVolumeARN           = p1
    , _csfvrpSnapshotDescription = p2
    }

csfvrpSnapshotDescription :: Lens' CreateSnapshotFromVolumeRecoveryPoint Text
csfvrpSnapshotDescription =
    lens _csfvrpSnapshotDescription
        (\s a -> s { _csfvrpSnapshotDescription = a })

csfvrpVolumeARN :: Lens' CreateSnapshotFromVolumeRecoveryPoint Text
csfvrpVolumeARN = lens _csfvrpVolumeARN (\s a -> s { _csfvrpVolumeARN = a })

data CreateSnapshotFromVolumeRecoveryPointResponse = CreateSnapshotFromVolumeRecoveryPointResponse
    { _csfvrprSnapshotId              :: Maybe Text
    , _csfvrprVolumeARN               :: Maybe Text
    , _csfvrprVolumeRecoveryPointTime :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateSnapshotFromVolumeRecoveryPointResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csfvrprSnapshotId' @::@ 'Maybe' 'Text'
--
-- * 'csfvrprVolumeARN' @::@ 'Maybe' 'Text'
--
-- * 'csfvrprVolumeRecoveryPointTime' @::@ 'Maybe' 'Text'
--
createSnapshotFromVolumeRecoveryPointResponse :: CreateSnapshotFromVolumeRecoveryPointResponse
createSnapshotFromVolumeRecoveryPointResponse = CreateSnapshotFromVolumeRecoveryPointResponse
    { _csfvrprSnapshotId              = Nothing
    , _csfvrprVolumeARN               = Nothing
    , _csfvrprVolumeRecoveryPointTime = Nothing
    }

csfvrprSnapshotId :: Lens' CreateSnapshotFromVolumeRecoveryPointResponse (Maybe Text)
csfvrprSnapshotId =
    lens _csfvrprSnapshotId (\s a -> s { _csfvrprSnapshotId = a })

csfvrprVolumeARN :: Lens' CreateSnapshotFromVolumeRecoveryPointResponse (Maybe Text)
csfvrprVolumeARN = lens _csfvrprVolumeARN (\s a -> s { _csfvrprVolumeARN = a })

csfvrprVolumeRecoveryPointTime :: Lens' CreateSnapshotFromVolumeRecoveryPointResponse (Maybe Text)
csfvrprVolumeRecoveryPointTime =
    lens _csfvrprVolumeRecoveryPointTime
        (\s a -> s { _csfvrprVolumeRecoveryPointTime = a })

instance ToPath CreateSnapshotFromVolumeRecoveryPoint where
    toPath = const "/"

instance ToQuery CreateSnapshotFromVolumeRecoveryPoint where
    toQuery = const mempty

instance ToHeaders CreateSnapshotFromVolumeRecoveryPoint

instance ToJSON CreateSnapshotFromVolumeRecoveryPoint where
    toJSON CreateSnapshotFromVolumeRecoveryPoint{..} = object
        [ "VolumeARN"           .= _csfvrpVolumeARN
        , "SnapshotDescription" .= _csfvrpSnapshotDescription
        ]

instance AWSRequest CreateSnapshotFromVolumeRecoveryPoint where
    type Sv CreateSnapshotFromVolumeRecoveryPoint = StorageGateway
    type Rs CreateSnapshotFromVolumeRecoveryPoint = CreateSnapshotFromVolumeRecoveryPointResponse

    request  = post "CreateSnapshotFromVolumeRecoveryPoint"
    response = jsonResponse

instance FromJSON CreateSnapshotFromVolumeRecoveryPointResponse where
    parseJSON = withObject "CreateSnapshotFromVolumeRecoveryPointResponse" $ \o -> CreateSnapshotFromVolumeRecoveryPointResponse
        <$> o .:? "SnapshotId"
        <*> o .:? "VolumeARN"
        <*> o .:? "VolumeRecoveryPointTime"
