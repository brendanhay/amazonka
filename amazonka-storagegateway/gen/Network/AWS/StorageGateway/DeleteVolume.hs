{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.StorageGateway.DeleteVolume
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation delete the specified gateway volume that you previously
-- created using the CreateStorediSCSIVolume API. For gateway-stored volumes,
-- the local disk that was configured as the storage volume is not deleted.
-- You can reuse the local disk to create another storage volume. Before you
-- delete a gateway volume, make sure there are no iSCSI connections to the
-- volume you are deleting. You should also make sure there is no snapshot in
-- progress. You can use the Amazon Elastic Compute Cloud (Amazon EC2) API to
-- query snapshots on the volume you are deleting and check the snapshot
-- status. For more information, go to DescribeSnapshots in the Amazon Elastic
-- Compute Cloud API Reference. In the request, you must provide the Amazon
-- Resource Name (ARN) of the storage volume you want to delete.
module Network.AWS.StorageGateway.DeleteVolume
    (
    -- * Request
      DeleteVolume
    -- ** Request constructor
    , deleteVolume
    -- ** Request lenses
    , dvVolumeARN

    -- * Response
    , DeleteVolumeResponse
    -- ** Response constructor
    , deleteVolumeResponse
    -- ** Response lenses
    , dvrVolumeARN
    ) where

import Data.Aeson
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.StorageGateway.Types

newtype DeleteVolume = DeleteVolume
    { _dvVolumeARN :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteVolume' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvVolumeARN' @::@ 'Text'
--
deleteVolume :: Text -- ^ 'dvVolumeARN'
             -> DeleteVolume
deleteVolume p1 = DeleteVolume
    { _dvVolumeARN = p1
    }

-- | The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
-- operation to return a list of gateway volumes.
dvVolumeARN :: Lens' DeleteVolume Text
dvVolumeARN = lens _dvVolumeARN (\s a -> s { _dvVolumeARN = a })

instance ToPath DeleteVolume where
    toPath = const "/"

instance ToQuery DeleteVolume where
    toQuery = const mempty

instance ToHeaders DeleteVolume

instance ToBody DeleteVolume where
    toBody = toBody . encode . _dvVolumeARN

newtype DeleteVolumeResponse = DeleteVolumeResponse
    { _dvrVolumeARN :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DeleteVolumeResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvrVolumeARN' @::@ 'Maybe' 'Text'
--
deleteVolumeResponse :: DeleteVolumeResponse
deleteVolumeResponse = DeleteVolumeResponse
    { _dvrVolumeARN = Nothing
    }

-- | The Amazon Resource Name (ARN) of the storage volume that was deleted. It
-- is the same ARN you provided in the request.
dvrVolumeARN :: Lens' DeleteVolumeResponse (Maybe Text)
dvrVolumeARN = lens _dvrVolumeARN (\s a -> s { _dvrVolumeARN = a })

-- FromJSON

instance AWSRequest DeleteVolume where
    type Sv DeleteVolume = StorageGateway
    type Rs DeleteVolume = DeleteVolumeResponse

    request  = post'
    response = jsonResponse $ \h o -> DeleteVolumeResponse
        <$> o .: "VolumeARN"
