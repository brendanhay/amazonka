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

-- Module      : Network.AWS.EC2.DeleteVolume
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified Amazon EBS volume. The volume must be in the
-- available state (not attached to an instance). For more information, see
-- Deleting an Amazon EBS Volume in the Amazon Elastic Compute Cloud User
-- Guide.
module Network.AWS.EC2.DeleteVolume
    (
    -- * Request
      DeleteVolume
    -- ** Request constructor
    , deleteVolume
    -- ** Request lenses
    , dv3DryRun
    , dv3VolumeId

    -- * Response
    , DeleteVolumeResponse
    -- ** Response constructor
    , deleteVolumeResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DeleteVolume = DeleteVolume
    { _dv3DryRun   :: Maybe Bool
    , _dv3VolumeId :: Text
    } (Eq, Ord, Show, Generic)

-- | 'DeleteVolume' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dv3DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dv3VolumeId' @::@ 'Text'
--
deleteVolume :: Text -- ^ 'dv3VolumeId'
             -> DeleteVolume
deleteVolume p1 = DeleteVolume
    { _dv3VolumeId = p1
    , _dv3DryRun   = Nothing
    }

dv3DryRun :: Lens' DeleteVolume (Maybe Bool)
dv3DryRun = lens _dv3DryRun (\s a -> s { _dv3DryRun = a })

-- | The ID of the volume.
dv3VolumeId :: Lens' DeleteVolume Text
dv3VolumeId = lens _dv3VolumeId (\s a -> s { _dv3VolumeId = a })
instance ToQuery DeleteVolume

instance ToPath DeleteVolume where
    toPath = const "/"

data DeleteVolumeResponse = DeleteVolumeResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteVolumeResponse' constructor.
deleteVolumeResponse :: DeleteVolumeResponse
deleteVolumeResponse = DeleteVolumeResponse

instance FromXML DeleteVolumeResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteVolumeResponse"

instance AWSRequest DeleteVolume where
    type Sv DeleteVolume = EC2
    type Rs DeleteVolume = DeleteVolumeResponse

    request  = post "DeleteVolume"
    response = nullaryResponse DeleteVolumeResponse
