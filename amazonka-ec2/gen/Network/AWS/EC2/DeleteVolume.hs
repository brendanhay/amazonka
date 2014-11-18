{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteVolume.html>
module Network.AWS.EC2.DeleteVolume
    (
    -- * Request
      DeleteVolume
    -- ** Request constructor
    , deleteVolume
    -- ** Request lenses
    , dv4DryRun
    , dv4VolumeId

    -- * Response
    , DeleteVolumeResponse
    -- ** Response constructor
    , deleteVolumeResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DeleteVolume = DeleteVolume
    { _dv4DryRun   :: Maybe Bool
    , _dv4VolumeId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteVolume' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dv4DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dv4VolumeId' @::@ 'Text'
--
deleteVolume :: Text -- ^ 'dv4VolumeId'
             -> DeleteVolume
deleteVolume p1 = DeleteVolume
    { _dv4VolumeId = p1
    , _dv4DryRun   = Nothing
    }

dv4DryRun :: Lens' DeleteVolume (Maybe Bool)
dv4DryRun = lens _dv4DryRun (\s a -> s { _dv4DryRun = a })

-- | The ID of the volume.
dv4VolumeId :: Lens' DeleteVolume Text
dv4VolumeId = lens _dv4VolumeId (\s a -> s { _dv4VolumeId = a })

data DeleteVolumeResponse = DeleteVolumeResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteVolumeResponse' constructor.
deleteVolumeResponse :: DeleteVolumeResponse
deleteVolumeResponse = DeleteVolumeResponse

instance ToPath DeleteVolume where
    toPath = const "/"

instance ToQuery DeleteVolume

instance ToHeaders DeleteVolume

instance AWSRequest DeleteVolume where
    type Sv DeleteVolume = EC2
    type Rs DeleteVolume = DeleteVolumeResponse

    request  = post "DeleteVolume"
    response = nullResponse DeleteVolumeResponse
