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

-- Module      : Network.AWS.OpsWorks.UpdateVolume
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Updates an Amazon EBS volume's name or mount point. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management>.
--
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing UserPermissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_UpdateVolume.html>
module Network.AWS.OpsWorks.UpdateVolume
    (
    -- * Request
      UpdateVolume
    -- ** Request constructor
    , updateVolume
    -- ** Request lenses
    , uv1MountPoint
    , uv1Name
    , uv1VolumeId

    -- * Response
    , UpdateVolumeResponse
    -- ** Response constructor
    , updateVolumeResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

data UpdateVolume = UpdateVolume
    { _uv1MountPoint :: Maybe Text
    , _uv1Name       :: Maybe Text
    , _uv1VolumeId   :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'UpdateVolume' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uv1MountPoint' @::@ 'Maybe' 'Text'
--
-- * 'uv1Name' @::@ 'Maybe' 'Text'
--
-- * 'uv1VolumeId' @::@ 'Text'
--
updateVolume :: Text -- ^ 'uv1VolumeId'
             -> UpdateVolume
updateVolume p1 = UpdateVolume
    { _uv1VolumeId   = p1
    , _uv1Name       = Nothing
    , _uv1MountPoint = Nothing
    }

-- | The new mount point.
uv1MountPoint :: Lens' UpdateVolume (Maybe Text)
uv1MountPoint = lens _uv1MountPoint (\s a -> s { _uv1MountPoint = a })

-- | The new name.
uv1Name :: Lens' UpdateVolume (Maybe Text)
uv1Name = lens _uv1Name (\s a -> s { _uv1Name = a })

-- | The volume ID.
uv1VolumeId :: Lens' UpdateVolume Text
uv1VolumeId = lens _uv1VolumeId (\s a -> s { _uv1VolumeId = a })

data UpdateVolumeResponse = UpdateVolumeResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'UpdateVolumeResponse' constructor.
updateVolumeResponse :: UpdateVolumeResponse
updateVolumeResponse = UpdateVolumeResponse

instance ToPath UpdateVolume where
    toPath = const "/"

instance ToQuery UpdateVolume where
    toQuery = const mempty

instance ToHeaders UpdateVolume

instance ToJSON UpdateVolume where
    toJSON UpdateVolume{..} = object
        [ "VolumeId"   .= _uv1VolumeId
        , "Name"       .= _uv1Name
        , "MountPoint" .= _uv1MountPoint
        ]

instance AWSRequest UpdateVolume where
    type Sv UpdateVolume = OpsWorks
    type Rs UpdateVolume = UpdateVolumeResponse

    request  = post "UpdateVolume"
    response = nullResponse UpdateVolumeResponse
