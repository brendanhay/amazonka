{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.UpdateVolume
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates an Amazon EBS volume's name or mount point. For more information,
-- see Resource Management. Required Permissions: To use this action, an IAM
-- user must have a Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.UpdateVolume
    (
    -- * Request
      UpdateVolume
    -- ** Request constructor
    , mkUpdateVolume
    -- ** Request lenses
    , uv1VolumeId
    , uv1Name
    , uv1MountPoint

    -- * Response
    , UpdateVolumeResponse
    -- ** Response constructor
    , mkUpdateVolumeResponse
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data UpdateVolume = UpdateVolume
    { _uv1VolumeId :: Text
    , _uv1Name :: Maybe Text
    , _uv1MountPoint :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateVolume' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VolumeId ::@ @Text@
--
-- * @Name ::@ @Maybe Text@
--
-- * @MountPoint ::@ @Maybe Text@
--
mkUpdateVolume :: Text -- ^ 'uv1VolumeId'
               -> UpdateVolume
mkUpdateVolume p1 = UpdateVolume
    { _uv1VolumeId = p1
    , _uv1Name = Nothing
    , _uv1MountPoint = Nothing
    }

-- | The volume ID.
uv1VolumeId :: Lens' UpdateVolume Text
uv1VolumeId = lens _uv1VolumeId (\s a -> s { _uv1VolumeId = a })

-- | The new name.
uv1Name :: Lens' UpdateVolume (Maybe Text)
uv1Name = lens _uv1Name (\s a -> s { _uv1Name = a })

-- | The new mount point.
uv1MountPoint :: Lens' UpdateVolume (Maybe Text)
uv1MountPoint = lens _uv1MountPoint (\s a -> s { _uv1MountPoint = a })

instance ToPath UpdateVolume

instance ToQuery UpdateVolume

instance ToHeaders UpdateVolume

instance ToJSON UpdateVolume

data UpdateVolumeResponse = UpdateVolumeResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateVolumeResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkUpdateVolumeResponse :: UpdateVolumeResponse
mkUpdateVolumeResponse = UpdateVolumeResponse

instance AWSRequest UpdateVolume where
    type Sv UpdateVolume = OpsWorks
    type Rs UpdateVolume = UpdateVolumeResponse

    request = get
    response _ = nullaryResponse UpdateVolumeResponse
