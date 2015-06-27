{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

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

-- | Updates an Amazon EBS volume\'s name or mount point. For more
-- information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_UpdateVolume.html>
module Network.AWS.OpsWorks.UpdateVolume
    (
    -- * Request
      UpdateVolume
    -- ** Request constructor
    , updateVolume
    -- ** Request lenses
    , updName
    , updMountPoint
    , updVolumeId

    -- * Response
    , UpdateVolumeResponse
    -- ** Response constructor
    , updateVolumeResponse
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateVolume' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'updName'
--
-- * 'updMountPoint'
--
-- * 'updVolumeId'
data UpdateVolume = UpdateVolume'
    { _updName       :: Maybe Text
    , _updMountPoint :: Maybe Text
    , _updVolumeId   :: Text
    } deriving (Eq,Read,Show)

-- | 'UpdateVolume' smart constructor.
updateVolume :: Text -> UpdateVolume
updateVolume pVolumeId =
    UpdateVolume'
    { _updName = Nothing
    , _updMountPoint = Nothing
    , _updVolumeId = pVolumeId
    }

-- | The new name.
updName :: Lens' UpdateVolume (Maybe Text)
updName = lens _updName (\ s a -> s{_updName = a});

-- | The new mount point.
updMountPoint :: Lens' UpdateVolume (Maybe Text)
updMountPoint = lens _updMountPoint (\ s a -> s{_updMountPoint = a});

-- | The volume ID.
updVolumeId :: Lens' UpdateVolume Text
updVolumeId = lens _updVolumeId (\ s a -> s{_updVolumeId = a});

instance AWSRequest UpdateVolume where
        type Sv UpdateVolume = OpsWorks
        type Rs UpdateVolume = UpdateVolumeResponse
        request = postJSON
        response = receiveNull UpdateVolumeResponse'

instance ToHeaders UpdateVolume where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.UpdateVolume" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateVolume where
        toJSON UpdateVolume'{..}
          = object
              ["Name" .= _updName, "MountPoint" .= _updMountPoint,
               "VolumeId" .= _updVolumeId]

instance ToPath UpdateVolume where
        toPath = const "/"

instance ToQuery UpdateVolume where
        toQuery = const mempty

-- | /See:/ 'updateVolumeResponse' smart constructor.
data UpdateVolumeResponse =
    UpdateVolumeResponse'
    deriving (Eq,Read,Show)

-- | 'UpdateVolumeResponse' smart constructor.
updateVolumeResponse :: UpdateVolumeResponse
updateVolumeResponse = UpdateVolumeResponse'
