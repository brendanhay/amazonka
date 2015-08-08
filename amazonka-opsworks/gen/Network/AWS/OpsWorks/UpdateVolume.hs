{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.UpdateVolume
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Updates an Amazon EBS volume\'s name or mount point. For more
-- information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- /See:/ <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_UpdateVolume.html AWS API Reference> for UpdateVolume.
module Network.AWS.OpsWorks.UpdateVolume
    (
    -- * Creating a Request
      UpdateVolume
    , updateVolume
    -- * Request Lenses
    , uName
    , uMountPoint
    , uVolumeId

    -- * Destructuring the Response
    , UpdateVolumeResponse
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
-- * 'uName'
--
-- * 'uMountPoint'
--
-- * 'uVolumeId'
data UpdateVolume = UpdateVolume'
    { _uName       :: !(Maybe Text)
    , _uMountPoint :: !(Maybe Text)
    , _uVolumeId   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateVolume' smart constructor.
updateVolume :: Text -> UpdateVolume
updateVolume pVolumeId_ =
    UpdateVolume'
    { _uName = Nothing
    , _uMountPoint = Nothing
    , _uVolumeId = pVolumeId_
    }

-- | The new name.
uName :: Lens' UpdateVolume (Maybe Text)
uName = lens _uName (\ s a -> s{_uName = a});

-- | The new mount point.
uMountPoint :: Lens' UpdateVolume (Maybe Text)
uMountPoint = lens _uMountPoint (\ s a -> s{_uMountPoint = a});

-- | The volume ID.
uVolumeId :: Lens' UpdateVolume Text
uVolumeId = lens _uVolumeId (\ s a -> s{_uVolumeId = a});

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
              ["Name" .= _uName, "MountPoint" .= _uMountPoint,
               "VolumeId" .= _uVolumeId]

instance ToPath UpdateVolume where
        toPath = const "/"

instance ToQuery UpdateVolume where
        toQuery = const mempty

-- | /See:/ 'updateVolumeResponse' smart constructor.
data UpdateVolumeResponse =
    UpdateVolumeResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateVolumeResponse' smart constructor.
updateVolumeResponse :: UpdateVolumeResponse
updateVolumeResponse = UpdateVolumeResponse'
