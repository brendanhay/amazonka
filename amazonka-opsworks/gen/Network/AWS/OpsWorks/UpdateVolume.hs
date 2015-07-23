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
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_UpdateVolume.html>
module Network.AWS.OpsWorks.UpdateVolume
    (
    -- * Request
      UpdateVolume
    -- ** Request constructor
    , updateVolume
    -- ** Request lenses
    , urqName
    , urqMountPoint
    , urqVolumeId

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
-- * 'urqName'
--
-- * 'urqMountPoint'
--
-- * 'urqVolumeId'
data UpdateVolume = UpdateVolume'
    { _urqName       :: !(Maybe Text)
    , _urqMountPoint :: !(Maybe Text)
    , _urqVolumeId   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateVolume' smart constructor.
updateVolume :: Text -> UpdateVolume
updateVolume pVolumeId_ =
    UpdateVolume'
    { _urqName = Nothing
    , _urqMountPoint = Nothing
    , _urqVolumeId = pVolumeId_
    }

-- | The new name.
urqName :: Lens' UpdateVolume (Maybe Text)
urqName = lens _urqName (\ s a -> s{_urqName = a});

-- | The new mount point.
urqMountPoint :: Lens' UpdateVolume (Maybe Text)
urqMountPoint = lens _urqMountPoint (\ s a -> s{_urqMountPoint = a});

-- | The volume ID.
urqVolumeId :: Lens' UpdateVolume Text
urqVolumeId = lens _urqVolumeId (\ s a -> s{_urqVolumeId = a});

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
              ["Name" .= _urqName, "MountPoint" .= _urqMountPoint,
               "VolumeId" .= _urqVolumeId]

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
