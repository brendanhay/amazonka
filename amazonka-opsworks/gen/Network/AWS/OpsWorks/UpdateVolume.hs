{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.UpdateVolume
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an Amazon EBS volume's name or mount point. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management> .
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.UpdateVolume
    (
    -- * Creating a Request
      updateVolume
    , UpdateVolume
    -- * Request Lenses
    , uName
    , uMountPoint
    , uVolumeId

    -- * Destructuring the Response
    , updateVolumeResponse
    , UpdateVolumeResponse
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateVolume' smart constructor.
data UpdateVolume = UpdateVolume'
  { _uName       :: !(Maybe Text)
  , _uMountPoint :: !(Maybe Text)
  , _uVolumeId   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateVolume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uName' - The new name.
--
-- * 'uMountPoint' - The new mount point.
--
-- * 'uVolumeId' - The volume ID.
updateVolume
    :: Text -- ^ 'uVolumeId'
    -> UpdateVolume
updateVolume pVolumeId_ =
  UpdateVolume'
    {_uName = Nothing, _uMountPoint = Nothing, _uVolumeId = pVolumeId_}


-- | The new name.
uName :: Lens' UpdateVolume (Maybe Text)
uName = lens _uName (\ s a -> s{_uName = a})

-- | The new mount point.
uMountPoint :: Lens' UpdateVolume (Maybe Text)
uMountPoint = lens _uMountPoint (\ s a -> s{_uMountPoint = a})

-- | The volume ID.
uVolumeId :: Lens' UpdateVolume Text
uVolumeId = lens _uVolumeId (\ s a -> s{_uVolumeId = a})

instance AWSRequest UpdateVolume where
        type Rs UpdateVolume = UpdateVolumeResponse
        request = postJSON opsWorks
        response = receiveNull UpdateVolumeResponse'

instance Hashable UpdateVolume where

instance NFData UpdateVolume where

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
              (catMaybes
                 [("Name" .=) <$> _uName,
                  ("MountPoint" .=) <$> _uMountPoint,
                  Just ("VolumeId" .= _uVolumeId)])

instance ToPath UpdateVolume where
        toPath = const "/"

instance ToQuery UpdateVolume where
        toQuery = const mempty

-- | /See:/ 'updateVolumeResponse' smart constructor.
data UpdateVolumeResponse =
  UpdateVolumeResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateVolumeResponse' with the minimum fields required to make a request.
--
updateVolumeResponse
    :: UpdateVolumeResponse
updateVolumeResponse = UpdateVolumeResponse'


instance NFData UpdateVolumeResponse where
