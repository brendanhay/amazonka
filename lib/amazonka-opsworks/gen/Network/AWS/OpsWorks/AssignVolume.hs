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
-- Module      : Network.AWS.OpsWorks.AssignVolume
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns one of the stack's registered Amazon EBS volumes to a specified instance. The volume must first be registered with the stack by calling 'RegisterVolume' . After you register the volume, you must call 'UpdateVolume' to specify a mount point before calling @AssignVolume@ . For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management> .
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.AssignVolume
    (
    -- * Creating a Request
      assignVolume
    , AssignVolume
    -- * Request Lenses
    , avInstanceId
    , avVolumeId

    -- * Destructuring the Response
    , assignVolumeResponse
    , AssignVolumeResponse
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'assignVolume' smart constructor.
data AssignVolume = AssignVolume'
  { _avInstanceId :: !(Maybe Text)
  , _avVolumeId   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssignVolume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avInstanceId' - The instance ID.
--
-- * 'avVolumeId' - The volume ID.
assignVolume
    :: Text -- ^ 'avVolumeId'
    -> AssignVolume
assignVolume pVolumeId_ =
  AssignVolume' {_avInstanceId = Nothing, _avVolumeId = pVolumeId_}


-- | The instance ID.
avInstanceId :: Lens' AssignVolume (Maybe Text)
avInstanceId = lens _avInstanceId (\ s a -> s{_avInstanceId = a})

-- | The volume ID.
avVolumeId :: Lens' AssignVolume Text
avVolumeId = lens _avVolumeId (\ s a -> s{_avVolumeId = a})

instance AWSRequest AssignVolume where
        type Rs AssignVolume = AssignVolumeResponse
        request = postJSON opsWorks
        response = receiveNull AssignVolumeResponse'

instance Hashable AssignVolume where

instance NFData AssignVolume where

instance ToHeaders AssignVolume where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.AssignVolume" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AssignVolume where
        toJSON AssignVolume'{..}
          = object
              (catMaybes
                 [("InstanceId" .=) <$> _avInstanceId,
                  Just ("VolumeId" .= _avVolumeId)])

instance ToPath AssignVolume where
        toPath = const "/"

instance ToQuery AssignVolume where
        toQuery = const mempty

-- | /See:/ 'assignVolumeResponse' smart constructor.
data AssignVolumeResponse =
  AssignVolumeResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssignVolumeResponse' with the minimum fields required to make a request.
--
assignVolumeResponse
    :: AssignVolumeResponse
assignVolumeResponse = AssignVolumeResponse'


instance NFData AssignVolumeResponse where
