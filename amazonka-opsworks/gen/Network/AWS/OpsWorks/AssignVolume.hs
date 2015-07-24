{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.AssignVolume
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Assigns one of the stack\'s registered Amazon EBS volumes to a specified
-- instance. The volume must first be registered with the stack by calling
-- RegisterVolume. After you register the volume, you must call
-- UpdateVolume to specify a mount point before calling @AssignVolume@. For
-- more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_AssignVolume.html>
module Network.AWS.OpsWorks.AssignVolume
    (
    -- * Request
      AssignVolume
    -- ** Request constructor
    , assignVolume
    -- ** Request lenses
    , avInstanceId
    , avVolumeId

    -- * Response
    , AssignVolumeResponse
    -- ** Response constructor
    , assignVolumeResponse
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'assignVolume' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avInstanceId'
--
-- * 'avVolumeId'
data AssignVolume = AssignVolume'
    { _avInstanceId :: !(Maybe Text)
    , _avVolumeId   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AssignVolume' smart constructor.
assignVolume :: Text -> AssignVolume
assignVolume pVolumeId_ =
    AssignVolume'
    { _avInstanceId = Nothing
    , _avVolumeId = pVolumeId_
    }

-- | The instance ID.
avInstanceId :: Lens' AssignVolume (Maybe Text)
avInstanceId = lens _avInstanceId (\ s a -> s{_avInstanceId = a});

-- | The volume ID.
avVolumeId :: Lens' AssignVolume Text
avVolumeId = lens _avVolumeId (\ s a -> s{_avVolumeId = a});

instance AWSRequest AssignVolume where
        type Sv AssignVolume = OpsWorks
        type Rs AssignVolume = AssignVolumeResponse
        request = postJSON "AssignVolume"
        response = receiveNull AssignVolumeResponse'

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
              ["InstanceId" .= _avInstanceId,
               "VolumeId" .= _avVolumeId]

instance ToPath AssignVolume where
        toPath = const "/"

instance ToQuery AssignVolume where
        toQuery = const mempty

-- | /See:/ 'assignVolumeResponse' smart constructor.
data AssignVolumeResponse =
    AssignVolumeResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AssignVolumeResponse' smart constructor.
assignVolumeResponse :: AssignVolumeResponse
assignVolumeResponse = AssignVolumeResponse'
