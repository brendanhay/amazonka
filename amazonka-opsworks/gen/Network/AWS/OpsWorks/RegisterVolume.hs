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
-- Module      : Network.AWS.OpsWorks.RegisterVolume
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an Amazon EBS volume with a specified stack. A volume can be
-- registered with only one stack at a time. If the volume is already
-- registered, you must first deregister it by calling DeregisterVolume.
-- For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- /See:/ <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_RegisterVolume.html AWS API Reference> for RegisterVolume.
module Network.AWS.OpsWorks.RegisterVolume
    (
    -- * Creating a Request
      RegisterVolume
    , registerVolume
    -- * Request Lenses
    , rvEC2VolumeId
    , rvStackId

    -- * Destructuring the Response
    , RegisterVolumeResponse
    , registerVolumeResponse
    -- * Response Lenses
    , rvrsVolumeId
    , rvrsStatus
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'registerVolume' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rvEC2VolumeId'
--
-- * 'rvStackId'
data RegisterVolume = RegisterVolume'
    { _rvEC2VolumeId :: !(Maybe Text)
    , _rvStackId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterVolume' smart constructor.
registerVolume :: Text -> RegisterVolume
registerVolume pStackId_ = 
    RegisterVolume'
    { _rvEC2VolumeId = Nothing
    , _rvStackId = pStackId_
    }

-- | The Amazon EBS volume ID.
rvEC2VolumeId :: Lens' RegisterVolume (Maybe Text)
rvEC2VolumeId = lens _rvEC2VolumeId (\ s a -> s{_rvEC2VolumeId = a});

-- | The stack ID.
rvStackId :: Lens' RegisterVolume Text
rvStackId = lens _rvStackId (\ s a -> s{_rvStackId = a});

instance AWSRequest RegisterVolume where
        type Sv RegisterVolume = OpsWorks
        type Rs RegisterVolume = RegisterVolumeResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 RegisterVolumeResponse' <$>
                   (x .?> "VolumeId") <*> (pure (fromEnum s)))

instance ToHeaders RegisterVolume where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.RegisterVolume" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RegisterVolume where
        toJSON RegisterVolume'{..}
          = object
              ["Ec2VolumeId" .= _rvEC2VolumeId,
               "StackId" .= _rvStackId]

instance ToPath RegisterVolume where
        toPath = const "/"

instance ToQuery RegisterVolume where
        toQuery = const mempty

-- | Contains the response to a @RegisterVolume@ request.
--
-- /See:/ 'registerVolumeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rvrsVolumeId'
--
-- * 'rvrsStatus'
data RegisterVolumeResponse = RegisterVolumeResponse'
    { _rvrsVolumeId :: !(Maybe Text)
    , _rvrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterVolumeResponse' smart constructor.
registerVolumeResponse :: Int -> RegisterVolumeResponse
registerVolumeResponse pStatus_ = 
    RegisterVolumeResponse'
    { _rvrsVolumeId = Nothing
    , _rvrsStatus = pStatus_
    }

-- | The volume ID.
rvrsVolumeId :: Lens' RegisterVolumeResponse (Maybe Text)
rvrsVolumeId = lens _rvrsVolumeId (\ s a -> s{_rvrsVolumeId = a});

-- | Undocumented member.
rvrsStatus :: Lens' RegisterVolumeResponse Int
rvrsStatus = lens _rvrsStatus (\ s a -> s{_rvrsStatus = a});
