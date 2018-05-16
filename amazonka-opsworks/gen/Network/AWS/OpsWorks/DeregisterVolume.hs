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
-- Module      : Network.AWS.OpsWorks.DeregisterVolume
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters an Amazon EBS volume. The volume can then be registered by another stack. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management> .
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.DeregisterVolume
    (
    -- * Creating a Request
      deregisterVolume
    , DeregisterVolume
    -- * Request Lenses
    , dvVolumeId

    -- * Destructuring the Response
    , deregisterVolumeResponse
    , DeregisterVolumeResponse
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deregisterVolume' smart constructor.
newtype DeregisterVolume = DeregisterVolume'
  { _dvVolumeId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterVolume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvVolumeId' - The AWS OpsWorks Stacks volume ID, which is the GUID that AWS OpsWorks Stacks assigned to the instance when you registered the volume with the stack, not the Amazon EC2 volume ID.
deregisterVolume
    :: Text -- ^ 'dvVolumeId'
    -> DeregisterVolume
deregisterVolume pVolumeId_ = DeregisterVolume' {_dvVolumeId = pVolumeId_}


-- | The AWS OpsWorks Stacks volume ID, which is the GUID that AWS OpsWorks Stacks assigned to the instance when you registered the volume with the stack, not the Amazon EC2 volume ID.
dvVolumeId :: Lens' DeregisterVolume Text
dvVolumeId = lens _dvVolumeId (\ s a -> s{_dvVolumeId = a})

instance AWSRequest DeregisterVolume where
        type Rs DeregisterVolume = DeregisterVolumeResponse
        request = postJSON opsWorks
        response = receiveNull DeregisterVolumeResponse'

instance Hashable DeregisterVolume where

instance NFData DeregisterVolume where

instance ToHeaders DeregisterVolume where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DeregisterVolume" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeregisterVolume where
        toJSON DeregisterVolume'{..}
          = object
              (catMaybes [Just ("VolumeId" .= _dvVolumeId)])

instance ToPath DeregisterVolume where
        toPath = const "/"

instance ToQuery DeregisterVolume where
        toQuery = const mempty

-- | /See:/ 'deregisterVolumeResponse' smart constructor.
data DeregisterVolumeResponse =
  DeregisterVolumeResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterVolumeResponse' with the minimum fields required to make a request.
--
deregisterVolumeResponse
    :: DeregisterVolumeResponse
deregisterVolumeResponse = DeregisterVolumeResponse'


instance NFData DeregisterVolumeResponse where
