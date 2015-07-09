{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DeregisterVolume
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Deregisters an Amazon EBS volume. The volume can then be registered by
-- another stack. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DeregisterVolume.html>
module Network.AWS.OpsWorks.DeregisterVolume
    (
    -- * Request
      DeregisterVolume
    -- ** Request constructor
    , deregisterVolume
    -- ** Request lenses
    , dvVolumeId

    -- * Response
    , DeregisterVolumeResponse
    -- ** Response constructor
    , deregisterVolumeResponse
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deregisterVolume' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvVolumeId'
newtype DeregisterVolume = DeregisterVolume'
    { _dvVolumeId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeregisterVolume' smart constructor.
deregisterVolume :: Text -> DeregisterVolume
deregisterVolume pVolumeId =
    DeregisterVolume'
    { _dvVolumeId = pVolumeId
    }

-- | The AWS OpsWorks volume ID, which is the GUID that AWS OpsWorks assigned
-- to the instance when you registered the volume with the stack, not the
-- Amazon EC2 volume ID.
dvVolumeId :: Lens' DeregisterVolume Text
dvVolumeId = lens _dvVolumeId (\ s a -> s{_dvVolumeId = a});

instance AWSRequest DeregisterVolume where
        type Sv DeregisterVolume = OpsWorks
        type Rs DeregisterVolume = DeregisterVolumeResponse
        request = postJSON
        response = receiveNull DeregisterVolumeResponse'

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
          = object ["VolumeId" .= _dvVolumeId]

instance ToPath DeregisterVolume where
        toPath = const "/"

instance ToQuery DeregisterVolume where
        toQuery = const mempty

-- | /See:/ 'deregisterVolumeResponse' smart constructor.
data DeregisterVolumeResponse =
    DeregisterVolumeResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeregisterVolumeResponse' smart constructor.
deregisterVolumeResponse :: DeregisterVolumeResponse
deregisterVolumeResponse = DeregisterVolumeResponse'
