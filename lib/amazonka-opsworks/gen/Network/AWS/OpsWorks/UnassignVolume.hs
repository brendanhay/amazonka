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
-- Module      : Network.AWS.OpsWorks.UnassignVolume
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unassigns an assigned Amazon EBS volume. The volume remains registered with the stack. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management> .
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.UnassignVolume
    (
    -- * Creating a Request
      unassignVolume
    , UnassignVolume
    -- * Request Lenses
    , uvVolumeId

    -- * Destructuring the Response
    , unassignVolumeResponse
    , UnassignVolumeResponse
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'unassignVolume' smart constructor.
newtype UnassignVolume = UnassignVolume'
  { _uvVolumeId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnassignVolume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uvVolumeId' - The volume ID.
unassignVolume
    :: Text -- ^ 'uvVolumeId'
    -> UnassignVolume
unassignVolume pVolumeId_ = UnassignVolume' {_uvVolumeId = pVolumeId_}


-- | The volume ID.
uvVolumeId :: Lens' UnassignVolume Text
uvVolumeId = lens _uvVolumeId (\ s a -> s{_uvVolumeId = a})

instance AWSRequest UnassignVolume where
        type Rs UnassignVolume = UnassignVolumeResponse
        request = postJSON opsWorks
        response = receiveNull UnassignVolumeResponse'

instance Hashable UnassignVolume where

instance NFData UnassignVolume where

instance ToHeaders UnassignVolume where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.UnassignVolume" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UnassignVolume where
        toJSON UnassignVolume'{..}
          = object
              (catMaybes [Just ("VolumeId" .= _uvVolumeId)])

instance ToPath UnassignVolume where
        toPath = const "/"

instance ToQuery UnassignVolume where
        toQuery = const mempty

-- | /See:/ 'unassignVolumeResponse' smart constructor.
data UnassignVolumeResponse =
  UnassignVolumeResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnassignVolumeResponse' with the minimum fields required to make a request.
--
unassignVolumeResponse
    :: UnassignVolumeResponse
unassignVolumeResponse = UnassignVolumeResponse'


instance NFData UnassignVolumeResponse where
