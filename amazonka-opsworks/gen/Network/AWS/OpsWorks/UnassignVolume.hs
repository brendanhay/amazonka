{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.OpsWorks.UnassignVolume
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Unassigns an assigned Amazon EBS volume. The volume remains registered
-- with the stack. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_UnassignVolume.html>
module Network.AWS.OpsWorks.UnassignVolume
    (
    -- * Request
      UnassignVolume
    -- ** Request constructor
    , unassignVolume
    -- ** Request lenses
    , uvVolumeId

    -- * Response
    , UnassignVolumeResponse
    -- ** Response constructor
    , unassignVolumeResponse
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'unassignVolume' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uvVolumeId'
newtype UnassignVolume = UnassignVolume'
    { _uvVolumeId :: Text
    } deriving (Eq,Read,Show)

-- | 'UnassignVolume' smart constructor.
unassignVolume :: Text -> UnassignVolume
unassignVolume pVolumeId =
    UnassignVolume'
    { _uvVolumeId = pVolumeId
    }

-- | The volume ID.
uvVolumeId :: Lens' UnassignVolume Text
uvVolumeId = lens _uvVolumeId (\ s a -> s{_uvVolumeId = a});

instance AWSRequest UnassignVolume where
        type Sv UnassignVolume = OpsWorks
        type Rs UnassignVolume = UnassignVolumeResponse
        request = postJSON
        response = receiveNull UnassignVolumeResponse'

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
          = object ["VolumeId" .= _uvVolumeId]

instance ToPath UnassignVolume where
        toPath = const "/"

instance ToQuery UnassignVolume where
        toQuery = const mempty

-- | /See:/ 'unassignVolumeResponse' smart constructor.
data UnassignVolumeResponse =
    UnassignVolumeResponse'
    deriving (Eq,Read,Show)

-- | 'UnassignVolumeResponse' smart constructor.
unassignVolumeResponse :: UnassignVolumeResponse
unassignVolumeResponse = UnassignVolumeResponse'
