{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.OpsWorks.UnassignInstance
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Unassigns a registered instance from all of it\'s layers. The instance
-- remains in the stack as an unassigned instance and can be assigned to
-- another layer, as needed. You cannot use this action with instances that
-- were created with AWS OpsWorks.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_UnassignInstance.html>
module Network.AWS.OpsWorks.UnassignInstance
    (
    -- * Request
      UnassignInstance
    -- ** Request constructor
    , unassignInstance
    -- ** Request lenses
    , unaInstanceId

    -- * Response
    , UnassignInstanceResponse
    -- ** Response constructor
    , unassignInstanceResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.OpsWorks.Types

-- | /See:/ 'unassignInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'unaInstanceId'
newtype UnassignInstance = UnassignInstance'{_unaInstanceId :: Text} deriving (Eq, Read, Show)

-- | 'UnassignInstance' smart constructor.
unassignInstance :: Text -> UnassignInstance
unassignInstance pInstanceId = UnassignInstance'{_unaInstanceId = pInstanceId};

-- | The instance ID.
unaInstanceId :: Lens' UnassignInstance Text
unaInstanceId = lens _unaInstanceId (\ s a -> s{_unaInstanceId = a});

instance AWSRequest UnassignInstance where
        type Sv UnassignInstance = OpsWorks
        type Rs UnassignInstance = UnassignInstanceResponse
        request = postJSON
        response = receiveNull UnassignInstanceResponse'

instance ToHeaders UnassignInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.UnassignInstance" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UnassignInstance where
        toJSON UnassignInstance'{..}
          = object ["InstanceId" .= _unaInstanceId]

instance ToPath UnassignInstance where
        toPath = const "/"

instance ToQuery UnassignInstance where
        toQuery = const mempty

-- | /See:/ 'unassignInstanceResponse' smart constructor.
data UnassignInstanceResponse = UnassignInstanceResponse' deriving (Eq, Read, Show)

-- | 'UnassignInstanceResponse' smart constructor.
unassignInstanceResponse :: UnassignInstanceResponse
unassignInstanceResponse = UnassignInstanceResponse';
