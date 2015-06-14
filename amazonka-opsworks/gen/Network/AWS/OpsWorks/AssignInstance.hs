{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.OpsWorks.AssignInstance
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

-- | Assign a registered instance to a layer.
--
-- -   You can assign registered on-premises instances to any layer type.
-- -   You can assign registered Amazon EC2 instances only to custom
--     layers.
-- -   You cannot use this action with instances that were created with AWS
--     OpsWorks.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_AssignInstance.html>
module Network.AWS.OpsWorks.AssignInstance
    (
    -- * Request
      AssignInstance
    -- ** Request constructor
    , assignInstance
    -- ** Request lenses
    , aiInstanceId
    , aiLayerIds

    -- * Response
    , AssignInstanceResponse
    -- ** Response constructor
    , assignInstanceResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.OpsWorks.Types

-- | /See:/ 'assignInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aiInstanceId'
--
-- * 'aiLayerIds'
data AssignInstance = AssignInstance'{_aiInstanceId :: Text, _aiLayerIds :: [Text]} deriving (Eq, Read, Show)

-- | 'AssignInstance' smart constructor.
assignInstance :: Text -> [Text] -> AssignInstance
assignInstance pInstanceId pLayerIds = AssignInstance'{_aiInstanceId = pInstanceId, _aiLayerIds = pLayerIds};

-- | The instance ID.
aiInstanceId :: Lens' AssignInstance Text
aiInstanceId = lens _aiInstanceId (\ s a -> s{_aiInstanceId = a});

-- | The layer ID, which must correspond to a custom layer. You cannot assign
-- a registered instance to a built-in layer.
aiLayerIds :: Lens' AssignInstance [Text]
aiLayerIds = lens _aiLayerIds (\ s a -> s{_aiLayerIds = a});

instance AWSRequest AssignInstance where
        type Sv AssignInstance = OpsWorks
        type Rs AssignInstance = AssignInstanceResponse
        request = postJSON
        response = receiveNull AssignInstanceResponse'

instance ToHeaders AssignInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.AssignInstance" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AssignInstance where
        toJSON AssignInstance'{..}
          = object
              ["InstanceId" .= _aiInstanceId,
               "LayerIds" .= _aiLayerIds]

instance ToPath AssignInstance where
        toPath = const "/"

instance ToQuery AssignInstance where
        toQuery = const mempty

-- | /See:/ 'assignInstanceResponse' smart constructor.
data AssignInstanceResponse = AssignInstanceResponse' deriving (Eq, Read, Show)

-- | 'AssignInstanceResponse' smart constructor.
assignInstanceResponse :: AssignInstanceResponse
assignInstanceResponse = AssignInstanceResponse';
