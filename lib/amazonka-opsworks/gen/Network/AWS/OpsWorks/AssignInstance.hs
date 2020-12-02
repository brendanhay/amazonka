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
-- Module      : Network.AWS.OpsWorks.AssignInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assign a registered instance to a layer.
--
--
--     * You can assign registered on-premises instances to any layer type.
--
--     * You can assign registered Amazon EC2 instances only to custom layers.
--
--     * You cannot use this action with instances that were created with AWS OpsWorks Stacks.
--
--
--
-- __Required Permissions__ : To use this action, an AWS Identity and Access Management (IAM) user must have a Manage permissions level for the stack or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.AssignInstance
    (
    -- * Creating a Request
      assignInstance
    , AssignInstance
    -- * Request Lenses
    , aiInstanceId
    , aiLayerIds

    -- * Destructuring the Response
    , assignInstanceResponse
    , AssignInstanceResponse
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'assignInstance' smart constructor.
data AssignInstance = AssignInstance'
  { _aiInstanceId :: !Text
  , _aiLayerIds   :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssignInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiInstanceId' - The instance ID.
--
-- * 'aiLayerIds' - The layer ID, which must correspond to a custom layer. You cannot assign a registered instance to a built-in layer.
assignInstance
    :: Text -- ^ 'aiInstanceId'
    -> AssignInstance
assignInstance pInstanceId_ =
  AssignInstance' {_aiInstanceId = pInstanceId_, _aiLayerIds = mempty}


-- | The instance ID.
aiInstanceId :: Lens' AssignInstance Text
aiInstanceId = lens _aiInstanceId (\ s a -> s{_aiInstanceId = a})

-- | The layer ID, which must correspond to a custom layer. You cannot assign a registered instance to a built-in layer.
aiLayerIds :: Lens' AssignInstance [Text]
aiLayerIds = lens _aiLayerIds (\ s a -> s{_aiLayerIds = a}) . _Coerce

instance AWSRequest AssignInstance where
        type Rs AssignInstance = AssignInstanceResponse
        request = postJSON opsWorks
        response = receiveNull AssignInstanceResponse'

instance Hashable AssignInstance where

instance NFData AssignInstance where

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
              (catMaybes
                 [Just ("InstanceId" .= _aiInstanceId),
                  Just ("LayerIds" .= _aiLayerIds)])

instance ToPath AssignInstance where
        toPath = const "/"

instance ToQuery AssignInstance where
        toQuery = const mempty

-- | /See:/ 'assignInstanceResponse' smart constructor.
data AssignInstanceResponse =
  AssignInstanceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssignInstanceResponse' with the minimum fields required to make a request.
--
assignInstanceResponse
    :: AssignInstanceResponse
assignInstanceResponse = AssignInstanceResponse'


instance NFData AssignInstanceResponse where
