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
-- Module      : Network.AWS.OpsWorks.UnassignInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unassigns a registered instance from all of it's layers. The instance remains in the stack as an unassigned instance and can be assigned to another layer, as needed. You cannot use this action with instances that were created with AWS OpsWorks Stacks.
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.UnassignInstance
    (
    -- * Creating a Request
      unassignInstance
    , UnassignInstance
    -- * Request Lenses
    , uInstanceId

    -- * Destructuring the Response
    , unassignInstanceResponse
    , UnassignInstanceResponse
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'unassignInstance' smart constructor.
newtype UnassignInstance = UnassignInstance'
  { _uInstanceId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnassignInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uInstanceId' - The instance ID.
unassignInstance
    :: Text -- ^ 'uInstanceId'
    -> UnassignInstance
unassignInstance pInstanceId_ = UnassignInstance' {_uInstanceId = pInstanceId_}


-- | The instance ID.
uInstanceId :: Lens' UnassignInstance Text
uInstanceId = lens _uInstanceId (\ s a -> s{_uInstanceId = a})

instance AWSRequest UnassignInstance where
        type Rs UnassignInstance = UnassignInstanceResponse
        request = postJSON opsWorks
        response = receiveNull UnassignInstanceResponse'

instance Hashable UnassignInstance where

instance NFData UnassignInstance where

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
          = object
              (catMaybes [Just ("InstanceId" .= _uInstanceId)])

instance ToPath UnassignInstance where
        toPath = const "/"

instance ToQuery UnassignInstance where
        toQuery = const mempty

-- | /See:/ 'unassignInstanceResponse' smart constructor.
data UnassignInstanceResponse =
  UnassignInstanceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnassignInstanceResponse' with the minimum fields required to make a request.
--
unassignInstanceResponse
    :: UnassignInstanceResponse
unassignInstanceResponse = UnassignInstanceResponse'


instance NFData UnassignInstanceResponse where
