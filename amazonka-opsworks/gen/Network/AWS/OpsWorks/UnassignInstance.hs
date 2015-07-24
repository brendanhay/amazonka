{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.UnassignInstance
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Unassigns a registered instance from all of it\'s layers. The instance
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
    , uInstanceId

    -- * Response
    , UnassignInstanceResponse
    -- ** Response constructor
    , unassignInstanceResponse
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'unassignInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uInstanceId'
newtype UnassignInstance = UnassignInstance'
    { _uInstanceId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UnassignInstance' smart constructor.
unassignInstance :: Text -> UnassignInstance
unassignInstance pInstanceId_ =
    UnassignInstance'
    { _uInstanceId = pInstanceId_
    }

-- | The instance ID.
uInstanceId :: Lens' UnassignInstance Text
uInstanceId = lens _uInstanceId (\ s a -> s{_uInstanceId = a});

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
          = object ["InstanceId" .= _uInstanceId]

instance ToPath UnassignInstance where
        toPath = const "/"

instance ToQuery UnassignInstance where
        toQuery = const mempty

-- | /See:/ 'unassignInstanceResponse' smart constructor.
data UnassignInstanceResponse =
    UnassignInstanceResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UnassignInstanceResponse' smart constructor.
unassignInstanceResponse :: UnassignInstanceResponse
unassignInstanceResponse = UnassignInstanceResponse'
