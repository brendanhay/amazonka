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
-- Module      : Network.AWS.OpsWorks.DeleteApp
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified app.
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.DeleteApp
    (
    -- * Creating a Request
      deleteApp
    , DeleteApp
    -- * Request Lenses
    , daAppId

    -- * Destructuring the Response
    , deleteAppResponse
    , DeleteAppResponse
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteApp' smart constructor.
newtype DeleteApp = DeleteApp'
  { _daAppId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteApp' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daAppId' - The app ID.
deleteApp
    :: Text -- ^ 'daAppId'
    -> DeleteApp
deleteApp pAppId_ = DeleteApp' {_daAppId = pAppId_}


-- | The app ID.
daAppId :: Lens' DeleteApp Text
daAppId = lens _daAppId (\ s a -> s{_daAppId = a})

instance AWSRequest DeleteApp where
        type Rs DeleteApp = DeleteAppResponse
        request = postJSON opsWorks
        response = receiveNull DeleteAppResponse'

instance Hashable DeleteApp where

instance NFData DeleteApp where

instance ToHeaders DeleteApp where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DeleteApp" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteApp where
        toJSON DeleteApp'{..}
          = object (catMaybes [Just ("AppId" .= _daAppId)])

instance ToPath DeleteApp where
        toPath = const "/"

instance ToQuery DeleteApp where
        toQuery = const mempty

-- | /See:/ 'deleteAppResponse' smart constructor.
data DeleteAppResponse =
  DeleteAppResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAppResponse' with the minimum fields required to make a request.
--
deleteAppResponse
    :: DeleteAppResponse
deleteAppResponse = DeleteAppResponse'


instance NFData DeleteAppResponse where
