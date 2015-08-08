{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DeleteApp
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified app.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- /See:/ <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DeleteApp.html AWS API Reference> for DeleteApp.
module Network.AWS.OpsWorks.DeleteApp
    (
    -- * Creating a Request
      DeleteApp
    , deleteApp
    -- * Request Lenses
    , daAppId

    -- * Destructuring the Response
    , DeleteAppResponse
    , deleteAppResponse
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteApp' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daAppId'
newtype DeleteApp = DeleteApp'
    { _daAppId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteApp' smart constructor.
deleteApp :: Text -> DeleteApp
deleteApp pAppId_ =
    DeleteApp'
    { _daAppId = pAppId_
    }

-- | The app ID.
daAppId :: Lens' DeleteApp Text
daAppId = lens _daAppId (\ s a -> s{_daAppId = a});

instance AWSRequest DeleteApp where
        type Sv DeleteApp = OpsWorks
        type Rs DeleteApp = DeleteAppResponse
        request = postJSON
        response = receiveNull DeleteAppResponse'

instance ToHeaders DeleteApp where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DeleteApp" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteApp where
        toJSON DeleteApp'{..} = object ["AppId" .= _daAppId]

instance ToPath DeleteApp where
        toPath = const "/"

instance ToQuery DeleteApp where
        toQuery = const mempty

-- | /See:/ 'deleteAppResponse' smart constructor.
data DeleteAppResponse =
    DeleteAppResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteAppResponse' smart constructor.
deleteAppResponse :: DeleteAppResponse
deleteAppResponse = DeleteAppResponse'
