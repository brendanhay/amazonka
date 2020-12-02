{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.DeleteApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified application. Optionally deletes the launched stack associated with the application and all AWS SMS replication jobs for servers in the application.
module Network.AWS.SMS.DeleteApp
  ( -- * Creating a Request
    deleteApp,
    DeleteApp,

    -- * Request Lenses
    daForceTerminateApp,
    daAppId,
    daForceStopAppReplication,

    -- * Destructuring the Response
    deleteAppResponse,
    DeleteAppResponse,

    -- * Response Lenses
    darsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types

-- | /See:/ 'deleteApp' smart constructor.
data DeleteApp = DeleteApp'
  { _daForceTerminateApp :: !(Maybe Bool),
    _daAppId :: !(Maybe Text),
    _daForceStopAppReplication :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteApp' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daForceTerminateApp' - Indicates whether to terminate the stack corresponding to the application while deleting the application.
--
-- * 'daAppId' - The ID of the application.
--
-- * 'daForceStopAppReplication' - Indicates whether to stop all replication jobs corresponding to the servers in the application while deleting the application.
deleteApp ::
  DeleteApp
deleteApp =
  DeleteApp'
    { _daForceTerminateApp = Nothing,
      _daAppId = Nothing,
      _daForceStopAppReplication = Nothing
    }

-- | Indicates whether to terminate the stack corresponding to the application while deleting the application.
daForceTerminateApp :: Lens' DeleteApp (Maybe Bool)
daForceTerminateApp = lens _daForceTerminateApp (\s a -> s {_daForceTerminateApp = a})

-- | The ID of the application.
daAppId :: Lens' DeleteApp (Maybe Text)
daAppId = lens _daAppId (\s a -> s {_daAppId = a})

-- | Indicates whether to stop all replication jobs corresponding to the servers in the application while deleting the application.
daForceStopAppReplication :: Lens' DeleteApp (Maybe Bool)
daForceStopAppReplication = lens _daForceStopAppReplication (\s a -> s {_daForceStopAppReplication = a})

instance AWSRequest DeleteApp where
  type Rs DeleteApp = DeleteAppResponse
  request = postJSON sms
  response =
    receiveEmpty
      (\s h x -> DeleteAppResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteApp

instance NFData DeleteApp

instance ToHeaders DeleteApp where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSServerMigrationService_V2016_10_24.DeleteApp" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteApp where
  toJSON DeleteApp' {..} =
    object
      ( catMaybes
          [ ("forceTerminateApp" .=) <$> _daForceTerminateApp,
            ("appId" .=) <$> _daAppId,
            ("forceStopAppReplication" .=) <$> _daForceStopAppReplication
          ]
      )

instance ToPath DeleteApp where
  toPath = const "/"

instance ToQuery DeleteApp where
  toQuery = const mempty

-- | /See:/ 'deleteAppResponse' smart constructor.
newtype DeleteAppResponse = DeleteAppResponse'
  { _darsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAppResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darsResponseStatus' - -- | The response status code.
deleteAppResponse ::
  -- | 'darsResponseStatus'
  Int ->
  DeleteAppResponse
deleteAppResponse pResponseStatus_ =
  DeleteAppResponse' {_darsResponseStatus = pResponseStatus_}

-- | -- | The response status code.
darsResponseStatus :: Lens' DeleteAppResponse Int
darsResponseStatus = lens _darsResponseStatus (\s a -> s {_darsResponseStatus = a})

instance NFData DeleteAppResponse
