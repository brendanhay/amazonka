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
-- Module      : Network.AWS.SMS.DeleteAppLaunchConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the launch configuration for the specified application.
module Network.AWS.SMS.DeleteAppLaunchConfiguration
  ( -- * Creating a Request
    deleteAppLaunchConfiguration,
    DeleteAppLaunchConfiguration,

    -- * Request Lenses
    dalcAppId,

    -- * Destructuring the Response
    deleteAppLaunchConfigurationResponse,
    DeleteAppLaunchConfigurationResponse,

    -- * Response Lenses
    dalcrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types

-- | /See:/ 'deleteAppLaunchConfiguration' smart constructor.
newtype DeleteAppLaunchConfiguration = DeleteAppLaunchConfiguration'
  { _dalcAppId ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAppLaunchConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dalcAppId' - The ID of the application.
deleteAppLaunchConfiguration ::
  DeleteAppLaunchConfiguration
deleteAppLaunchConfiguration =
  DeleteAppLaunchConfiguration' {_dalcAppId = Nothing}

-- | The ID of the application.
dalcAppId :: Lens' DeleteAppLaunchConfiguration (Maybe Text)
dalcAppId = lens _dalcAppId (\s a -> s {_dalcAppId = a})

instance AWSRequest DeleteAppLaunchConfiguration where
  type
    Rs DeleteAppLaunchConfiguration =
      DeleteAppLaunchConfigurationResponse
  request = postJSON sms
  response =
    receiveEmpty
      ( \s h x ->
          DeleteAppLaunchConfigurationResponse' <$> (pure (fromEnum s))
      )

instance Hashable DeleteAppLaunchConfiguration

instance NFData DeleteAppLaunchConfiguration

instance ToHeaders DeleteAppLaunchConfiguration where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSServerMigrationService_V2016_10_24.DeleteAppLaunchConfiguration" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteAppLaunchConfiguration where
  toJSON DeleteAppLaunchConfiguration' {..} =
    object (catMaybes [("appId" .=) <$> _dalcAppId])

instance ToPath DeleteAppLaunchConfiguration where
  toPath = const "/"

instance ToQuery DeleteAppLaunchConfiguration where
  toQuery = const mempty

-- | /See:/ 'deleteAppLaunchConfigurationResponse' smart constructor.
newtype DeleteAppLaunchConfigurationResponse = DeleteAppLaunchConfigurationResponse'
  { _dalcrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAppLaunchConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dalcrsResponseStatus' - -- | The response status code.
deleteAppLaunchConfigurationResponse ::
  -- | 'dalcrsResponseStatus'
  Int ->
  DeleteAppLaunchConfigurationResponse
deleteAppLaunchConfigurationResponse pResponseStatus_ =
  DeleteAppLaunchConfigurationResponse'
    { _dalcrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dalcrsResponseStatus :: Lens' DeleteAppLaunchConfigurationResponse Int
dalcrsResponseStatus = lens _dalcrsResponseStatus (\s a -> s {_dalcrsResponseStatus = a})

instance NFData DeleteAppLaunchConfigurationResponse
