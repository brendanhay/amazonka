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
-- Module      : Network.AWS.SMS.GetAppLaunchConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the application launch configuration associated with the specified application.
module Network.AWS.SMS.GetAppLaunchConfiguration
  ( -- * Creating a Request
    getAppLaunchConfiguration,
    GetAppLaunchConfiguration,

    -- * Request Lenses
    galcAppId,

    -- * Destructuring the Response
    getAppLaunchConfigurationResponse,
    GetAppLaunchConfigurationResponse,

    -- * Response Lenses
    galcrsServerGroupLaunchConfigurations,
    galcrsAutoLaunch,
    galcrsRoleName,
    galcrsAppId,
    galcrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types

-- | /See:/ 'getAppLaunchConfiguration' smart constructor.
newtype GetAppLaunchConfiguration = GetAppLaunchConfiguration'
  { _galcAppId ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetAppLaunchConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'galcAppId' - The ID of the application.
getAppLaunchConfiguration ::
  GetAppLaunchConfiguration
getAppLaunchConfiguration =
  GetAppLaunchConfiguration' {_galcAppId = Nothing}

-- | The ID of the application.
galcAppId :: Lens' GetAppLaunchConfiguration (Maybe Text)
galcAppId = lens _galcAppId (\s a -> s {_galcAppId = a})

instance AWSRequest GetAppLaunchConfiguration where
  type
    Rs GetAppLaunchConfiguration =
      GetAppLaunchConfigurationResponse
  request = postJSON sms
  response =
    receiveJSON
      ( \s h x ->
          GetAppLaunchConfigurationResponse'
            <$> (x .?> "serverGroupLaunchConfigurations" .!@ mempty)
            <*> (x .?> "autoLaunch")
            <*> (x .?> "roleName")
            <*> (x .?> "appId")
            <*> (pure (fromEnum s))
      )

instance Hashable GetAppLaunchConfiguration

instance NFData GetAppLaunchConfiguration

instance ToHeaders GetAppLaunchConfiguration where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSServerMigrationService_V2016_10_24.GetAppLaunchConfiguration" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetAppLaunchConfiguration where
  toJSON GetAppLaunchConfiguration' {..} =
    object (catMaybes [("appId" .=) <$> _galcAppId])

instance ToPath GetAppLaunchConfiguration where
  toPath = const "/"

instance ToQuery GetAppLaunchConfiguration where
  toQuery = const mempty

-- | /See:/ 'getAppLaunchConfigurationResponse' smart constructor.
data GetAppLaunchConfigurationResponse = GetAppLaunchConfigurationResponse'
  { _galcrsServerGroupLaunchConfigurations ::
      !( Maybe
           [ServerGroupLaunchConfiguration]
       ),
    _galcrsAutoLaunch ::
      !(Maybe Bool),
    _galcrsRoleName ::
      !(Maybe Text),
    _galcrsAppId ::
      !(Maybe Text),
    _galcrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetAppLaunchConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'galcrsServerGroupLaunchConfigurations' - The launch configurations for server groups in this application.
--
-- * 'galcrsAutoLaunch' - Indicates whether the application is configured to launch automatically after replication is complete.
--
-- * 'galcrsRoleName' - The name of the service role in the customer's account that AWS CloudFormation uses to launch the application.
--
-- * 'galcrsAppId' - The ID of the application.
--
-- * 'galcrsResponseStatus' - -- | The response status code.
getAppLaunchConfigurationResponse ::
  -- | 'galcrsResponseStatus'
  Int ->
  GetAppLaunchConfigurationResponse
getAppLaunchConfigurationResponse pResponseStatus_ =
  GetAppLaunchConfigurationResponse'
    { _galcrsServerGroupLaunchConfigurations =
        Nothing,
      _galcrsAutoLaunch = Nothing,
      _galcrsRoleName = Nothing,
      _galcrsAppId = Nothing,
      _galcrsResponseStatus = pResponseStatus_
    }

-- | The launch configurations for server groups in this application.
galcrsServerGroupLaunchConfigurations :: Lens' GetAppLaunchConfigurationResponse [ServerGroupLaunchConfiguration]
galcrsServerGroupLaunchConfigurations = lens _galcrsServerGroupLaunchConfigurations (\s a -> s {_galcrsServerGroupLaunchConfigurations = a}) . _Default . _Coerce

-- | Indicates whether the application is configured to launch automatically after replication is complete.
galcrsAutoLaunch :: Lens' GetAppLaunchConfigurationResponse (Maybe Bool)
galcrsAutoLaunch = lens _galcrsAutoLaunch (\s a -> s {_galcrsAutoLaunch = a})

-- | The name of the service role in the customer's account that AWS CloudFormation uses to launch the application.
galcrsRoleName :: Lens' GetAppLaunchConfigurationResponse (Maybe Text)
galcrsRoleName = lens _galcrsRoleName (\s a -> s {_galcrsRoleName = a})

-- | The ID of the application.
galcrsAppId :: Lens' GetAppLaunchConfigurationResponse (Maybe Text)
galcrsAppId = lens _galcrsAppId (\s a -> s {_galcrsAppId = a})

-- | -- | The response status code.
galcrsResponseStatus :: Lens' GetAppLaunchConfigurationResponse Int
galcrsResponseStatus = lens _galcrsResponseStatus (\s a -> s {_galcrsResponseStatus = a})

instance NFData GetAppLaunchConfigurationResponse
