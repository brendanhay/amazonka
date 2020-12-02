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
-- Module      : Network.AWS.SMS.PutAppLaunchConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the launch configuration for the specified application.
module Network.AWS.SMS.PutAppLaunchConfiguration
  ( -- * Creating a Request
    putAppLaunchConfiguration,
    PutAppLaunchConfiguration,

    -- * Request Lenses
    palcServerGroupLaunchConfigurations,
    palcAutoLaunch,
    palcRoleName,
    palcAppId,

    -- * Destructuring the Response
    putAppLaunchConfigurationResponse,
    PutAppLaunchConfigurationResponse,

    -- * Response Lenses
    palcrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types

-- | /See:/ 'putAppLaunchConfiguration' smart constructor.
data PutAppLaunchConfiguration = PutAppLaunchConfiguration'
  { _palcServerGroupLaunchConfigurations ::
      !( Maybe
           [ServerGroupLaunchConfiguration]
       ),
    _palcAutoLaunch :: !(Maybe Bool),
    _palcRoleName :: !(Maybe Text),
    _palcAppId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutAppLaunchConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'palcServerGroupLaunchConfigurations' - Information about the launch configurations for server groups in the application.
--
-- * 'palcAutoLaunch' - Indicates whether the application is configured to launch automatically after replication is complete.
--
-- * 'palcRoleName' - The name of service role in the customer's account that AWS CloudFormation uses to launch the application.
--
-- * 'palcAppId' - The ID of the application.
putAppLaunchConfiguration ::
  PutAppLaunchConfiguration
putAppLaunchConfiguration =
  PutAppLaunchConfiguration'
    { _palcServerGroupLaunchConfigurations =
        Nothing,
      _palcAutoLaunch = Nothing,
      _palcRoleName = Nothing,
      _palcAppId = Nothing
    }

-- | Information about the launch configurations for server groups in the application.
palcServerGroupLaunchConfigurations :: Lens' PutAppLaunchConfiguration [ServerGroupLaunchConfiguration]
palcServerGroupLaunchConfigurations = lens _palcServerGroupLaunchConfigurations (\s a -> s {_palcServerGroupLaunchConfigurations = a}) . _Default . _Coerce

-- | Indicates whether the application is configured to launch automatically after replication is complete.
palcAutoLaunch :: Lens' PutAppLaunchConfiguration (Maybe Bool)
palcAutoLaunch = lens _palcAutoLaunch (\s a -> s {_palcAutoLaunch = a})

-- | The name of service role in the customer's account that AWS CloudFormation uses to launch the application.
palcRoleName :: Lens' PutAppLaunchConfiguration (Maybe Text)
palcRoleName = lens _palcRoleName (\s a -> s {_palcRoleName = a})

-- | The ID of the application.
palcAppId :: Lens' PutAppLaunchConfiguration (Maybe Text)
palcAppId = lens _palcAppId (\s a -> s {_palcAppId = a})

instance AWSRequest PutAppLaunchConfiguration where
  type
    Rs PutAppLaunchConfiguration =
      PutAppLaunchConfigurationResponse
  request = postJSON sms
  response =
    receiveEmpty
      ( \s h x ->
          PutAppLaunchConfigurationResponse' <$> (pure (fromEnum s))
      )

instance Hashable PutAppLaunchConfiguration

instance NFData PutAppLaunchConfiguration

instance ToHeaders PutAppLaunchConfiguration where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSServerMigrationService_V2016_10_24.PutAppLaunchConfiguration" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON PutAppLaunchConfiguration where
  toJSON PutAppLaunchConfiguration' {..} =
    object
      ( catMaybes
          [ ("serverGroupLaunchConfigurations" .=)
              <$> _palcServerGroupLaunchConfigurations,
            ("autoLaunch" .=) <$> _palcAutoLaunch,
            ("roleName" .=) <$> _palcRoleName,
            ("appId" .=) <$> _palcAppId
          ]
      )

instance ToPath PutAppLaunchConfiguration where
  toPath = const "/"

instance ToQuery PutAppLaunchConfiguration where
  toQuery = const mempty

-- | /See:/ 'putAppLaunchConfigurationResponse' smart constructor.
newtype PutAppLaunchConfigurationResponse = PutAppLaunchConfigurationResponse'
  { _palcrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutAppLaunchConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'palcrsResponseStatus' - -- | The response status code.
putAppLaunchConfigurationResponse ::
  -- | 'palcrsResponseStatus'
  Int ->
  PutAppLaunchConfigurationResponse
putAppLaunchConfigurationResponse pResponseStatus_ =
  PutAppLaunchConfigurationResponse'
    { _palcrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
palcrsResponseStatus :: Lens' PutAppLaunchConfigurationResponse Int
palcrsResponseStatus = lens _palcrsResponseStatus (\s a -> s {_palcrsResponseStatus = a})

instance NFData PutAppLaunchConfigurationResponse
