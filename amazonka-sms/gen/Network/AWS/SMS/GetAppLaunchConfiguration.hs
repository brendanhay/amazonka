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
-- Module      : Network.AWS.SMS.GetAppLaunchConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the application launch configuration associated with an application.
--
--
module Network.AWS.SMS.GetAppLaunchConfiguration
    (
    -- * Creating a Request
      getAppLaunchConfiguration
    , GetAppLaunchConfiguration
    -- * Request Lenses
    , galcAppId

    -- * Destructuring the Response
    , getAppLaunchConfigurationResponse
    , GetAppLaunchConfigurationResponse
    -- * Response Lenses
    , galcrsServerGroupLaunchConfigurations
    , galcrsRoleName
    , galcrsAppId
    , galcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types
import Network.AWS.SMS.Types.Product

-- | /See:/ 'getAppLaunchConfiguration' smart constructor.
newtype GetAppLaunchConfiguration = GetAppLaunchConfiguration'
  { _galcAppId :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAppLaunchConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'galcAppId' - ID of the application launch configuration.
getAppLaunchConfiguration
    :: GetAppLaunchConfiguration
getAppLaunchConfiguration = GetAppLaunchConfiguration' {_galcAppId = Nothing}


-- | ID of the application launch configuration.
galcAppId :: Lens' GetAppLaunchConfiguration (Maybe Text)
galcAppId = lens _galcAppId (\ s a -> s{_galcAppId = a})

instance AWSRequest GetAppLaunchConfiguration where
        type Rs GetAppLaunchConfiguration =
             GetAppLaunchConfigurationResponse
        request = postJSON sms
        response
          = receiveJSON
              (\ s h x ->
                 GetAppLaunchConfigurationResponse' <$>
                   (x .?> "serverGroupLaunchConfigurations" .!@ mempty)
                     <*> (x .?> "roleName")
                     <*> (x .?> "appId")
                     <*> (pure (fromEnum s)))

instance Hashable GetAppLaunchConfiguration where

instance NFData GetAppLaunchConfiguration where

instance ToHeaders GetAppLaunchConfiguration where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSServerMigrationService_V2016_10_24.GetAppLaunchConfiguration"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetAppLaunchConfiguration where
        toJSON GetAppLaunchConfiguration'{..}
          = object (catMaybes [("appId" .=) <$> _galcAppId])

instance ToPath GetAppLaunchConfiguration where
        toPath = const "/"

instance ToQuery GetAppLaunchConfiguration where
        toQuery = const mempty

-- | /See:/ 'getAppLaunchConfigurationResponse' smart constructor.
data GetAppLaunchConfigurationResponse = GetAppLaunchConfigurationResponse'
  { _galcrsServerGroupLaunchConfigurations :: !(Maybe [ServerGroupLaunchConfiguration])
  , _galcrsRoleName :: !(Maybe Text)
  , _galcrsAppId :: !(Maybe Text)
  , _galcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAppLaunchConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'galcrsServerGroupLaunchConfigurations' - List of launch configurations for server groups in this application.
--
-- * 'galcrsRoleName' - Name of the service role in the customer's account that Amazon CloudFormation uses to launch the application.
--
-- * 'galcrsAppId' - ID of the application associated with the launch configuration.
--
-- * 'galcrsResponseStatus' - -- | The response status code.
getAppLaunchConfigurationResponse
    :: Int -- ^ 'galcrsResponseStatus'
    -> GetAppLaunchConfigurationResponse
getAppLaunchConfigurationResponse pResponseStatus_ =
  GetAppLaunchConfigurationResponse'
    { _galcrsServerGroupLaunchConfigurations = Nothing
    , _galcrsRoleName = Nothing
    , _galcrsAppId = Nothing
    , _galcrsResponseStatus = pResponseStatus_
    }


-- | List of launch configurations for server groups in this application.
galcrsServerGroupLaunchConfigurations :: Lens' GetAppLaunchConfigurationResponse [ServerGroupLaunchConfiguration]
galcrsServerGroupLaunchConfigurations = lens _galcrsServerGroupLaunchConfigurations (\ s a -> s{_galcrsServerGroupLaunchConfigurations = a}) . _Default . _Coerce

-- | Name of the service role in the customer's account that Amazon CloudFormation uses to launch the application.
galcrsRoleName :: Lens' GetAppLaunchConfigurationResponse (Maybe Text)
galcrsRoleName = lens _galcrsRoleName (\ s a -> s{_galcrsRoleName = a})

-- | ID of the application associated with the launch configuration.
galcrsAppId :: Lens' GetAppLaunchConfigurationResponse (Maybe Text)
galcrsAppId = lens _galcrsAppId (\ s a -> s{_galcrsAppId = a})

-- | -- | The response status code.
galcrsResponseStatus :: Lens' GetAppLaunchConfigurationResponse Int
galcrsResponseStatus = lens _galcrsResponseStatus (\ s a -> s{_galcrsResponseStatus = a})

instance NFData GetAppLaunchConfigurationResponse
         where
