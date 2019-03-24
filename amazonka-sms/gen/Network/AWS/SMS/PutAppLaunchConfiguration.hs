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
-- Module      : Network.AWS.SMS.PutAppLaunchConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a launch configuration for an application.
--
--
module Network.AWS.SMS.PutAppLaunchConfiguration
    (
    -- * Creating a Request
      putAppLaunchConfiguration
    , PutAppLaunchConfiguration
    -- * Request Lenses
    , palcServerGroupLaunchConfigurations
    , palcRoleName
    , palcAppId

    -- * Destructuring the Response
    , putAppLaunchConfigurationResponse
    , PutAppLaunchConfigurationResponse
    -- * Response Lenses
    , palcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types
import Network.AWS.SMS.Types.Product

-- | /See:/ 'putAppLaunchConfiguration' smart constructor.
data PutAppLaunchConfiguration = PutAppLaunchConfiguration'
  { _palcServerGroupLaunchConfigurations :: !(Maybe [ServerGroupLaunchConfiguration])
  , _palcRoleName :: !(Maybe Text)
  , _palcAppId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutAppLaunchConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'palcServerGroupLaunchConfigurations' - Launch configurations for server groups in the application.
--
-- * 'palcRoleName' - Name of service role in the customer's account that Amazon CloudFormation uses to launch the application.
--
-- * 'palcAppId' - ID of the application associated with the launch configuration.
putAppLaunchConfiguration
    :: PutAppLaunchConfiguration
putAppLaunchConfiguration =
  PutAppLaunchConfiguration'
    { _palcServerGroupLaunchConfigurations = Nothing
    , _palcRoleName = Nothing
    , _palcAppId = Nothing
    }


-- | Launch configurations for server groups in the application.
palcServerGroupLaunchConfigurations :: Lens' PutAppLaunchConfiguration [ServerGroupLaunchConfiguration]
palcServerGroupLaunchConfigurations = lens _palcServerGroupLaunchConfigurations (\ s a -> s{_palcServerGroupLaunchConfigurations = a}) . _Default . _Coerce

-- | Name of service role in the customer's account that Amazon CloudFormation uses to launch the application.
palcRoleName :: Lens' PutAppLaunchConfiguration (Maybe Text)
palcRoleName = lens _palcRoleName (\ s a -> s{_palcRoleName = a})

-- | ID of the application associated with the launch configuration.
palcAppId :: Lens' PutAppLaunchConfiguration (Maybe Text)
palcAppId = lens _palcAppId (\ s a -> s{_palcAppId = a})

instance AWSRequest PutAppLaunchConfiguration where
        type Rs PutAppLaunchConfiguration =
             PutAppLaunchConfigurationResponse
        request = postJSON sms
        response
          = receiveEmpty
              (\ s h x ->
                 PutAppLaunchConfigurationResponse' <$>
                   (pure (fromEnum s)))

instance Hashable PutAppLaunchConfiguration where

instance NFData PutAppLaunchConfiguration where

instance ToHeaders PutAppLaunchConfiguration where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSServerMigrationService_V2016_10_24.PutAppLaunchConfiguration"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutAppLaunchConfiguration where
        toJSON PutAppLaunchConfiguration'{..}
          = object
              (catMaybes
                 [("serverGroupLaunchConfigurations" .=) <$>
                    _palcServerGroupLaunchConfigurations,
                  ("roleName" .=) <$> _palcRoleName,
                  ("appId" .=) <$> _palcAppId])

instance ToPath PutAppLaunchConfiguration where
        toPath = const "/"

instance ToQuery PutAppLaunchConfiguration where
        toQuery = const mempty

-- | /See:/ 'putAppLaunchConfigurationResponse' smart constructor.
newtype PutAppLaunchConfigurationResponse = PutAppLaunchConfigurationResponse'
  { _palcrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutAppLaunchConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'palcrsResponseStatus' - -- | The response status code.
putAppLaunchConfigurationResponse
    :: Int -- ^ 'palcrsResponseStatus'
    -> PutAppLaunchConfigurationResponse
putAppLaunchConfigurationResponse pResponseStatus_ =
  PutAppLaunchConfigurationResponse' {_palcrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
palcrsResponseStatus :: Lens' PutAppLaunchConfigurationResponse Int
palcrsResponseStatus = lens _palcrsResponseStatus (\ s a -> s{_palcrsResponseStatus = a})

instance NFData PutAppLaunchConfigurationResponse
         where
