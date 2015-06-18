{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.OpsWorks.UpdateApp
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Updates a specified app.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Deploy or Manage permissions level for the stack, or an attached policy
-- that explicitly grants permissions. For more information on user
-- permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_UpdateApp.html>
module Network.AWS.OpsWorks.UpdateApp
    (
    -- * Request
      UpdateApp
    -- ** Request constructor
    , updateApp
    -- ** Request lenses
    , uaSSLConfiguration
    , uaEnableSSL
    , uaEnvironment
    , uaDataSources
    , uaAppSource
    , uaName
    , uaAttributes
    , uaType
    , uaDomains
    , uaDescription
    , uaAppId

    -- * Response
    , UpdateAppResponse
    -- ** Response constructor
    , updateAppResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.OpsWorks.Types

-- | /See:/ 'updateApp' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uaSSLConfiguration'
--
-- * 'uaEnableSSL'
--
-- * 'uaEnvironment'
--
-- * 'uaDataSources'
--
-- * 'uaAppSource'
--
-- * 'uaName'
--
-- * 'uaAttributes'
--
-- * 'uaType'
--
-- * 'uaDomains'
--
-- * 'uaDescription'
--
-- * 'uaAppId'
data UpdateApp = UpdateApp'{_uaSSLConfiguration :: Maybe SSLConfiguration, _uaEnableSSL :: Maybe Bool, _uaEnvironment :: Maybe [EnvironmentVariable], _uaDataSources :: Maybe [DataSource], _uaAppSource :: Maybe Source, _uaName :: Maybe Text, _uaAttributes :: Maybe (Map AppAttributesKeys Text), _uaType :: Maybe AppType, _uaDomains :: Maybe [Text], _uaDescription :: Maybe Text, _uaAppId :: Text} deriving (Eq, Read, Show)

-- | 'UpdateApp' smart constructor.
updateApp :: Text -> UpdateApp
updateApp pAppId = UpdateApp'{_uaSSLConfiguration = Nothing, _uaEnableSSL = Nothing, _uaEnvironment = Nothing, _uaDataSources = Nothing, _uaAppSource = Nothing, _uaName = Nothing, _uaAttributes = Nothing, _uaType = Nothing, _uaDomains = Nothing, _uaDescription = Nothing, _uaAppId = pAppId};

-- | An @SslConfiguration@ object with the SSL configuration.
uaSSLConfiguration :: Lens' UpdateApp (Maybe SSLConfiguration)
uaSSLConfiguration = lens _uaSSLConfiguration (\ s a -> s{_uaSSLConfiguration = a});

-- | Whether SSL is enabled for the app.
uaEnableSSL :: Lens' UpdateApp (Maybe Bool)
uaEnableSSL = lens _uaEnableSSL (\ s a -> s{_uaEnableSSL = a});

-- | An array of @EnvironmentVariable@ objects that specify environment
-- variables to be associated with the app. After you deploy the app, these
-- variables are defined on the associated app server instances.For more
-- information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables>.
--
-- There is no specific limit on the number of environment variables.
-- However, the size of the associated data structure - which includes the
-- variables\' names, values, and protected flag values - cannot exceed 10
-- KB (10240 Bytes). This limit should accommodate most if not all use
-- cases. Exceeding it will cause an exception with the message,
-- \"Environment: is too large (maximum is 10KB).\"
--
-- This parameter is supported only by Chef 11.10 stacks. If you have
-- specified one or more environment variables, you cannot modify the
-- stack\'s Chef version.
uaEnvironment :: Lens' UpdateApp [EnvironmentVariable]
uaEnvironment = lens _uaEnvironment (\ s a -> s{_uaEnvironment = a}) . _Default;

-- | The app\'s data sources.
uaDataSources :: Lens' UpdateApp [DataSource]
uaDataSources = lens _uaDataSources (\ s a -> s{_uaDataSources = a}) . _Default;

-- | A @Source@ object that specifies the app repository.
uaAppSource :: Lens' UpdateApp (Maybe Source)
uaAppSource = lens _uaAppSource (\ s a -> s{_uaAppSource = a});

-- | The app name.
uaName :: Lens' UpdateApp (Maybe Text)
uaName = lens _uaName (\ s a -> s{_uaName = a});

-- | One or more user-defined key\/value pairs to be added to the stack
-- attributes.
uaAttributes :: Lens' UpdateApp (HashMap AppAttributesKeys Text)
uaAttributes = lens _uaAttributes (\ s a -> s{_uaAttributes = a}) . _Default . _Map;

-- | The app type.
uaType :: Lens' UpdateApp (Maybe AppType)
uaType = lens _uaType (\ s a -> s{_uaType = a});

-- | The app\'s virtual host settings, with multiple domains separated by
-- commas. For example: @\'www.example.com, example.com\'@
uaDomains :: Lens' UpdateApp [Text]
uaDomains = lens _uaDomains (\ s a -> s{_uaDomains = a}) . _Default;

-- | A description of the app.
uaDescription :: Lens' UpdateApp (Maybe Text)
uaDescription = lens _uaDescription (\ s a -> s{_uaDescription = a});

-- | The app ID.
uaAppId :: Lens' UpdateApp Text
uaAppId = lens _uaAppId (\ s a -> s{_uaAppId = a});

instance AWSRequest UpdateApp where
        type Sv UpdateApp = OpsWorks
        type Rs UpdateApp = UpdateAppResponse
        request = postJSON
        response = receiveNull UpdateAppResponse'

instance ToHeaders UpdateApp where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.UpdateApp" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateApp where
        toJSON UpdateApp'{..}
          = object
              ["SslConfiguration" .= _uaSSLConfiguration,
               "EnableSsl" .= _uaEnableSSL,
               "Environment" .= _uaEnvironment,
               "DataSources" .= _uaDataSources,
               "AppSource" .= _uaAppSource, "Name" .= _uaName,
               "Attributes" .= _uaAttributes, "Type" .= _uaType,
               "Domains" .= _uaDomains,
               "Description" .= _uaDescription, "AppId" .= _uaAppId]

instance ToPath UpdateApp where
        toPath = const "/"

instance ToQuery UpdateApp where
        toQuery = const mempty

-- | /See:/ 'updateAppResponse' smart constructor.
data UpdateAppResponse = UpdateAppResponse' deriving (Eq, Read, Show)

-- | 'UpdateAppResponse' smart constructor.
updateAppResponse :: UpdateAppResponse
updateAppResponse = UpdateAppResponse';
