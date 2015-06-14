{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.OpsWorks.CreateApp
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

-- | Creates an app for a specified stack. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Creating Apps>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_CreateApp.html>
module Network.AWS.OpsWorks.CreateApp
    (
    -- * Request
      CreateApp
    -- ** Request constructor
    , createApp
    -- ** Request lenses
    , caSSLConfiguration
    , caShortname
    , caEnableSSL
    , caEnvironment
    , caDataSources
    , caAppSource
    , caAttributes
    , caDomains
    , caDescription
    , caStackId
    , caName
    , caType

    -- * Response
    , CreateAppResponse
    -- ** Response constructor
    , createAppResponse
    -- ** Response lenses
    , carAppId
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.OpsWorks.Types

-- | /See:/ 'createApp' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'caSSLConfiguration'
--
-- * 'caShortname'
--
-- * 'caEnableSSL'
--
-- * 'caEnvironment'
--
-- * 'caDataSources'
--
-- * 'caAppSource'
--
-- * 'caAttributes'
--
-- * 'caDomains'
--
-- * 'caDescription'
--
-- * 'caStackId'
--
-- * 'caName'
--
-- * 'caType'
data CreateApp = CreateApp'{_caSSLConfiguration :: Maybe SSLConfiguration, _caShortname :: Maybe Text, _caEnableSSL :: Maybe Bool, _caEnvironment :: [EnvironmentVariable], _caDataSources :: [DataSource], _caAppSource :: Maybe Source, _caAttributes :: HashMap AppAttributesKeys Text, _caDomains :: [Text], _caDescription :: Maybe Text, _caStackId :: Text, _caName :: Text, _caType :: AppType} deriving (Eq, Read, Show)

-- | 'CreateApp' smart constructor.
createApp :: Text -> Text -> AppType -> CreateApp
createApp pStackId pName pType' = CreateApp'{_caSSLConfiguration = Nothing, _caShortname = Nothing, _caEnableSSL = Nothing, _caEnvironment = mempty, _caDataSources = mempty, _caAppSource = Nothing, _caAttributes = mempty, _caDomains = mempty, _caDescription = Nothing, _caStackId = pStackId, _caName = pName, _caType = pType'};

-- | An @SslConfiguration@ object with the SSL configuration.
caSSLConfiguration :: Lens' CreateApp (Maybe SSLConfiguration)
caSSLConfiguration = lens _caSSLConfiguration (\ s a -> s{_caSSLConfiguration = a});

-- | The app\'s short name.
caShortname :: Lens' CreateApp (Maybe Text)
caShortname = lens _caShortname (\ s a -> s{_caShortname = a});

-- | Whether to enable SSL for the app.
caEnableSSL :: Lens' CreateApp (Maybe Bool)
caEnableSSL = lens _caEnableSSL (\ s a -> s{_caEnableSSL = a});

-- | An array of @EnvironmentVariable@ objects that specify environment
-- variables to be associated with the app. After you deploy the app, these
-- variables are defined on the associated app server instance. For more
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
caEnvironment :: Lens' CreateApp [EnvironmentVariable]
caEnvironment = lens _caEnvironment (\ s a -> s{_caEnvironment = a});

-- | The app\'s data source.
caDataSources :: Lens' CreateApp [DataSource]
caDataSources = lens _caDataSources (\ s a -> s{_caDataSources = a});

-- | A @Source@ object that specifies the app repository.
caAppSource :: Lens' CreateApp (Maybe Source)
caAppSource = lens _caAppSource (\ s a -> s{_caAppSource = a});

-- | One or more user-defined key\/value pairs to be added to the stack
-- attributes.
caAttributes :: Lens' CreateApp (HashMap AppAttributesKeys Text)
caAttributes = lens _caAttributes (\ s a -> s{_caAttributes = a}) . _Coerce;

-- | The app virtual host settings, with multiple domains separated by
-- commas. For example: @\'www.example.com, example.com\'@
caDomains :: Lens' CreateApp [Text]
caDomains = lens _caDomains (\ s a -> s{_caDomains = a});

-- | A description of the app.
caDescription :: Lens' CreateApp (Maybe Text)
caDescription = lens _caDescription (\ s a -> s{_caDescription = a});

-- | The stack ID.
caStackId :: Lens' CreateApp Text
caStackId = lens _caStackId (\ s a -> s{_caStackId = a});

-- | The app name.
caName :: Lens' CreateApp Text
caName = lens _caName (\ s a -> s{_caName = a});

-- | The app type. Each supported type is associated with a particular layer.
-- For example, PHP applications are associated with a PHP layer. AWS
-- OpsWorks deploys an application to those instances that are members of
-- the corresponding layer. If your app isn\'t one of the standard types,
-- or you prefer to implement your own Deploy recipes, specify @other@.
caType :: Lens' CreateApp AppType
caType = lens _caType (\ s a -> s{_caType = a});

instance AWSRequest CreateApp where
        type Sv CreateApp = OpsWorks
        type Rs CreateApp = CreateAppResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x -> CreateAppResponse' <$> x .?> "AppId")

instance ToHeaders CreateApp where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.CreateApp" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateApp where
        toJSON CreateApp'{..}
          = object
              ["SslConfiguration" .= _caSSLConfiguration,
               "Shortname" .= _caShortname,
               "EnableSsl" .= _caEnableSSL,
               "Environment" .= _caEnvironment,
               "DataSources" .= _caDataSources,
               "AppSource" .= _caAppSource,
               "Attributes" .= _caAttributes,
               "Domains" .= _caDomains,
               "Description" .= _caDescription,
               "StackId" .= _caStackId, "Name" .= _caName,
               "Type" .= _caType]

instance ToPath CreateApp where
        toPath = const "/"

instance ToQuery CreateApp where
        toQuery = const mempty

-- | /See:/ 'createAppResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'carAppId'
newtype CreateAppResponse = CreateAppResponse'{_carAppId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'CreateAppResponse' smart constructor.
createAppResponse :: CreateAppResponse
createAppResponse = CreateAppResponse'{_carAppId = Nothing};

-- | The app ID.
carAppId :: Lens' CreateAppResponse (Maybe Text)
carAppId = lens _carAppId (\ s a -> s{_carAppId = a});
