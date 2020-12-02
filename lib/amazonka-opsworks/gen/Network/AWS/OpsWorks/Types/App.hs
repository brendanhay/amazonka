{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.App
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.App where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types.AppAttributesKeys
import Network.AWS.OpsWorks.Types.AppType
import Network.AWS.OpsWorks.Types.DataSource
import Network.AWS.OpsWorks.Types.EnvironmentVariable
import Network.AWS.OpsWorks.Types.SSLConfiguration
import Network.AWS.OpsWorks.Types.Source
import Network.AWS.Prelude

-- | A description of the app.
--
--
--
-- /See:/ 'app' smart constructor.
data App = App'
  { _appSSLConfiguration :: !(Maybe SSLConfiguration),
    _appEnvironment :: !(Maybe [EnvironmentVariable]),
    _appEnableSSL :: !(Maybe Bool),
    _appCreatedAt :: !(Maybe Text),
    _appShortname :: !(Maybe Text),
    _appDataSources :: !(Maybe [DataSource]),
    _appAppSource :: !(Maybe Source),
    _appAppId :: !(Maybe Text),
    _appAttributes :: !(Maybe (Map AppAttributesKeys (Text))),
    _appName :: !(Maybe Text),
    _appType :: !(Maybe AppType),
    _appStackId :: !(Maybe Text),
    _appDomains :: !(Maybe [Text]),
    _appDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'App' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'appSSLConfiguration' - An @SslConfiguration@ object with the SSL configuration.
--
-- * 'appEnvironment' - An array of @EnvironmentVariable@ objects that specify environment variables to be associated with the app. After you deploy the app, these variables are defined on the associated app server instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables> .
--
-- * 'appEnableSSL' - Whether to enable SSL for the app.
--
-- * 'appCreatedAt' - When the app was created.
--
-- * 'appShortname' - The app's short name.
--
-- * 'appDataSources' - The app's data sources.
--
-- * 'appAppSource' - A @Source@ object that describes the app repository.
--
-- * 'appAppId' - The app ID.
--
-- * 'appAttributes' - The stack attributes.
--
-- * 'appName' - The app name.
--
-- * 'appType' - The app type.
--
-- * 'appStackId' - The app stack ID.
--
-- * 'appDomains' - The app vhost settings with multiple domains separated by commas. For example: @'www.example.com, example.com'@
--
-- * 'appDescription' - A description of the app.
app ::
  App
app =
  App'
    { _appSSLConfiguration = Nothing,
      _appEnvironment = Nothing,
      _appEnableSSL = Nothing,
      _appCreatedAt = Nothing,
      _appShortname = Nothing,
      _appDataSources = Nothing,
      _appAppSource = Nothing,
      _appAppId = Nothing,
      _appAttributes = Nothing,
      _appName = Nothing,
      _appType = Nothing,
      _appStackId = Nothing,
      _appDomains = Nothing,
      _appDescription = Nothing
    }

-- | An @SslConfiguration@ object with the SSL configuration.
appSSLConfiguration :: Lens' App (Maybe SSLConfiguration)
appSSLConfiguration = lens _appSSLConfiguration (\s a -> s {_appSSLConfiguration = a})

-- | An array of @EnvironmentVariable@ objects that specify environment variables to be associated with the app. After you deploy the app, these variables are defined on the associated app server instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables> .
appEnvironment :: Lens' App [EnvironmentVariable]
appEnvironment = lens _appEnvironment (\s a -> s {_appEnvironment = a}) . _Default . _Coerce

-- | Whether to enable SSL for the app.
appEnableSSL :: Lens' App (Maybe Bool)
appEnableSSL = lens _appEnableSSL (\s a -> s {_appEnableSSL = a})

-- | When the app was created.
appCreatedAt :: Lens' App (Maybe Text)
appCreatedAt = lens _appCreatedAt (\s a -> s {_appCreatedAt = a})

-- | The app's short name.
appShortname :: Lens' App (Maybe Text)
appShortname = lens _appShortname (\s a -> s {_appShortname = a})

-- | The app's data sources.
appDataSources :: Lens' App [DataSource]
appDataSources = lens _appDataSources (\s a -> s {_appDataSources = a}) . _Default . _Coerce

-- | A @Source@ object that describes the app repository.
appAppSource :: Lens' App (Maybe Source)
appAppSource = lens _appAppSource (\s a -> s {_appAppSource = a})

-- | The app ID.
appAppId :: Lens' App (Maybe Text)
appAppId = lens _appAppId (\s a -> s {_appAppId = a})

-- | The stack attributes.
appAttributes :: Lens' App (HashMap AppAttributesKeys (Text))
appAttributes = lens _appAttributes (\s a -> s {_appAttributes = a}) . _Default . _Map

-- | The app name.
appName :: Lens' App (Maybe Text)
appName = lens _appName (\s a -> s {_appName = a})

-- | The app type.
appType :: Lens' App (Maybe AppType)
appType = lens _appType (\s a -> s {_appType = a})

-- | The app stack ID.
appStackId :: Lens' App (Maybe Text)
appStackId = lens _appStackId (\s a -> s {_appStackId = a})

-- | The app vhost settings with multiple domains separated by commas. For example: @'www.example.com, example.com'@
appDomains :: Lens' App [Text]
appDomains = lens _appDomains (\s a -> s {_appDomains = a}) . _Default . _Coerce

-- | A description of the app.
appDescription :: Lens' App (Maybe Text)
appDescription = lens _appDescription (\s a -> s {_appDescription = a})

instance FromJSON App where
  parseJSON =
    withObject
      "App"
      ( \x ->
          App'
            <$> (x .:? "SslConfiguration")
            <*> (x .:? "Environment" .!= mempty)
            <*> (x .:? "EnableSsl")
            <*> (x .:? "CreatedAt")
            <*> (x .:? "Shortname")
            <*> (x .:? "DataSources" .!= mempty)
            <*> (x .:? "AppSource")
            <*> (x .:? "AppId")
            <*> (x .:? "Attributes" .!= mempty)
            <*> (x .:? "Name")
            <*> (x .:? "Type")
            <*> (x .:? "StackId")
            <*> (x .:? "Domains" .!= mempty)
            <*> (x .:? "Description")
      )

instance Hashable App

instance NFData App
