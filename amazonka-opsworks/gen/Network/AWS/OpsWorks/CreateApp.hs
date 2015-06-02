{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.CreateApp
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates an app for a specified stack. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Creating Apps>.
--
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing UserPermissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_CreateApp.html>
module Network.AWS.OpsWorks.CreateApp
    (
    -- * Request
      CreateApp
    -- ** Request constructor
    , createApp
    -- ** Request lenses
    , caAppSource
    , caAttributes
    , caDataSources
    , caDescription
    , caDomains
    , caEnableSsl
    , caEnvironment
    , caName
    , caShortname
    , caSslConfiguration
    , caStackId
    , caType

    -- * Response
    , CreateAppResponse
    -- ** Response constructor
    , createAppResponse
    -- ** Response lenses
    , carAppId
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

data CreateApp = CreateApp
    { _caAppSource        :: Maybe Source
    , _caAttributes       :: Map AppAttributesKeys Text
    , _caDataSources      :: List "DataSources" DataSource
    , _caDescription      :: Maybe Text
    , _caDomains          :: List "Domains" Text
    , _caEnableSsl        :: Maybe Bool
    , _caEnvironment      :: List "Environment" EnvironmentVariable
    , _caName             :: Text
    , _caShortname        :: Maybe Text
    , _caSslConfiguration :: Maybe SslConfiguration
    , _caStackId          :: Text
    , _caType             :: AppType
    } deriving (Eq, Read, Show)

-- | 'CreateApp' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'caAppSource' @::@ 'Maybe' 'Source'
--
-- * 'caAttributes' @::@ 'HashMap' 'AppAttributesKeys' 'Text'
--
-- * 'caDataSources' @::@ ['DataSource']
--
-- * 'caDescription' @::@ 'Maybe' 'Text'
--
-- * 'caDomains' @::@ ['Text']
--
-- * 'caEnableSsl' @::@ 'Maybe' 'Bool'
--
-- * 'caEnvironment' @::@ ['EnvironmentVariable']
--
-- * 'caName' @::@ 'Text'
--
-- * 'caShortname' @::@ 'Maybe' 'Text'
--
-- * 'caSslConfiguration' @::@ 'Maybe' 'SslConfiguration'
--
-- * 'caStackId' @::@ 'Text'
--
-- * 'caType' @::@ 'AppType'
--
createApp :: Text -- ^ 'caStackId'
          -> Text -- ^ 'caName'
          -> AppType -- ^ 'caType'
          -> CreateApp
createApp p1 p2 p3 = CreateApp
    { _caStackId          = p1
    , _caName             = p2
    , _caType             = p3
    , _caShortname        = Nothing
    , _caDescription      = Nothing
    , _caDataSources      = mempty
    , _caAppSource        = Nothing
    , _caDomains          = mempty
    , _caEnableSsl        = Nothing
    , _caSslConfiguration = Nothing
    , _caAttributes       = mempty
    , _caEnvironment      = mempty
    }

-- | A 'Source' object that specifies the app repository.
caAppSource :: Lens' CreateApp (Maybe Source)
caAppSource = lens _caAppSource (\s a -> s { _caAppSource = a })

-- | One or more user-defined key/value pairs to be added to the stack attributes.
caAttributes :: Lens' CreateApp (HashMap AppAttributesKeys Text)
caAttributes = lens _caAttributes (\s a -> s { _caAttributes = a }) . _Map

-- | The app's data source.
caDataSources :: Lens' CreateApp [DataSource]
caDataSources = lens _caDataSources (\s a -> s { _caDataSources = a }) . _List

-- | A description of the app.
caDescription :: Lens' CreateApp (Maybe Text)
caDescription = lens _caDescription (\s a -> s { _caDescription = a })

-- | The app virtual host settings, with multiple domains separated by commas. For
-- example: ''www.example.com, example.com''
caDomains :: Lens' CreateApp [Text]
caDomains = lens _caDomains (\s a -> s { _caDomains = a }) . _List

-- | Whether to enable SSL for the app.
caEnableSsl :: Lens' CreateApp (Maybe Bool)
caEnableSsl = lens _caEnableSsl (\s a -> s { _caEnableSsl = a })

-- | An array of 'EnvironmentVariable' objects that specify environment variables to
-- be associated with the app. After you deploy the app, these variables are
-- defined on the associated app server instance. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables>.
--
-- There is no specific limit on the number of environment variables. However,
-- the size of the associated data structure - which includes the variables'
-- names, values, and protected flag values - cannot exceed 10 KB (10240 Bytes).
-- This limit should accommodate most if not all use cases. Exceeding it will
-- cause an exception with the message, "Environment: is too large (maximum is
-- 10KB)."
--
-- This parameter is supported only by Chef 11.10 stacks. If you have specified
-- one or more environment variables, you cannot modify the stack's Chef version.
caEnvironment :: Lens' CreateApp [EnvironmentVariable]
caEnvironment = lens _caEnvironment (\s a -> s { _caEnvironment = a }) . _List

-- | The app name.
caName :: Lens' CreateApp Text
caName = lens _caName (\s a -> s { _caName = a })

-- | The app's short name.
caShortname :: Lens' CreateApp (Maybe Text)
caShortname = lens _caShortname (\s a -> s { _caShortname = a })

-- | An 'SslConfiguration' object with the SSL configuration.
caSslConfiguration :: Lens' CreateApp (Maybe SslConfiguration)
caSslConfiguration =
    lens _caSslConfiguration (\s a -> s { _caSslConfiguration = a })

-- | The stack ID.
caStackId :: Lens' CreateApp Text
caStackId = lens _caStackId (\s a -> s { _caStackId = a })

-- | The app type. Each supported type is associated with a particular layer. For
-- example, PHP applications are associated with a PHP layer. AWS OpsWorks
-- deploys an application to those instances that are members of the
-- corresponding layer. If your app isn't one of the standard types, or you
-- prefer to implement your own Deploy recipes, specify 'other'.
caType :: Lens' CreateApp AppType
caType = lens _caType (\s a -> s { _caType = a })

newtype CreateAppResponse = CreateAppResponse
    { _carAppId :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'CreateAppResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'carAppId' @::@ 'Maybe' 'Text'
--
createAppResponse :: CreateAppResponse
createAppResponse = CreateAppResponse
    { _carAppId = Nothing
    }

-- | The app ID.
carAppId :: Lens' CreateAppResponse (Maybe Text)
carAppId = lens _carAppId (\s a -> s { _carAppId = a })

instance ToPath CreateApp where
    toPath = const "/"

instance ToQuery CreateApp where
    toQuery = const mempty

instance ToHeaders CreateApp

instance ToJSON CreateApp where
    toJSON CreateApp{..} = object
        [ "StackId"          .= _caStackId
        , "Shortname"        .= _caShortname
        , "Name"             .= _caName
        , "Description"      .= _caDescription
        , "DataSources"      .= _caDataSources
        , "Type"             .= _caType
        , "AppSource"        .= _caAppSource
        , "Domains"          .= _caDomains
        , "EnableSsl"        .= _caEnableSsl
        , "SslConfiguration" .= _caSslConfiguration
        , "Attributes"       .= _caAttributes
        , "Environment"      .= _caEnvironment
        ]

instance AWSRequest CreateApp where
    type Sv CreateApp = OpsWorks
    type Rs CreateApp = CreateAppResponse

    request  = post "CreateApp"
    response = jsonResponse

instance FromJSON CreateAppResponse where
    parseJSON = withObject "CreateAppResponse" $ \o -> CreateAppResponse
        <$> o .:? "AppId"
