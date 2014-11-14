{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.OpsWorks.CreateApp
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an app for a specified stack. For more information, see Creating
-- Apps. Required Permissions: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see Managing User Permissions.
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

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.OpsWorks.Types

data CreateApp = CreateApp
    { _caAppSource        :: Maybe Source
    , _caAttributes       :: Map Text Text
    , _caDataSources      :: [DataSource]
    , _caDescription      :: Maybe Text
    , _caDomains          :: [Text]
    , _caEnableSsl        :: Maybe Bool
    , _caName             :: Text
    , _caShortname        :: Maybe Text
    , _caSslConfiguration :: Maybe SslConfiguration
    , _caStackId          :: Text
    , _caType             :: Text
    } deriving (Eq, Show, Generic)

-- | 'CreateApp' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'caAppSource' @::@ 'Maybe' 'Source'
--
-- * 'caAttributes' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'caDataSources' @::@ ['DataSource']
--
-- * 'caDescription' @::@ 'Maybe' 'Text'
--
-- * 'caDomains' @::@ ['Text']
--
-- * 'caEnableSsl' @::@ 'Maybe' 'Bool'
--
-- * 'caName' @::@ 'Text'
--
-- * 'caShortname' @::@ 'Maybe' 'Text'
--
-- * 'caSslConfiguration' @::@ 'Maybe' 'SslConfiguration'
--
-- * 'caStackId' @::@ 'Text'
--
-- * 'caType' @::@ 'Text'
--
createApp :: Text -- ^ 'caStackId'
          -> Text -- ^ 'caName'
          -> Text -- ^ 'caType'
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
    }

-- | A Source object that specifies the app repository.
caAppSource :: Lens' CreateApp (Maybe Source)
caAppSource = lens _caAppSource (\s a -> s { _caAppSource = a })

-- | One or more user-defined key/value pairs to be added to the stack
-- attributes.
caAttributes :: Lens' CreateApp (HashMap Text Text)
caAttributes = lens _caAttributes (\s a -> s { _caAttributes = a })
    . _Map

-- | The app's data source.
caDataSources :: Lens' CreateApp [DataSource]
caDataSources = lens _caDataSources (\s a -> s { _caDataSources = a })

-- | A description of the app.
caDescription :: Lens' CreateApp (Maybe Text)
caDescription = lens _caDescription (\s a -> s { _caDescription = a })

-- | The app virtual host settings, with multiple domains separated by commas.
-- For example: 'www.example.com, example.com'.
caDomains :: Lens' CreateApp [Text]
caDomains = lens _caDomains (\s a -> s { _caDomains = a })

-- | Whether to enable SSL for the app.
caEnableSsl :: Lens' CreateApp (Maybe Bool)
caEnableSsl = lens _caEnableSsl (\s a -> s { _caEnableSsl = a })

-- | The app name.
caName :: Lens' CreateApp Text
caName = lens _caName (\s a -> s { _caName = a })

-- | The app's short name.
caShortname :: Lens' CreateApp (Maybe Text)
caShortname = lens _caShortname (\s a -> s { _caShortname = a })

-- | An SslConfiguration object with the SSL configuration.
caSslConfiguration :: Lens' CreateApp (Maybe SslConfiguration)
caSslConfiguration =
    lens _caSslConfiguration (\s a -> s { _caSslConfiguration = a })

-- | The stack ID.
caStackId :: Lens' CreateApp Text
caStackId = lens _caStackId (\s a -> s { _caStackId = a })

-- | The app type. Each supported type is associated with a particular layer.
-- For example, PHP applications are associated with a PHP layer. AWS
-- OpsWorks deploys an application to those instances that are members of
-- the corresponding layer.
caType :: Lens' CreateApp Text
caType = lens _caType (\s a -> s { _caType = a })

instance ToPath CreateApp where
    toPath = const "/"

instance ToQuery CreateApp where
    toQuery = const mempty

instance ToHeaders CreateApp

instance ToBody CreateApp where
    toBody = toBody . encode . _caStackId

newtype CreateAppResponse = CreateAppResponse
    { _carAppId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

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

instance AWSRequest CreateApp where
    type Sv CreateApp = OpsWorks
    type Rs CreateApp = CreateAppResponse

    request  = post
    response = jsonResponse $ \h o -> CreateAppResponse
        <$> o .: "AppId"
