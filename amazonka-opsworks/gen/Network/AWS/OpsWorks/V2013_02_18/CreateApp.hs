{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.CreateApp
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
module Network.AWS.OpsWorks.V2013_02_18.CreateApp
    (
    -- * Request
      CreateApp
    -- ** Request constructor
    , createApp
    -- ** Request lenses
    , carType
    , carStackId
    , carName
    , carAttributes
    , carEnableSsl
    , carDataSources
    , carAppSource
    , carSslConfiguration
    , carShortname
    , carDescription
    , carDomains

    -- * Response
    , CreateAppResponse
    -- ** Response lenses
    , casAppId
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'CreateApp' request.
createApp :: AppType -- ^ 'carType'
          -> Text -- ^ 'carStackId'
          -> Text -- ^ 'carName'
          -> CreateApp
createApp p1 p2 p3 = CreateApp
    { _carType = p1
    , _carStackId = p2
    , _carName = p3
    , _carAttributes = mempty
    , _carEnableSsl = Nothing
    , _carDataSources = mempty
    , _carAppSource = Nothing
    , _carSslConfiguration = Nothing
    , _carShortname = Nothing
    , _carDescription = Nothing
    , _carDomains = mempty
    }
{-# INLINE createApp #-}

data CreateApp = CreateApp
    { _carType :: AppType
      -- ^ The app type. Each supported type is associated with a particular
      -- layer. For example, PHP applications are associated with a PHP
      -- layer. AWS OpsWorks deploys an application to those instances
      -- that are members of the corresponding layer.
    , _carStackId :: Text
      -- ^ The stack ID.
    , _carName :: Text
      -- ^ The app name.
    , _carAttributes :: Map AppAttributesKeys Text
      -- ^ One or more user-defined key/value pairs to be added to the stack
      -- attributes.
    , _carEnableSsl :: Maybe Bool
      -- ^ Whether to enable SSL for the app.
    , _carDataSources :: [DataSource]
      -- ^ The app's data source.
    , _carAppSource :: Maybe Source
      -- ^ A Source object that specifies the app repository.
    , _carSslConfiguration :: Maybe SslConfiguration
      -- ^ An SslConfiguration object with the SSL configuration.
    , _carShortname :: Maybe Text
      -- ^ The app's short name.
    , _carDescription :: Maybe Text
      -- ^ A description of the app.
    , _carDomains :: [Text]
      -- ^ The app virtual host settings, with multiple domains separated by
      -- commas. For example: 'www.example.com, example.com'.
    } deriving (Show, Generic)

-- | The app type. Each supported type is associated with a particular layer.
-- For example, PHP applications are associated with a PHP layer. AWS OpsWorks
-- deploys an application to those instances that are members of the
-- corresponding layer.
carType :: Lens' CreateApp (AppType)
carType f x =
    f (_carType x)
        <&> \y -> x { _carType = y }
{-# INLINE carType #-}

-- | The stack ID.
carStackId :: Lens' CreateApp (Text)
carStackId f x =
    f (_carStackId x)
        <&> \y -> x { _carStackId = y }
{-# INLINE carStackId #-}

-- | The app name.
carName :: Lens' CreateApp (Text)
carName f x =
    f (_carName x)
        <&> \y -> x { _carName = y }
{-# INLINE carName #-}

-- | One or more user-defined key/value pairs to be added to the stack
-- attributes.
carAttributes :: Lens' CreateApp (Map AppAttributesKeys Text)
carAttributes f x =
    f (_carAttributes x)
        <&> \y -> x { _carAttributes = y }
{-# INLINE carAttributes #-}

-- | Whether to enable SSL for the app.
carEnableSsl :: Lens' CreateApp (Maybe Bool)
carEnableSsl f x =
    f (_carEnableSsl x)
        <&> \y -> x { _carEnableSsl = y }
{-# INLINE carEnableSsl #-}

-- | The app's data source.
carDataSources :: Lens' CreateApp ([DataSource])
carDataSources f x =
    f (_carDataSources x)
        <&> \y -> x { _carDataSources = y }
{-# INLINE carDataSources #-}

-- | A Source object that specifies the app repository.
carAppSource :: Lens' CreateApp (Maybe Source)
carAppSource f x =
    f (_carAppSource x)
        <&> \y -> x { _carAppSource = y }
{-# INLINE carAppSource #-}

-- | An SslConfiguration object with the SSL configuration.
carSslConfiguration :: Lens' CreateApp (Maybe SslConfiguration)
carSslConfiguration f x =
    f (_carSslConfiguration x)
        <&> \y -> x { _carSslConfiguration = y }
{-# INLINE carSslConfiguration #-}

-- | The app's short name.
carShortname :: Lens' CreateApp (Maybe Text)
carShortname f x =
    f (_carShortname x)
        <&> \y -> x { _carShortname = y }
{-# INLINE carShortname #-}

-- | A description of the app.
carDescription :: Lens' CreateApp (Maybe Text)
carDescription f x =
    f (_carDescription x)
        <&> \y -> x { _carDescription = y }
{-# INLINE carDescription #-}

-- | The app virtual host settings, with multiple domains separated by commas.
-- For example: 'www.example.com, example.com'.
carDomains :: Lens' CreateApp ([Text])
carDomains f x =
    f (_carDomains x)
        <&> \y -> x { _carDomains = y }
{-# INLINE carDomains #-}

instance ToPath CreateApp

instance ToQuery CreateApp

instance ToHeaders CreateApp

instance ToJSON CreateApp

data CreateAppResponse = CreateAppResponse
    { _casAppId :: Maybe Text
      -- ^ The app ID.
    } deriving (Show, Generic)

-- | The app ID.
casAppId :: Lens' CreateAppResponse (Maybe Text)
casAppId f x =
    f (_casAppId x)
        <&> \y -> x { _casAppId = y }
{-# INLINE casAppId #-}

instance FromJSON CreateAppResponse

instance AWSRequest CreateApp where
    type Sv CreateApp = OpsWorks
    type Rs CreateApp = CreateAppResponse

    request = get
    response _ = jsonResponse
