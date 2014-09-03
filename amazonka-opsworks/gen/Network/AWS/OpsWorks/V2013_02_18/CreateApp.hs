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
carType
    :: Functor f
    => (AppType
    -> f (AppType))
    -> CreateApp
    -> f CreateApp
carType f x =
    (\y -> x { _carType = y })
       <$> f (_carType x)
{-# INLINE carType #-}

-- | The stack ID.
carStackId
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateApp
    -> f CreateApp
carStackId f x =
    (\y -> x { _carStackId = y })
       <$> f (_carStackId x)
{-# INLINE carStackId #-}

-- | The app name.
carName
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateApp
    -> f CreateApp
carName f x =
    (\y -> x { _carName = y })
       <$> f (_carName x)
{-# INLINE carName #-}

-- | One or more user-defined key/value pairs to be added to the stack
-- attributes.
carAttributes
    :: Functor f
    => (Map AppAttributesKeys Text
    -> f (Map AppAttributesKeys Text))
    -> CreateApp
    -> f CreateApp
carAttributes f x =
    (\y -> x { _carAttributes = y })
       <$> f (_carAttributes x)
{-# INLINE carAttributes #-}

-- | Whether to enable SSL for the app.
carEnableSsl
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> CreateApp
    -> f CreateApp
carEnableSsl f x =
    (\y -> x { _carEnableSsl = y })
       <$> f (_carEnableSsl x)
{-# INLINE carEnableSsl #-}

-- | The app's data source.
carDataSources
    :: Functor f
    => ([DataSource]
    -> f ([DataSource]))
    -> CreateApp
    -> f CreateApp
carDataSources f x =
    (\y -> x { _carDataSources = y })
       <$> f (_carDataSources x)
{-# INLINE carDataSources #-}

-- | A Source object that specifies the app repository.
carAppSource
    :: Functor f
    => (Maybe Source
    -> f (Maybe Source))
    -> CreateApp
    -> f CreateApp
carAppSource f x =
    (\y -> x { _carAppSource = y })
       <$> f (_carAppSource x)
{-# INLINE carAppSource #-}

-- | An SslConfiguration object with the SSL configuration.
carSslConfiguration
    :: Functor f
    => (Maybe SslConfiguration
    -> f (Maybe SslConfiguration))
    -> CreateApp
    -> f CreateApp
carSslConfiguration f x =
    (\y -> x { _carSslConfiguration = y })
       <$> f (_carSslConfiguration x)
{-# INLINE carSslConfiguration #-}

-- | The app's short name.
carShortname
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateApp
    -> f CreateApp
carShortname f x =
    (\y -> x { _carShortname = y })
       <$> f (_carShortname x)
{-# INLINE carShortname #-}

-- | A description of the app.
carDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateApp
    -> f CreateApp
carDescription f x =
    (\y -> x { _carDescription = y })
       <$> f (_carDescription x)
{-# INLINE carDescription #-}

-- | The app virtual host settings, with multiple domains separated by commas.
-- For example: 'www.example.com, example.com'.
carDomains
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> CreateApp
    -> f CreateApp
carDomains f x =
    (\y -> x { _carDomains = y })
       <$> f (_carDomains x)
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
casAppId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateAppResponse
    -> f CreateAppResponse
casAppId f x =
    (\y -> x { _casAppId = y })
       <$> f (_casAppId x)
{-# INLINE casAppId #-}

instance FromJSON CreateAppResponse

instance AWSRequest CreateApp where
    type Sv CreateApp = OpsWorks
    type Rs CreateApp = CreateAppResponse

    request = get
    response _ = jsonResponse
