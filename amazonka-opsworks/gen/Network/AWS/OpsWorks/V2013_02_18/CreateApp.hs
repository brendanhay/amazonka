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
    , mkCreateAppRequest
    -- ** Request lenses
    , carStackId
    , carShortname
    , carName
    , carDescription
    , carDataSources
    , carType
    , carAppSource
    , carDomains
    , carEnableSsl
    , carSslConfiguration
    , carAttributes

    -- * Response
    , CreateAppResponse
    -- ** Response lenses
    , casAppId
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateApp' request.
mkCreateAppRequest :: Text -- ^ 'carStackId'
                   -> Text -- ^ 'carName'
                   -> AppType -- ^ 'carType'
                   -> CreateApp
mkCreateAppRequest p1 p2 p3 = CreateApp
    { _carStackId = p1
    , _carShortname = Nothing
    , _carName = p3
    , _carDescription = Nothing
    , _carDataSources = mempty
    , _carType = p6
    , _carAppSource = Nothing
    , _carDomains = mempty
    , _carEnableSsl = Nothing
    , _carSslConfiguration = Nothing
    , _carAttributes = mempty
    }
{-# INLINE mkCreateAppRequest #-}

data CreateApp = CreateApp
    { _carStackId :: Text
      -- ^ The stack ID.
    , _carShortname :: Maybe Text
      -- ^ The app's short name.
    , _carName :: Text
      -- ^ The app name.
    , _carDescription :: Maybe Text
      -- ^ A description of the app.
    , _carDataSources :: [DataSource]
      -- ^ The app's data source.
    , _carType :: AppType
      -- ^ The app type. Each supported type is associated with a particular
      -- layer. For example, PHP applications are associated with a PHP
      -- layer. AWS OpsWorks deploys an application to those instances
      -- that are members of the corresponding layer.
    , _carAppSource :: Maybe Source
      -- ^ A Source object that specifies the app repository.
    , _carDomains :: [Text]
      -- ^ The app virtual host settings, with multiple domains separated by
      -- commas. For example: 'www.example.com, example.com'.
    , _carEnableSsl :: Maybe Bool
      -- ^ Whether to enable SSL for the app.
    , _carSslConfiguration :: Maybe SslConfiguration
      -- ^ An SslConfiguration object with the SSL configuration.
    , _carAttributes :: Map AppAttributesKeys Text
      -- ^ One or more user-defined key/value pairs to be added to the stack
      -- attributes.
    } deriving (Show, Generic)

-- | The stack ID.
carStackId :: Lens' CreateApp (Text)
carStackId = lens _carStackId (\s a -> s { _carStackId = a })
{-# INLINE carStackId #-}

-- | The app's short name.
carShortname :: Lens' CreateApp (Maybe Text)
carShortname = lens _carShortname (\s a -> s { _carShortname = a })
{-# INLINE carShortname #-}

-- | The app name.
carName :: Lens' CreateApp (Text)
carName = lens _carName (\s a -> s { _carName = a })
{-# INLINE carName #-}

-- | A description of the app.
carDescription :: Lens' CreateApp (Maybe Text)
carDescription = lens _carDescription (\s a -> s { _carDescription = a })
{-# INLINE carDescription #-}

-- | The app's data source.
carDataSources :: Lens' CreateApp ([DataSource])
carDataSources = lens _carDataSources (\s a -> s { _carDataSources = a })
{-# INLINE carDataSources #-}

-- | The app type. Each supported type is associated with a particular layer.
-- For example, PHP applications are associated with a PHP layer. AWS OpsWorks
-- deploys an application to those instances that are members of the
-- corresponding layer.
carType :: Lens' CreateApp (AppType)
carType = lens _carType (\s a -> s { _carType = a })
{-# INLINE carType #-}

-- | A Source object that specifies the app repository.
carAppSource :: Lens' CreateApp (Maybe Source)
carAppSource = lens _carAppSource (\s a -> s { _carAppSource = a })
{-# INLINE carAppSource #-}

-- | The app virtual host settings, with multiple domains separated by commas.
-- For example: 'www.example.com, example.com'.
carDomains :: Lens' CreateApp ([Text])
carDomains = lens _carDomains (\s a -> s { _carDomains = a })
{-# INLINE carDomains #-}

-- | Whether to enable SSL for the app.
carEnableSsl :: Lens' CreateApp (Maybe Bool)
carEnableSsl = lens _carEnableSsl (\s a -> s { _carEnableSsl = a })
{-# INLINE carEnableSsl #-}

-- | An SslConfiguration object with the SSL configuration.
carSslConfiguration :: Lens' CreateApp (Maybe SslConfiguration)
carSslConfiguration = lens _carSslConfiguration (\s a -> s { _carSslConfiguration = a })
{-# INLINE carSslConfiguration #-}

-- | One or more user-defined key/value pairs to be added to the stack
-- attributes.
carAttributes :: Lens' CreateApp (Map AppAttributesKeys Text)
carAttributes = lens _carAttributes (\s a -> s { _carAttributes = a })
{-# INLINE carAttributes #-}

instance ToPath CreateApp

instance ToQuery CreateApp

instance ToHeaders CreateApp

instance ToJSON CreateApp

newtype CreateAppResponse = CreateAppResponse
    { _casAppId :: Maybe Text
      -- ^ The app ID.
    } deriving (Show, Generic)

-- | The app ID.
casAppId :: Lens' CreateAppResponse (Maybe Text)
casAppId = lens _casAppId (\s a -> s { _casAppId = a })
{-# INLINE casAppId #-}

instance FromJSON CreateAppResponse

instance AWSRequest CreateApp where
    type Sv CreateApp = OpsWorks
    type Rs CreateApp = CreateAppResponse

    request = get
    response _ = jsonResponse
