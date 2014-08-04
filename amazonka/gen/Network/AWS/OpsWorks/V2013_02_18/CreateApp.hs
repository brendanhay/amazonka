{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.OpsWorks.V2013_02_18.CreateApp where

import Control.Lens
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateApp' request.
createApp :: AppType -- ^ '_carType'
          -> Text -- ^ '_carName'
          -> Text -- ^ '_carStackId'
          -> CreateApp
createApp p1 p2 p3 = CreateApp
    { _carType = p1
    , _carName = p2
    , _carStackId = p3
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
    , _carName :: Text
      -- ^ The app name.
    , _carStackId :: Text
      -- ^ The stack ID.
    , _carAttributes :: HashMap AppAttributesKeys Text
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
    } deriving (Generic)

makeLenses ''CreateApp

instance ToPath CreateApp

instance ToQuery CreateApp

instance ToHeaders CreateApp

instance ToJSON CreateApp

data CreateAppResponse = CreateAppResponse
    { _casAppId :: Maybe Text
      -- ^ The app ID.
    } deriving (Generic)

makeLenses ''CreateAppResponse

instance FromJSON CreateAppResponse

instance AWSRequest CreateApp where
    type Sv CreateApp = OpsWorks
    type Rs CreateApp = CreateAppResponse

    request = get
    response _ = jsonResponse
