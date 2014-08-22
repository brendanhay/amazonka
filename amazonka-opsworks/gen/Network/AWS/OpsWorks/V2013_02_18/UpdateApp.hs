{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.UpdateApp
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates a specified app. Required Permissions: To use this action, an IAM
-- user must have a Deploy or Manage permissions level for the stack, or an
-- attached policy that explicitly grants permissions. For more information on
-- user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.UpdateApp where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

-- | Minimum specification for a 'UpdateApp' request.
updateApp :: Text -- ^ '_uarAppId'
          -> UpdateApp
updateApp p1 = UpdateApp
    { _uarAppId = p1
    , _uarAttributes = mempty
    , _uarType = Nothing
    , _uarEnableSsl = Nothing
    , _uarDataSources = mempty
    , _uarAppSource = Nothing
    , _uarSslConfiguration = Nothing
    , _uarName = Nothing
    , _uarDescription = Nothing
    , _uarDomains = mempty
    }

data UpdateApp = UpdateApp
    { _uarAppId :: Text
      -- ^ The app ID.
    , _uarAttributes :: Map AppAttributesKeys Text
      -- ^ One or more user-defined key/value pairs to be added to the stack
      -- attributes.
    , _uarType :: Maybe AppType
      -- ^ The app type.
    , _uarEnableSsl :: Maybe Bool
      -- ^ Whether SSL is enabled for the app.
    , _uarDataSources :: [DataSource]
      -- ^ The app's data sources.
    , _uarAppSource :: Maybe Source
      -- ^ A Source object that specifies the app repository.
    , _uarSslConfiguration :: Maybe SslConfiguration
      -- ^ An SslConfiguration object with the SSL configuration.
    , _uarName :: Maybe Text
      -- ^ The app name.
    , _uarDescription :: Maybe Text
      -- ^ A description of the app.
    , _uarDomains :: [Text]
      -- ^ The app's virtual host settings, with multiple domains separated
      -- by commas. For example: 'www.example.com, example.com'.
    } deriving (Show, Generic)

makeLenses ''UpdateApp

instance ToPath UpdateApp

instance ToQuery UpdateApp

instance ToHeaders UpdateApp

instance ToJSON UpdateApp

data UpdateAppResponse = UpdateAppResponse
    deriving (Eq, Show, Generic)

makeLenses ''UpdateAppResponse

instance AWSRequest UpdateApp where
    type Sv UpdateApp = OpsWorks
    type Rs UpdateApp = UpdateAppResponse

    request = get
    response _ = nullaryResponse UpdateAppResponse
