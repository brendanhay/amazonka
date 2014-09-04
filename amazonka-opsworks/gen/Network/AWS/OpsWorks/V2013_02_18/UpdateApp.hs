{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
module Network.AWS.OpsWorks.V2013_02_18.UpdateApp
    (
    -- * Request
      UpdateApp
    -- ** Request constructor
    , mkUpdateAppRequest
    -- ** Request lenses
    , uarAppId
    , uarName
    , uarDescription
    , uarDataSources
    , uarType
    , uarAppSource
    , uarDomains
    , uarEnableSsl
    , uarSslConfiguration
    , uarAttributes

    -- * Response
    , UpdateAppResponse
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateApp' request.
mkUpdateAppRequest :: Text -- ^ 'uarAppId'
                   -> UpdateApp
mkUpdateAppRequest p1 = UpdateApp
    { _uarAppId = p1
    , _uarName = Nothing
    , _uarDescription = Nothing
    , _uarDataSources = mempty
    , _uarType = Nothing
    , _uarAppSource = Nothing
    , _uarDomains = mempty
    , _uarEnableSsl = Nothing
    , _uarSslConfiguration = Nothing
    , _uarAttributes = mempty
    }
{-# INLINE mkUpdateAppRequest #-}

data UpdateApp = UpdateApp
    { _uarAppId :: Text
      -- ^ The app ID.
    , _uarName :: Maybe Text
      -- ^ The app name.
    , _uarDescription :: Maybe Text
      -- ^ A description of the app.
    , _uarDataSources :: [DataSource]
      -- ^ The app's data sources.
    , _uarType :: Maybe AppType
      -- ^ The app type.
    , _uarAppSource :: Maybe Source
      -- ^ A Source object that specifies the app repository.
    , _uarDomains :: [Text]
      -- ^ The app's virtual host settings, with multiple domains separated
      -- by commas. For example: 'www.example.com, example.com'.
    , _uarEnableSsl :: Maybe Bool
      -- ^ Whether SSL is enabled for the app.
    , _uarSslConfiguration :: Maybe SslConfiguration
      -- ^ An SslConfiguration object with the SSL configuration.
    , _uarAttributes :: Map AppAttributesKeys Text
      -- ^ One or more user-defined key/value pairs to be added to the stack
      -- attributes.
    } deriving (Show, Generic)

-- | The app ID.
uarAppId :: Lens' UpdateApp (Text)
uarAppId = lens _uarAppId (\s a -> s { _uarAppId = a })
{-# INLINE uarAppId #-}

-- | The app name.
uarName :: Lens' UpdateApp (Maybe Text)
uarName = lens _uarName (\s a -> s { _uarName = a })
{-# INLINE uarName #-}

-- | A description of the app.
uarDescription :: Lens' UpdateApp (Maybe Text)
uarDescription = lens _uarDescription (\s a -> s { _uarDescription = a })
{-# INLINE uarDescription #-}

-- | The app's data sources.
uarDataSources :: Lens' UpdateApp ([DataSource])
uarDataSources = lens _uarDataSources (\s a -> s { _uarDataSources = a })
{-# INLINE uarDataSources #-}

-- | The app type.
uarType :: Lens' UpdateApp (Maybe AppType)
uarType = lens _uarType (\s a -> s { _uarType = a })
{-# INLINE uarType #-}

-- | A Source object that specifies the app repository.
uarAppSource :: Lens' UpdateApp (Maybe Source)
uarAppSource = lens _uarAppSource (\s a -> s { _uarAppSource = a })
{-# INLINE uarAppSource #-}

-- | The app's virtual host settings, with multiple domains separated by commas.
-- For example: 'www.example.com, example.com'.
uarDomains :: Lens' UpdateApp ([Text])
uarDomains = lens _uarDomains (\s a -> s { _uarDomains = a })
{-# INLINE uarDomains #-}

-- | Whether SSL is enabled for the app.
uarEnableSsl :: Lens' UpdateApp (Maybe Bool)
uarEnableSsl = lens _uarEnableSsl (\s a -> s { _uarEnableSsl = a })
{-# INLINE uarEnableSsl #-}

-- | An SslConfiguration object with the SSL configuration.
uarSslConfiguration :: Lens' UpdateApp (Maybe SslConfiguration)
uarSslConfiguration = lens _uarSslConfiguration (\s a -> s { _uarSslConfiguration = a })
{-# INLINE uarSslConfiguration #-}

-- | One or more user-defined key/value pairs to be added to the stack
-- attributes.
uarAttributes :: Lens' UpdateApp (Map AppAttributesKeys Text)
uarAttributes = lens _uarAttributes (\s a -> s { _uarAttributes = a })
{-# INLINE uarAttributes #-}

instance ToPath UpdateApp

instance ToQuery UpdateApp

instance ToHeaders UpdateApp

instance ToJSON UpdateApp

data UpdateAppResponse = UpdateAppResponse
    deriving (Eq, Show, Generic)

instance AWSRequest UpdateApp where
    type Sv UpdateApp = OpsWorks
    type Rs UpdateApp = UpdateAppResponse

    request = get
    response _ = nullaryResponse UpdateAppResponse
