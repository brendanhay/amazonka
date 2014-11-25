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

-- Module      : Network.AWS.OpsWorks.UpdateApp
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates a specified app.
--
-- Required Permissions: To use this action, an IAM user must have a Deploy or
-- Manage permissions level for the stack, or an attached policy that explicitly
-- grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html ManagingUser Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_UpdateApp.html>
module Network.AWS.OpsWorks.UpdateApp
    (
    -- * Request
      UpdateApp
    -- ** Request constructor
    , updateApp
    -- ** Request lenses
    , uaAppId
    , uaAppSource
    , uaAttributes
    , uaDataSources
    , uaDescription
    , uaDomains
    , uaEnableSsl
    , uaName
    , uaSslConfiguration
    , uaType

    -- * Response
    , UpdateAppResponse
    -- ** Response constructor
    , updateAppResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

data UpdateApp = UpdateApp
    { _uaAppId            :: Text
    , _uaAppSource        :: Maybe Source
    , _uaAttributes       :: Map AppAttributesKeys Text
    , _uaDataSources      :: List "DataSources" DataSource
    , _uaDescription      :: Maybe Text
    , _uaDomains          :: List "InstanceIds" Text
    , _uaEnableSsl        :: Maybe Bool
    , _uaName             :: Maybe Text
    , _uaSslConfiguration :: Maybe SslConfiguration
    , _uaType             :: Maybe AppType
    } deriving (Eq, Show)

-- | 'UpdateApp' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uaAppId' @::@ 'Text'
--
-- * 'uaAppSource' @::@ 'Maybe' 'Source'
--
-- * 'uaAttributes' @::@ 'HashMap' 'AppAttributesKeys' 'Text'
--
-- * 'uaDataSources' @::@ ['DataSource']
--
-- * 'uaDescription' @::@ 'Maybe' 'Text'
--
-- * 'uaDomains' @::@ ['Text']
--
-- * 'uaEnableSsl' @::@ 'Maybe' 'Bool'
--
-- * 'uaName' @::@ 'Maybe' 'Text'
--
-- * 'uaSslConfiguration' @::@ 'Maybe' 'SslConfiguration'
--
-- * 'uaType' @::@ 'Maybe' 'AppType'
--
updateApp :: Text -- ^ 'uaAppId'
          -> UpdateApp
updateApp p1 = UpdateApp
    { _uaAppId            = p1
    , _uaName             = Nothing
    , _uaDescription      = Nothing
    , _uaDataSources      = mempty
    , _uaType             = Nothing
    , _uaAppSource        = Nothing
    , _uaDomains          = mempty
    , _uaEnableSsl        = Nothing
    , _uaSslConfiguration = Nothing
    , _uaAttributes       = mempty
    }

-- | The app ID.
uaAppId :: Lens' UpdateApp Text
uaAppId = lens _uaAppId (\s a -> s { _uaAppId = a })

-- | A 'Source' object that specifies the app repository.
uaAppSource :: Lens' UpdateApp (Maybe Source)
uaAppSource = lens _uaAppSource (\s a -> s { _uaAppSource = a })

-- | One or more user-defined key/value pairs to be added to the stack attributes.
uaAttributes :: Lens' UpdateApp (HashMap AppAttributesKeys Text)
uaAttributes = lens _uaAttributes (\s a -> s { _uaAttributes = a }) . _Map

-- | The app's data sources.
uaDataSources :: Lens' UpdateApp [DataSource]
uaDataSources = lens _uaDataSources (\s a -> s { _uaDataSources = a }) . _List

-- | A description of the app.
uaDescription :: Lens' UpdateApp (Maybe Text)
uaDescription = lens _uaDescription (\s a -> s { _uaDescription = a })

-- | The app's virtual host settings, with multiple domains separated by commas.
-- For example: ''www.example.com, example.com''
uaDomains :: Lens' UpdateApp [Text]
uaDomains = lens _uaDomains (\s a -> s { _uaDomains = a }) . _List

-- | Whether SSL is enabled for the app.
uaEnableSsl :: Lens' UpdateApp (Maybe Bool)
uaEnableSsl = lens _uaEnableSsl (\s a -> s { _uaEnableSsl = a })

-- | The app name.
uaName :: Lens' UpdateApp (Maybe Text)
uaName = lens _uaName (\s a -> s { _uaName = a })

-- | An 'SslConfiguration' object with the SSL configuration.
uaSslConfiguration :: Lens' UpdateApp (Maybe SslConfiguration)
uaSslConfiguration =
    lens _uaSslConfiguration (\s a -> s { _uaSslConfiguration = a })

-- | The app type.
uaType :: Lens' UpdateApp (Maybe AppType)
uaType = lens _uaType (\s a -> s { _uaType = a })

data UpdateAppResponse = UpdateAppResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'UpdateAppResponse' constructor.
updateAppResponse :: UpdateAppResponse
updateAppResponse = UpdateAppResponse

instance ToPath UpdateApp where
    toPath = const "/"

instance ToQuery UpdateApp where
    toQuery = const mempty

instance ToHeaders UpdateApp

instance ToJSON UpdateApp where
    toJSON UpdateApp{..} = object
        [ "AppId"            .= _uaAppId
        , "Name"             .= _uaName
        , "Description"      .= _uaDescription
        , "DataSources"      .= _uaDataSources
        , "Type"             .= _uaType
        , "AppSource"        .= _uaAppSource
        , "Domains"          .= _uaDomains
        , "EnableSsl"        .= _uaEnableSsl
        , "SslConfiguration" .= _uaSslConfiguration
        , "Attributes"       .= _uaAttributes
        ]

instance AWSRequest UpdateApp where
    type Sv UpdateApp = OpsWorks
    type Rs UpdateApp = UpdateAppResponse

    request  = post "UpdateApp"
    response = nullResponse UpdateAppResponse
