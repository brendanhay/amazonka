{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
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

-- | Updates a specified app. Required Permissions: To use this action, an IAM
-- user must have a Deploy or Manage permissions level for the stack, or an
-- attached policy that explicitly grants permissions. For more information on
-- user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.UpdateApp
    (
    -- * Request
      UpdateApp
    -- ** Request constructor
    , updateApp
    -- ** Request lenses
    , uaAppId
    , uaName
    , uaDescription
    , uaDataSources
    , uaType
    , uaAppSource
    , uaDomains
    , uaEnableSsl
    , uaSslConfiguration
    , uaAttributes
    , uaEnvironment

    -- * Response
    , UpdateAppResponse
    -- ** Response constructor
    , updateAppResponse
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data UpdateApp = UpdateApp
    { _uaAppId :: Text
    , _uaName :: Maybe Text
    , _uaDescription :: Maybe Text
    , _uaDataSources :: [DataSource]
    , _uaType :: Maybe AppType
    , _uaAppSource :: Maybe Source'
    , _uaDomains :: [Text]
    , _uaEnableSsl :: Maybe Bool
    , _uaSslConfiguration :: Maybe SslConfiguration
    , _uaAttributes :: Map AppAttributesKeys Text
    , _uaEnvironment :: [EnvironmentVariable]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateApp' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AppId ::@ @Text@
--
-- * @Name ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @DataSources ::@ @[DataSource]@
--
-- * @Type ::@ @Maybe AppType@
--
-- * @AppSource ::@ @Maybe Source'@
--
-- * @Domains ::@ @[Text]@
--
-- * @EnableSsl ::@ @Maybe Bool@
--
-- * @SslConfiguration ::@ @Maybe SslConfiguration@
--
-- * @Attributes ::@ @Map AppAttributesKeys Text@
--
-- * @Environment ::@ @[EnvironmentVariable]@
--
updateApp :: Text -- ^ 'uaAppId'
          -> UpdateApp
updateApp p1 = UpdateApp
    { _uaAppId = p1
    , _uaName = Nothing
    , _uaDescription = Nothing
    , _uaDataSources = mempty
    , _uaType = Nothing
    , _uaAppSource = Nothing
    , _uaDomains = mempty
    , _uaEnableSsl = Nothing
    , _uaSslConfiguration = Nothing
    , _uaAttributes = mempty
    , _uaEnvironment = mempty
    }

-- | The app ID.
uaAppId :: Lens' UpdateApp Text
uaAppId = lens _uaAppId (\s a -> s { _uaAppId = a })

-- | The app name.
uaName :: Lens' UpdateApp (Maybe Text)
uaName = lens _uaName (\s a -> s { _uaName = a })

-- | A description of the app.
uaDescription :: Lens' UpdateApp (Maybe Text)
uaDescription = lens _uaDescription (\s a -> s { _uaDescription = a })

-- | The app's data sources.
uaDataSources :: Lens' UpdateApp [DataSource]
uaDataSources = lens _uaDataSources (\s a -> s { _uaDataSources = a })

-- | The app type.
uaType :: Lens' UpdateApp (Maybe AppType)
uaType = lens _uaType (\s a -> s { _uaType = a })

-- | A Source object that specifies the app repository.
uaAppSource :: Lens' UpdateApp (Maybe Source')
uaAppSource = lens _uaAppSource (\s a -> s { _uaAppSource = a })

-- | The app's virtual host settings, with multiple domains separated by commas.
-- For example: 'www.example.com, example.com'.
uaDomains :: Lens' UpdateApp [Text]
uaDomains = lens _uaDomains (\s a -> s { _uaDomains = a })

-- | Whether SSL is enabled for the app.
uaEnableSsl :: Lens' UpdateApp (Maybe Bool)
uaEnableSsl = lens _uaEnableSsl (\s a -> s { _uaEnableSsl = a })

-- | An SslConfiguration object with the SSL configuration.
uaSslConfiguration :: Lens' UpdateApp (Maybe SslConfiguration)
uaSslConfiguration =
    lens _uaSslConfiguration (\s a -> s { _uaSslConfiguration = a })

-- | One or more user-defined key/value pairs to be added to the stack
-- attributes.
uaAttributes :: Lens' UpdateApp (Map AppAttributesKeys Text)
uaAttributes = lens _uaAttributes (\s a -> s { _uaAttributes = a })

-- | An array of EnvironmentVariable objects that specify environment variables
-- to be associated with the app. You can specify up to ten environment
-- variables. After you deploy the app, these variables are defined on the
-- associated app server instances. This parameter is supported only by Chef
-- 11.10 stacks. If you have specified one or more environment variables, you
-- cannot modify the stack's Chef version.
uaEnvironment :: Lens' UpdateApp [EnvironmentVariable]
uaEnvironment = lens _uaEnvironment (\s a -> s { _uaEnvironment = a })

instance ToPath UpdateApp

instance ToQuery UpdateApp

instance ToHeaders UpdateApp

instance ToJSON UpdateApp

data UpdateAppResponse = UpdateAppResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateAppResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
updateAppResponse :: UpdateAppResponse
updateAppResponse = UpdateAppResponse

instance AWSRequest UpdateApp where
    type Sv UpdateApp = OpsWorks
    type Rs UpdateApp = UpdateAppResponse

    request = get
    response _ = nullaryResponse UpdateAppResponse
