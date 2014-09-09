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
    , mkCreateApp
    -- ** Request lenses
    , caStackId
    , caShortname
    , caName
    , caDescription
    , caDataSources
    , caType
    , caAppSource
    , caDomains
    , caEnableSsl
    , caSslConfiguration
    , caAttributes

    -- * Response
    , CreateAppResponse
    -- ** Response constructor
    , mkCreateAppResponse
    -- ** Response lenses
    , carAppId
    ) where

import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data CreateApp = CreateApp
    { _caStackId :: Text
    , _caShortname :: Maybe Text
    , _caName :: Text
    , _caDescription :: Maybe Text
    , _caDataSources :: [DataSource]
    , _caType :: AppType
    , _caAppSource :: Maybe Source
    , _caDomains :: [Text]
    , _caEnableSsl :: Maybe Bool
    , _caSslConfiguration :: Maybe SslConfiguration
    , _caAttributes :: Map AppAttributesKeys Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateApp' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StackId ::@ @Text@
--
-- * @Shortname ::@ @Maybe Text@
--
-- * @Name ::@ @Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @DataSources ::@ @[DataSource]@
--
-- * @Type ::@ @AppType@
--
-- * @AppSource ::@ @Maybe Source@
--
-- * @Domains ::@ @[Text]@
--
-- * @EnableSsl ::@ @Maybe Bool@
--
-- * @SslConfiguration ::@ @Maybe SslConfiguration@
--
-- * @Attributes ::@ @Map AppAttributesKeys Text@
--
mkCreateApp :: Text -- ^ 'caStackId'
            -> Text -- ^ 'caName'
            -> AppType -- ^ 'caType'
            -> CreateApp
mkCreateApp p1 p3 p6 = CreateApp
    { _caStackId = p1
    , _caShortname = Nothing
    , _caName = p3
    , _caDescription = Nothing
    , _caDataSources = mempty
    , _caType = p6
    , _caAppSource = Nothing
    , _caDomains = mempty
    , _caEnableSsl = Nothing
    , _caSslConfiguration = Nothing
    , _caAttributes = mempty
    }

-- | The stack ID.
caStackId :: Lens' CreateApp Text
caStackId = lens _caStackId (\s a -> s { _caStackId = a })

-- | The app's short name.
caShortname :: Lens' CreateApp (Maybe Text)
caShortname = lens _caShortname (\s a -> s { _caShortname = a })

-- | The app name.
caName :: Lens' CreateApp Text
caName = lens _caName (\s a -> s { _caName = a })

-- | A description of the app.
caDescription :: Lens' CreateApp (Maybe Text)
caDescription = lens _caDescription (\s a -> s { _caDescription = a })

-- | The app's data source.
caDataSources :: Lens' CreateApp [DataSource]
caDataSources = lens _caDataSources (\s a -> s { _caDataSources = a })

-- | The app type. Each supported type is associated with a particular layer.
-- For example, PHP applications are associated with a PHP layer. AWS OpsWorks
-- deploys an application to those instances that are members of the
-- corresponding layer.
caType :: Lens' CreateApp AppType
caType = lens _caType (\s a -> s { _caType = a })

-- | A Source object that specifies the app repository.
caAppSource :: Lens' CreateApp (Maybe Source)
caAppSource = lens _caAppSource (\s a -> s { _caAppSource = a })

-- | The app virtual host settings, with multiple domains separated by commas.
-- For example: 'www.example.com, example.com'.
caDomains :: Lens' CreateApp [Text]
caDomains = lens _caDomains (\s a -> s { _caDomains = a })

-- | Whether to enable SSL for the app.
caEnableSsl :: Lens' CreateApp (Maybe Bool)
caEnableSsl = lens _caEnableSsl (\s a -> s { _caEnableSsl = a })

-- | An SslConfiguration object with the SSL configuration.
caSslConfiguration :: Lens' CreateApp (Maybe SslConfiguration)
caSslConfiguration =
    lens _caSslConfiguration (\s a -> s { _caSslConfiguration = a })

-- | One or more user-defined key/value pairs to be added to the stack
-- attributes.
caAttributes :: Lens' CreateApp (Map AppAttributesKeys Text)
caAttributes = lens _caAttributes (\s a -> s { _caAttributes = a })

instance ToPath CreateApp

instance ToQuery CreateApp

instance ToHeaders CreateApp

instance ToJSON CreateApp

-- | Contains the response to a CreateApp request.
newtype CreateAppResponse = CreateAppResponse
    { _carAppId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateAppResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AppId ::@ @Maybe Text@
--
mkCreateAppResponse :: CreateAppResponse
mkCreateAppResponse = CreateAppResponse
    { _carAppId = Nothing
    }

-- | The app ID.
carAppId :: Lens' CreateAppResponse (Maybe Text)
carAppId = lens _carAppId (\s a -> s { _carAppId = a })

instance FromJSON CreateAppResponse

instance AWSRequest CreateApp where
    type Sv CreateApp = OpsWorks
    type Rs CreateApp = CreateAppResponse

    request = get
    response _ = jsonResponse
