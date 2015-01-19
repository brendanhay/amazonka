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

-- Module      : Network.AWS.CloudHSM.GetConfig
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Gets the configuration files necessary to connect to all high availability
-- partition groups the client is associated with.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_GetConfig.html>
module Network.AWS.CloudHSM.GetConfig
    (
    -- * Request
      GetConfig
    -- ** Request constructor
    , getConfig
    -- ** Request lenses
    , gcClientArn
    , gcClientVersion
    , gcHapgList

    -- * Response
    , GetConfigResponse
    -- ** Response constructor
    , getConfigResponse
    -- ** Response lenses
    , gcrConfigCred
    , gcrConfigFile
    , gcrConfigType
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CloudHSM.Types
import qualified GHC.Exts

data GetConfig = GetConfig
    { _gcClientArn     :: Text
    , _gcClientVersion :: ClientVersion
    , _gcHapgList      :: List "HapgList" Text
    } deriving (Eq, Show)

-- | 'GetConfig' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcClientArn' @::@ 'Text'
--
-- * 'gcClientVersion' @::@ 'ClientVersion'
--
-- * 'gcHapgList' @::@ ['Text']
--
getConfig :: Text -- ^ 'gcClientArn'
          -> ClientVersion -- ^ 'gcClientVersion'
          -> GetConfig
getConfig p1 p2 = GetConfig
    { _gcClientArn     = p1
    , _gcClientVersion = p2
    , _gcHapgList      = mempty
    }

-- | The ARN of the client.
gcClientArn :: Lens' GetConfig Text
gcClientArn = lens _gcClientArn (\s a -> s { _gcClientArn = a })

-- | The client version.
gcClientVersion :: Lens' GetConfig ClientVersion
gcClientVersion = lens _gcClientVersion (\s a -> s { _gcClientVersion = a })

-- | A list of ARNs that identify the high-availability partition groups that are
-- associated with the client.
gcHapgList :: Lens' GetConfig [Text]
gcHapgList = lens _gcHapgList (\s a -> s { _gcHapgList = a }) . _List

data GetConfigResponse = GetConfigResponse
    { _gcrConfigCred :: Maybe Text
    , _gcrConfigFile :: Maybe Text
    , _gcrConfigType :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'GetConfigResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcrConfigCred' @::@ 'Maybe' 'Text'
--
-- * 'gcrConfigFile' @::@ 'Maybe' 'Text'
--
-- * 'gcrConfigType' @::@ 'Maybe' 'Text'
--
getConfigResponse :: GetConfigResponse
getConfigResponse = GetConfigResponse
    { _gcrConfigType = Nothing
    , _gcrConfigFile = Nothing
    , _gcrConfigCred = Nothing
    }

-- | The certificate file containing the server.pem files of the HSMs.
gcrConfigCred :: Lens' GetConfigResponse (Maybe Text)
gcrConfigCred = lens _gcrConfigCred (\s a -> s { _gcrConfigCred = a })

-- | The chrystoki.conf configuration file.
gcrConfigFile :: Lens' GetConfigResponse (Maybe Text)
gcrConfigFile = lens _gcrConfigFile (\s a -> s { _gcrConfigFile = a })

-- | The type of credentials.
gcrConfigType :: Lens' GetConfigResponse (Maybe Text)
gcrConfigType = lens _gcrConfigType (\s a -> s { _gcrConfigType = a })

instance ToPath GetConfig where
    toPath = const "/"

instance ToQuery GetConfig where
    toQuery = const mempty

instance ToHeaders GetConfig

instance ToJSON GetConfig where
    toJSON GetConfig{..} = object
        [ "ClientArn"     .= _gcClientArn
        , "ClientVersion" .= _gcClientVersion
        , "HapgList"      .= _gcHapgList
        ]

instance AWSRequest GetConfig where
    type Sv GetConfig = CloudHSM
    type Rs GetConfig = GetConfigResponse

    request  = post "GetConfig"
    response = jsonResponse

instance FromJSON GetConfigResponse where
    parseJSON = withObject "GetConfigResponse" $ \o -> GetConfigResponse
        <$> o .:? "ConfigCred"
        <*> o .:? "ConfigFile"
        <*> o .:? "ConfigType"
