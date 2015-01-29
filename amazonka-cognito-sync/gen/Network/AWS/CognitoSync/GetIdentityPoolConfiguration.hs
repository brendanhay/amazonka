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

-- Module      : Network.AWS.CognitoSync.GetIdentityPoolConfiguration
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Gets the configuration settings of an identity pool.
--
-- <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_GetIdentityPoolConfiguration.html>
module Network.AWS.CognitoSync.GetIdentityPoolConfiguration
    (
    -- * Request
      GetIdentityPoolConfiguration
    -- ** Request constructor
    , getIdentityPoolConfiguration
    -- ** Request lenses
    , gipcIdentityPoolId

    -- * Response
    , GetIdentityPoolConfigurationResponse
    -- ** Response constructor
    , getIdentityPoolConfigurationResponse
    -- ** Response lenses
    , gipcrIdentityPoolId
    , gipcrPushSync
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.CognitoSync.Types
import qualified GHC.Exts

newtype GetIdentityPoolConfiguration = GetIdentityPoolConfiguration
    { _gipcIdentityPoolId :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'GetIdentityPoolConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gipcIdentityPoolId' @::@ 'Text'
--
getIdentityPoolConfiguration :: Text -- ^ 'gipcIdentityPoolId'
                             -> GetIdentityPoolConfiguration
getIdentityPoolConfiguration p1 = GetIdentityPoolConfiguration
    { _gipcIdentityPoolId = p1
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito.
-- This is the ID of the pool for which to return a configuration.
gipcIdentityPoolId :: Lens' GetIdentityPoolConfiguration Text
gipcIdentityPoolId =
    lens _gipcIdentityPoolId (\s a -> s { _gipcIdentityPoolId = a })

data GetIdentityPoolConfigurationResponse = GetIdentityPoolConfigurationResponse
    { _gipcrIdentityPoolId :: Maybe Text
    , _gipcrPushSync       :: Maybe PushSync
    } deriving (Eq, Read, Show)

-- | 'GetIdentityPoolConfigurationResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gipcrIdentityPoolId' @::@ 'Maybe' 'Text'
--
-- * 'gipcrPushSync' @::@ 'Maybe' 'PushSync'
--
getIdentityPoolConfigurationResponse :: GetIdentityPoolConfigurationResponse
getIdentityPoolConfigurationResponse = GetIdentityPoolConfigurationResponse
    { _gipcrIdentityPoolId = Nothing
    , _gipcrPushSync       = Nothing
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito.
gipcrIdentityPoolId :: Lens' GetIdentityPoolConfigurationResponse (Maybe Text)
gipcrIdentityPoolId =
    lens _gipcrIdentityPoolId (\s a -> s { _gipcrIdentityPoolId = a })

-- | Configuration options applied to the identity pool.
gipcrPushSync :: Lens' GetIdentityPoolConfigurationResponse (Maybe PushSync)
gipcrPushSync = lens _gipcrPushSync (\s a -> s { _gipcrPushSync = a })

instance ToPath GetIdentityPoolConfiguration where
    toPath GetIdentityPoolConfiguration{..} = mconcat
        [ "/identitypools/"
        , toText _gipcIdentityPoolId
        , "/configuration"
        ]

instance ToQuery GetIdentityPoolConfiguration where
    toQuery = const mempty

instance ToHeaders GetIdentityPoolConfiguration

instance ToJSON GetIdentityPoolConfiguration where
    toJSON = const (toJSON Empty)

instance AWSRequest GetIdentityPoolConfiguration where
    type Sv GetIdentityPoolConfiguration = CognitoSync
    type Rs GetIdentityPoolConfiguration = GetIdentityPoolConfigurationResponse

    request  = get
    response = jsonResponse

instance FromJSON GetIdentityPoolConfigurationResponse where
    parseJSON = withObject "GetIdentityPoolConfigurationResponse" $ \o -> GetIdentityPoolConfigurationResponse
        <$> o .:? "IdentityPoolId"
        <*> o .:? "PushSync"
