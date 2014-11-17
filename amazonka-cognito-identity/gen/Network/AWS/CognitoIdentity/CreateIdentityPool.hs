{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoIdentity.CreateIdentityPool
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new identity pool. The identity pool is a store of user identity
-- information that is specific to your AWS account. The limit on identity
-- pools is 60 per account.
--
-- <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_CreateIdentityPool.html>
module Network.AWS.CognitoIdentity.CreateIdentityPool
    (
    -- * Request
      CreateIdentityPool
    -- ** Request constructor
    , createIdentityPool
    -- ** Request lenses
    , cipAllowUnauthenticatedIdentities
    , cipDeveloperProviderName
    , cipIdentityPoolName
    , cipOpenIdConnectProviderARNs
    , cipSupportedLoginProviders

    -- * Response
    , CreateIdentityPoolResponse
    -- ** Response constructor
    , createIdentityPoolResponse
    -- ** Response lenses
    , ciprAllowUnauthenticatedIdentities
    , ciprDeveloperProviderName
    , ciprIdentityPoolId
    , ciprIdentityPoolName
    , ciprOpenIdConnectProviderARNs
    , ciprSupportedLoginProviders
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CognitoIdentity.Types
import qualified GHC.Exts

data CreateIdentityPool = CreateIdentityPool
    { _cipAllowUnauthenticatedIdentities :: Bool
    , _cipDeveloperProviderName          :: Maybe Text
    , _cipIdentityPoolName               :: Text
    , _cipOpenIdConnectProviderARNs      :: [Text]
    , _cipSupportedLoginProviders        :: Map Text Text
    } deriving (Eq, Show, Generic)

-- | 'CreateIdentityPool' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cipAllowUnauthenticatedIdentities' @::@ 'Bool'
--
-- * 'cipDeveloperProviderName' @::@ 'Maybe' 'Text'
--
-- * 'cipIdentityPoolName' @::@ 'Text'
--
-- * 'cipOpenIdConnectProviderARNs' @::@ ['Text']
--
-- * 'cipSupportedLoginProviders' @::@ 'HashMap' 'Text' 'Text'
--
createIdentityPool :: Text -- ^ 'cipIdentityPoolName'
                   -> Bool -- ^ 'cipAllowUnauthenticatedIdentities'
                   -> CreateIdentityPool
createIdentityPool p1 p2 = CreateIdentityPool
    { _cipIdentityPoolName               = p1
    , _cipAllowUnauthenticatedIdentities = p2
    , _cipSupportedLoginProviders        = mempty
    , _cipDeveloperProviderName          = Nothing
    , _cipOpenIdConnectProviderARNs      = mempty
    }

-- | TRUE if the identity pool supports unauthenticated logins.
cipAllowUnauthenticatedIdentities :: Lens' CreateIdentityPool Bool
cipAllowUnauthenticatedIdentities =
    lens _cipAllowUnauthenticatedIdentities
        (\s a -> s { _cipAllowUnauthenticatedIdentities = a })

-- | The "domain" by which Cognito will refer to your users. This name acts as
-- a placeholder that allows your backend and the Cognito service to
-- communicate about the developer provider. For the DeveloperProviderName,
-- you can use letters as well as period (.), underscore (_), and dash (-).
-- Once you have set a developer provider name, you cannot change it. Please
-- take care in setting this parameter.
cipDeveloperProviderName :: Lens' CreateIdentityPool (Maybe Text)
cipDeveloperProviderName =
    lens _cipDeveloperProviderName
        (\s a -> s { _cipDeveloperProviderName = a })

-- | A string that you provide.
cipIdentityPoolName :: Lens' CreateIdentityPool Text
cipIdentityPoolName =
    lens _cipIdentityPoolName (\s a -> s { _cipIdentityPoolName = a })

cipOpenIdConnectProviderARNs :: Lens' CreateIdentityPool [Text]
cipOpenIdConnectProviderARNs =
    lens _cipOpenIdConnectProviderARNs
        (\s a -> s { _cipOpenIdConnectProviderARNs = a })

-- | Optional key:value pairs mapping provider names to provider app IDs.
cipSupportedLoginProviders :: Lens' CreateIdentityPool (HashMap Text Text)
cipSupportedLoginProviders =
    lens _cipSupportedLoginProviders
        (\s a -> s { _cipSupportedLoginProviders = a })
            . _Map

data CreateIdentityPoolResponse = CreateIdentityPoolResponse
    { _ciprAllowUnauthenticatedIdentities :: Bool
    , _ciprDeveloperProviderName          :: Maybe Text
    , _ciprIdentityPoolId                 :: Text
    , _ciprIdentityPoolName               :: Text
    , _ciprOpenIdConnectProviderARNs      :: [Text]
    , _ciprSupportedLoginProviders        :: Map Text Text
    } deriving (Eq, Show, Generic)

-- | 'CreateIdentityPoolResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ciprAllowUnauthenticatedIdentities' @::@ 'Bool'
--
-- * 'ciprDeveloperProviderName' @::@ 'Maybe' 'Text'
--
-- * 'ciprIdentityPoolId' @::@ 'Text'
--
-- * 'ciprIdentityPoolName' @::@ 'Text'
--
-- * 'ciprOpenIdConnectProviderARNs' @::@ ['Text']
--
-- * 'ciprSupportedLoginProviders' @::@ 'HashMap' 'Text' 'Text'
--
createIdentityPoolResponse :: Text -- ^ 'ciprIdentityPoolId'
                           -> Text -- ^ 'ciprIdentityPoolName'
                           -> Bool -- ^ 'ciprAllowUnauthenticatedIdentities'
                           -> CreateIdentityPoolResponse
createIdentityPoolResponse p1 p2 p3 = CreateIdentityPoolResponse
    { _ciprIdentityPoolId                 = p1
    , _ciprIdentityPoolName               = p2
    , _ciprAllowUnauthenticatedIdentities = p3
    , _ciprSupportedLoginProviders        = mempty
    , _ciprDeveloperProviderName          = Nothing
    , _ciprOpenIdConnectProviderARNs      = mempty
    }

-- | TRUE if the identity pool supports unauthenticated logins.
ciprAllowUnauthenticatedIdentities :: Lens' CreateIdentityPoolResponse Bool
ciprAllowUnauthenticatedIdentities =
    lens _ciprAllowUnauthenticatedIdentities
        (\s a -> s { _ciprAllowUnauthenticatedIdentities = a })

-- | The "domain" by which Cognito will refer to your users.
ciprDeveloperProviderName :: Lens' CreateIdentityPoolResponse (Maybe Text)
ciprDeveloperProviderName =
    lens _ciprDeveloperProviderName
        (\s a -> s { _ciprDeveloperProviderName = a })

-- | An identity pool ID in the format REGION:GUID.
ciprIdentityPoolId :: Lens' CreateIdentityPoolResponse Text
ciprIdentityPoolId =
    lens _ciprIdentityPoolId (\s a -> s { _ciprIdentityPoolId = a })

-- | A string that you provide.
ciprIdentityPoolName :: Lens' CreateIdentityPoolResponse Text
ciprIdentityPoolName =
    lens _ciprIdentityPoolName (\s a -> s { _ciprIdentityPoolName = a })

ciprOpenIdConnectProviderARNs :: Lens' CreateIdentityPoolResponse [Text]
ciprOpenIdConnectProviderARNs =
    lens _ciprOpenIdConnectProviderARNs
        (\s a -> s { _ciprOpenIdConnectProviderARNs = a })

-- | Optional key:value pairs mapping provider names to provider app IDs.
ciprSupportedLoginProviders :: Lens' CreateIdentityPoolResponse (HashMap Text Text)
ciprSupportedLoginProviders =
    lens _ciprSupportedLoginProviders
        (\s a -> s { _ciprSupportedLoginProviders = a })
            . _Map

instance ToPath CreateIdentityPool where
    toPath = const "/"

instance ToQuery CreateIdentityPool where
    toQuery = const mempty

instance ToHeaders CreateIdentityPool
instance ToJSON CreateIdentityPool where
    toJSON = genericToJSON jsonOptions

instance AWSRequest CreateIdentityPool where
    type Sv CreateIdentityPool = CognitoIdentity
    type Rs CreateIdentityPool = CreateIdentityPoolResponse

    request  = post "CreateIdentityPool"
    response = jsonResponse

instance FromJSON CreateIdentityPoolResponse where
    parseJSON = genericParseJSON jsonOptions
