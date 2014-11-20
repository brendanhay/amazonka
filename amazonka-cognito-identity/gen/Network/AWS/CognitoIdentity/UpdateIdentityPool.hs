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

-- Module      : Network.AWS.CognitoIdentity.UpdateIdentityPool
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates a user pool.
--
-- <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_UpdateIdentityPool.html>
module Network.AWS.CognitoIdentity.UpdateIdentityPool
    (
    -- * Request
      UpdateIdentityPool
    -- ** Request constructor
    , updateIdentityPool
    -- ** Request lenses
    , uipAllowUnauthenticatedIdentities
    , uipDeveloperProviderName
    , uipIdentityPoolId
    , uipIdentityPoolName
    , uipOpenIdConnectProviderARNs
    , uipSupportedLoginProviders

    -- * Response
    , UpdateIdentityPoolResponse
    -- ** Response constructor
    , updateIdentityPoolResponse
    -- ** Response lenses
    , uiprAllowUnauthenticatedIdentities
    , uiprDeveloperProviderName
    , uiprIdentityPoolId
    , uiprIdentityPoolName
    , uiprOpenIdConnectProviderARNs
    , uiprSupportedLoginProviders
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CognitoIdentity.Types
import qualified GHC.Exts

data UpdateIdentityPool = UpdateIdentityPool
    { _uipAllowUnauthenticatedIdentities :: Bool
    , _uipDeveloperProviderName          :: Maybe Text
    , _uipIdentityPoolId                 :: Text
    , _uipIdentityPoolName               :: Text
    , _uipOpenIdConnectProviderARNs      :: List "OpenIdConnectProviderARNs" Text
    , _uipSupportedLoginProviders        :: Map Text Text
    } deriving (Eq, Show)

-- | 'UpdateIdentityPool' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uipAllowUnauthenticatedIdentities' @::@ 'Bool'
--
-- * 'uipDeveloperProviderName' @::@ 'Maybe' 'Text'
--
-- * 'uipIdentityPoolId' @::@ 'Text'
--
-- * 'uipIdentityPoolName' @::@ 'Text'
--
-- * 'uipOpenIdConnectProviderARNs' @::@ ['Text']
--
-- * 'uipSupportedLoginProviders' @::@ 'HashMap' 'Text' 'Text'
--
updateIdentityPool :: Text -- ^ 'uipIdentityPoolId'
                   -> Text -- ^ 'uipIdentityPoolName'
                   -> Bool -- ^ 'uipAllowUnauthenticatedIdentities'
                   -> UpdateIdentityPool
updateIdentityPool p1 p2 p3 = UpdateIdentityPool
    { _uipIdentityPoolId                 = p1
    , _uipIdentityPoolName               = p2
    , _uipAllowUnauthenticatedIdentities = p3
    , _uipSupportedLoginProviders        = mempty
    , _uipDeveloperProviderName          = Nothing
    , _uipOpenIdConnectProviderARNs      = mempty
    }

-- | TRUE if the identity pool supports unauthenticated logins.
uipAllowUnauthenticatedIdentities :: Lens' UpdateIdentityPool Bool
uipAllowUnauthenticatedIdentities =
    lens _uipAllowUnauthenticatedIdentities
        (\s a -> s { _uipAllowUnauthenticatedIdentities = a })

-- | The "domain" by which Cognito will refer to your users.
uipDeveloperProviderName :: Lens' UpdateIdentityPool (Maybe Text)
uipDeveloperProviderName =
    lens _uipDeveloperProviderName
        (\s a -> s { _uipDeveloperProviderName = a })

-- | An identity pool ID in the format REGION:GUID.
uipIdentityPoolId :: Lens' UpdateIdentityPool Text
uipIdentityPoolId =
    lens _uipIdentityPoolId (\s a -> s { _uipIdentityPoolId = a })

-- | A string that you provide.
uipIdentityPoolName :: Lens' UpdateIdentityPool Text
uipIdentityPoolName =
    lens _uipIdentityPoolName (\s a -> s { _uipIdentityPoolName = a })

uipOpenIdConnectProviderARNs :: Lens' UpdateIdentityPool [Text]
uipOpenIdConnectProviderARNs =
    lens _uipOpenIdConnectProviderARNs
        (\s a -> s { _uipOpenIdConnectProviderARNs = a })
            . _List

-- | Optional key:value pairs mapping provider names to provider app IDs.
uipSupportedLoginProviders :: Lens' UpdateIdentityPool (HashMap Text Text)
uipSupportedLoginProviders =
    lens _uipSupportedLoginProviders
        (\s a -> s { _uipSupportedLoginProviders = a })
            . _Map

data UpdateIdentityPoolResponse = UpdateIdentityPoolResponse
    { _uiprAllowUnauthenticatedIdentities :: Bool
    , _uiprDeveloperProviderName          :: Maybe Text
    , _uiprIdentityPoolId                 :: Text
    , _uiprIdentityPoolName               :: Text
    , _uiprOpenIdConnectProviderARNs      :: List "OpenIdConnectProviderARNs" Text
    , _uiprSupportedLoginProviders        :: Map Text Text
    } deriving (Eq, Show)

-- | 'UpdateIdentityPoolResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uiprAllowUnauthenticatedIdentities' @::@ 'Bool'
--
-- * 'uiprDeveloperProviderName' @::@ 'Maybe' 'Text'
--
-- * 'uiprIdentityPoolId' @::@ 'Text'
--
-- * 'uiprIdentityPoolName' @::@ 'Text'
--
-- * 'uiprOpenIdConnectProviderARNs' @::@ ['Text']
--
-- * 'uiprSupportedLoginProviders' @::@ 'HashMap' 'Text' 'Text'
--
updateIdentityPoolResponse :: Text -- ^ 'uiprIdentityPoolId'
                           -> Text -- ^ 'uiprIdentityPoolName'
                           -> Bool -- ^ 'uiprAllowUnauthenticatedIdentities'
                           -> UpdateIdentityPoolResponse
updateIdentityPoolResponse p1 p2 p3 = UpdateIdentityPoolResponse
    { _uiprIdentityPoolId                 = p1
    , _uiprIdentityPoolName               = p2
    , _uiprAllowUnauthenticatedIdentities = p3
    , _uiprSupportedLoginProviders        = mempty
    , _uiprDeveloperProviderName          = Nothing
    , _uiprOpenIdConnectProviderARNs      = mempty
    }

-- | TRUE if the identity pool supports unauthenticated logins.
uiprAllowUnauthenticatedIdentities :: Lens' UpdateIdentityPoolResponse Bool
uiprAllowUnauthenticatedIdentities =
    lens _uiprAllowUnauthenticatedIdentities
        (\s a -> s { _uiprAllowUnauthenticatedIdentities = a })

-- | The "domain" by which Cognito will refer to your users.
uiprDeveloperProviderName :: Lens' UpdateIdentityPoolResponse (Maybe Text)
uiprDeveloperProviderName =
    lens _uiprDeveloperProviderName
        (\s a -> s { _uiprDeveloperProviderName = a })

-- | An identity pool ID in the format REGION:GUID.
uiprIdentityPoolId :: Lens' UpdateIdentityPoolResponse Text
uiprIdentityPoolId =
    lens _uiprIdentityPoolId (\s a -> s { _uiprIdentityPoolId = a })

-- | A string that you provide.
uiprIdentityPoolName :: Lens' UpdateIdentityPoolResponse Text
uiprIdentityPoolName =
    lens _uiprIdentityPoolName (\s a -> s { _uiprIdentityPoolName = a })

uiprOpenIdConnectProviderARNs :: Lens' UpdateIdentityPoolResponse [Text]
uiprOpenIdConnectProviderARNs =
    lens _uiprOpenIdConnectProviderARNs
        (\s a -> s { _uiprOpenIdConnectProviderARNs = a })
            . _List

-- | Optional key:value pairs mapping provider names to provider app IDs.
uiprSupportedLoginProviders :: Lens' UpdateIdentityPoolResponse (HashMap Text Text)
uiprSupportedLoginProviders =
    lens _uiprSupportedLoginProviders
        (\s a -> s { _uiprSupportedLoginProviders = a })
            . _Map

instance ToPath UpdateIdentityPool where
    toPath = const "/"

instance ToQuery UpdateIdentityPool where
    toQuery = const mempty

instance ToHeaders UpdateIdentityPool

instance ToJSON UpdateIdentityPool where
    toJSON UpdateIdentityPool{..} = object
        [ "IdentityPoolId"                 .= _uipIdentityPoolId
        , "IdentityPoolName"               .= _uipIdentityPoolName
        , "AllowUnauthenticatedIdentities" .= _uipAllowUnauthenticatedIdentities
        , "SupportedLoginProviders"        .= _uipSupportedLoginProviders
        , "DeveloperProviderName"          .= _uipDeveloperProviderName
        , "OpenIdConnectProviderARNs"      .= _uipOpenIdConnectProviderARNs
        ]

instance AWSRequest UpdateIdentityPool where
    type Sv UpdateIdentityPool = CognitoIdentity
    type Rs UpdateIdentityPool = UpdateIdentityPoolResponse

    request  = post "UpdateIdentityPool"
    response = jsonResponse

instance FromJSON UpdateIdentityPoolResponse where
    parseJSON = withObject "UpdateIdentityPoolResponse" $ \o -> UpdateIdentityPoolResponse
        <$> o .:  "AllowUnauthenticatedIdentities"
        <*> o .:? "DeveloperProviderName"
        <*> o .:  "IdentityPoolId"
        <*> o .:  "IdentityPoolName"
        <*> o .:  "OpenIdConnectProviderARNs"
        <*> o .:  "SupportedLoginProviders"


Some kind of operator / class to check the types whether to continue?
