{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.IAM.CreateOpenIDConnectProvider
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an IAM entity to describe an identity provider (IdP) that supports
-- OpenID Connect (OIDC). The OIDC provider that you create with this
-- operation can be used as a principal in a role's trust policy to establish
-- a trust relationship between AWS and the OIDC provider. When you create the
-- IAM OIDC provider, you specify the URL of the OIDC identity provider (IdP)
-- to trust, a list of client IDs (also known as audiences) that identify the
-- application or applications that are allowed to authenticate using the OIDC
-- provider, and a list of thumbprints of the server certificate(s) that the
-- IdP uses. You get all of this information from the OIDC IdP that you want
-- to use for access to AWS.
module Network.AWS.IAM.CreateOpenIDConnectProvider
    (
    -- * Request
      CreateOpenIDConnectProvider
    -- ** Request constructor
    , createOpenIDConnectProvider
    -- ** Request lenses
    , coidcpClientIDList
    , coidcpThumbprintList
    , coidcpUrl

    -- * Response
    , CreateOpenIDConnectProviderResponse
    -- ** Response constructor
    , createOpenIDConnectProviderResponse
    -- ** Response lenses
    , coidcprOpenIDConnectProviderArn
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types

data CreateOpenIDConnectProvider = CreateOpenIDConnectProvider
    { _coidcpClientIDList   :: [Text]
    , _coidcpThumbprintList :: [Text]
    , _coidcpUrl            :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateOpenIDConnectProvider' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'coidcpClientIDList' @::@ ['Text']
--
-- * 'coidcpThumbprintList' @::@ ['Text']
--
-- * 'coidcpUrl' @::@ 'Text'
--
createOpenIDConnectProvider :: Text -- ^ 'coidcpUrl'
                            -> CreateOpenIDConnectProvider
createOpenIDConnectProvider p1 = CreateOpenIDConnectProvider
    { _coidcpUrl            = p1
    , _coidcpClientIDList   = mempty
    , _coidcpThumbprintList = mempty
    }

-- | A list of client IDs (also known as audiences). When a mobile or web app
-- registers with an OpenID Connect provider, they establish a value that
-- identifies the application. (This is the value that's sent as the
-- client_id parameter on OAuth requests.) You can register multiple client
-- IDs with the same provider. For example, you might have multiple
-- applications that use the same OIDC provider. You cannot register more
-- than 100 client IDs with a single IAM OIDC provider. There is no defined
-- format for a client ID. The CreateOpenIDConnectProviderRequest action
-- accepts client IDs up to 255 characters long.
coidcpClientIDList :: Lens' CreateOpenIDConnectProvider [Text]
coidcpClientIDList =
    lens _coidcpClientIDList (\s a -> s { _coidcpClientIDList = a })

-- | A list of server certificate thumbprints for the OpenID Connect (OIDC)
-- identity provider's server certificate(s). Typically this list includes
-- only one entry. However, IAM lets you have up to five thumbprints for an
-- OIDC provider. This lets you maintain multiple thumbprints if the
-- identity provider is rotating certificates. The server certificate
-- thumbprint is the hex-encoded SHA-1 hash value of the X.509 certificate
-- used by the domain where the OpenID Connect provider makes its keys
-- available. It is always a 40-character string. You must provide at least
-- one thumbprint when creating an IAM OIDC provider. For example, if the
-- OIDC provider is server.example.com and the provider stores its keys at
-- "https://keys.server.example.com/openid-connect", the thumbprint string
-- would be the hex-encoded SHA-1 hash value of the certificate used by
-- https://keys.server.example.com.
coidcpThumbprintList :: Lens' CreateOpenIDConnectProvider [Text]
coidcpThumbprintList =
    lens _coidcpThumbprintList (\s a -> s { _coidcpThumbprintList = a })

-- | The URL of the identity provider. The URL must begin with "https://" and
-- should correspond to the iss claim in the provider's OpenID Connect ID
-- tokens. Per the OIDC standard, path components are allowed but query
-- parameters are not. Typically the URL consists of only a host name, like
-- "https://server.example.org" or "https://example.com". You cannot
-- register the same provider multiple times in a single AWS account. If you
-- try to submit a URL that has already been used for an OpenID Connect
-- provider in the AWS account, you will get an error.
coidcpUrl :: Lens' CreateOpenIDConnectProvider Text
coidcpUrl = lens _coidcpUrl (\s a -> s { _coidcpUrl = a })

instance ToQuery CreateOpenIDConnectProvider

instance ToPath CreateOpenIDConnectProvider where
    toPath = const "/"

newtype CreateOpenIDConnectProviderResponse = CreateOpenIDConnectProviderResponse
    { _coidcprOpenIDConnectProviderArn :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'CreateOpenIDConnectProviderResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'coidcprOpenIDConnectProviderArn' @::@ 'Maybe' 'Text'
--
createOpenIDConnectProviderResponse :: CreateOpenIDConnectProviderResponse
createOpenIDConnectProviderResponse = CreateOpenIDConnectProviderResponse
    { _coidcprOpenIDConnectProviderArn = Nothing
    }

-- | The Amazon Resource Name (ARN) of the IAM OpenID Connect provider that
-- was created. For more information, see OpenIDConnectProviderListEntry.
coidcprOpenIDConnectProviderArn :: Lens' CreateOpenIDConnectProviderResponse (Maybe Text)
coidcprOpenIDConnectProviderArn =
    lens _coidcprOpenIDConnectProviderArn
        (\s a -> s { _coidcprOpenIDConnectProviderArn = a })

instance FromXML CreateOpenIDConnectProviderResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateOpenIDConnectProviderResponse"

instance AWSRequest CreateOpenIDConnectProvider where
    type Sv CreateOpenIDConnectProvider = IAM
    type Rs CreateOpenIDConnectProvider = CreateOpenIDConnectProviderResponse

    request  = post "CreateOpenIDConnectProvider"
    response = xmlResponse $ \h x -> CreateOpenIDConnectProviderResponse
        <$> x %| "OpenIDConnectProviderArn"
