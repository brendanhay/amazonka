{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.GetOpenIDConnectProvider
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about the specified OpenID Connect provider.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetOpenIDConnectProvider.html>
module Network.AWS.IAM.GetOpenIDConnectProvider
    (
    -- * Request
      GetOpenIDConnectProvider
    -- ** Request constructor
    , getOpenIDConnectProvider
    -- ** Request lenses
    , goidcpOpenIDConnectProviderArn

    -- * Response
    , GetOpenIDConnectProviderResponse
    -- ** Response constructor
    , getOpenIDConnectProviderResponse
    -- ** Response lenses
    , goidcprClientIDList
    , goidcprCreateDate
    , goidcprThumbprintList
    , goidcprUrl
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

newtype GetOpenIDConnectProvider = GetOpenIDConnectProvider
    { _goidcpOpenIDConnectProviderArn :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'GetOpenIDConnectProvider' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'goidcpOpenIDConnectProviderArn' @::@ 'Text'
--
getOpenIDConnectProvider :: Text -- ^ 'goidcpOpenIDConnectProviderArn'
                         -> GetOpenIDConnectProvider
getOpenIDConnectProvider p1 = GetOpenIDConnectProvider
    { _goidcpOpenIDConnectProviderArn = p1
    }

-- | The Amazon Resource Name (ARN) of the IAM OpenID Connect (OIDC) provider
-- to get information for. You can get a list of OIDC provider ARNs by using
-- the ListOpenIDConnectProviders action.
goidcpOpenIDConnectProviderArn :: Lens' GetOpenIDConnectProvider Text
goidcpOpenIDConnectProviderArn =
    lens _goidcpOpenIDConnectProviderArn
        (\s a -> s { _goidcpOpenIDConnectProviderArn = a })

data GetOpenIDConnectProviderResponse = GetOpenIDConnectProviderResponse
    { _goidcprClientIDList   :: [Text]
    , _goidcprCreateDate     :: Maybe RFC822
    , _goidcprThumbprintList :: [Text]
    , _goidcprUrl            :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetOpenIDConnectProviderResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'goidcprClientIDList' @::@ ['Text']
--
-- * 'goidcprCreateDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'goidcprThumbprintList' @::@ ['Text']
--
-- * 'goidcprUrl' @::@ 'Maybe' 'Text'
--
getOpenIDConnectProviderResponse :: GetOpenIDConnectProviderResponse
getOpenIDConnectProviderResponse = GetOpenIDConnectProviderResponse
    { _goidcprUrl            = Nothing
    , _goidcprClientIDList   = mempty
    , _goidcprThumbprintList = mempty
    , _goidcprCreateDate     = Nothing
    }

-- | A list of client IDs (also known as audiences) that are associated with
-- the specified IAM OpenID Connect provider. For more information, see
-- CreateOpenIDConnectProvider.
goidcprClientIDList :: Lens' GetOpenIDConnectProviderResponse [Text]
goidcprClientIDList =
    lens _goidcprClientIDList (\s a -> s { _goidcprClientIDList = a })

-- | The date and time when the IAM OpenID Connect provider entity was created
-- in the AWS account.
goidcprCreateDate :: Lens' GetOpenIDConnectProviderResponse (Maybe UTCTime)
goidcprCreateDate =
    lens _goidcprCreateDate (\s a -> s { _goidcprCreateDate = a })
        . mapping _Time

-- | A list of certificate thumbprints that are associated with the specified
-- IAM OpenID Connect provider. For more information, see
-- CreateOpenIDConnectProvider.
goidcprThumbprintList :: Lens' GetOpenIDConnectProviderResponse [Text]
goidcprThumbprintList =
    lens _goidcprThumbprintList (\s a -> s { _goidcprThumbprintList = a })

-- | The URL that the IAM OpenID Connect provider is associated with. For more
-- information, see CreateOpenIDConnectProvider.
goidcprUrl :: Lens' GetOpenIDConnectProviderResponse (Maybe Text)
goidcprUrl = lens _goidcprUrl (\s a -> s { _goidcprUrl = a })

instance ToPath GetOpenIDConnectProvider where
    toPath = const "/"

instance ToQuery GetOpenIDConnectProvider

instance ToHeaders GetOpenIDConnectProvider

instance AWSRequest GetOpenIDConnectProvider where
    type Sv GetOpenIDConnectProvider = IAM
    type Rs GetOpenIDConnectProvider = GetOpenIDConnectProviderResponse

    request  = post "GetOpenIDConnectProvider"
    response = xmlResponse

instance FromXML GetOpenIDConnectProviderResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GetOpenIDConnectProviderResponse"
