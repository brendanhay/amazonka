{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.GetOpenIDConnectProvider
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
    , goidcpOpenIDConnectProviderARN

    -- * Response
    , GetOpenIDConnectProviderResponse
    -- ** Response constructor
    , getOpenIDConnectProviderResponse
    -- ** Response lenses
    , goidcprCreateDate
    , goidcprURL
    , goidcprThumbprintList
    , goidcprClientIDList
    ) where

import Network.AWS.IAM.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getOpenIDConnectProvider' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'goidcpOpenIDConnectProviderARN'
newtype GetOpenIDConnectProvider = GetOpenIDConnectProvider'{_goidcpOpenIDConnectProviderARN :: Text} deriving (Eq, Read, Show)

-- | 'GetOpenIDConnectProvider' smart constructor.
getOpenIDConnectProvider :: Text -> GetOpenIDConnectProvider
getOpenIDConnectProvider pOpenIDConnectProviderARN = GetOpenIDConnectProvider'{_goidcpOpenIDConnectProviderARN = pOpenIDConnectProviderARN};

-- | The Amazon Resource Name (ARN) of the IAM OpenID Connect (OIDC) provider
-- to get information for. You can get a list of OIDC provider ARNs by
-- using the ListOpenIDConnectProviders action.
goidcpOpenIDConnectProviderARN :: Lens' GetOpenIDConnectProvider Text
goidcpOpenIDConnectProviderARN = lens _goidcpOpenIDConnectProviderARN (\ s a -> s{_goidcpOpenIDConnectProviderARN = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest GetOpenIDConnectProvider where
        type Sv GetOpenIDConnectProvider = IAM
        type Rs GetOpenIDConnectProvider =
             GetOpenIDConnectProviderResponse
        request = post
        response
          = receiveXMLWrapper "GetOpenIDConnectProviderResult"
              (\ s h x ->
                 GetOpenIDConnectProviderResponse' <$>
                   (x .@? "CreateDate") <*> (x .@? "Url") <*>
                     (x .@? "ThumbprintList" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*>
                     (x .@? "ClientIDList" .!@ mempty >>=
                        may (parseXMLList "member")))

instance ToHeaders GetOpenIDConnectProvider where
        toHeaders = const mempty

instance ToPath GetOpenIDConnectProvider where
        toPath = const "/"

instance ToQuery GetOpenIDConnectProvider where
        toQuery GetOpenIDConnectProvider'{..}
          = mconcat
              ["Action" =:
                 ("GetOpenIDConnectProvider" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "OpenIDConnectProviderArn" =:
                 _goidcpOpenIDConnectProviderARN]

-- | /See:/ 'getOpenIDConnectProviderResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'goidcprCreateDate'
--
-- * 'goidcprURL'
--
-- * 'goidcprThumbprintList'
--
-- * 'goidcprClientIDList'
data GetOpenIDConnectProviderResponse = GetOpenIDConnectProviderResponse'{_goidcprCreateDate :: Maybe ISO8601, _goidcprURL :: Maybe Text, _goidcprThumbprintList :: Maybe [Text], _goidcprClientIDList :: Maybe [Text]} deriving (Eq, Read, Show)

-- | 'GetOpenIDConnectProviderResponse' smart constructor.
getOpenIDConnectProviderResponse :: GetOpenIDConnectProviderResponse
getOpenIDConnectProviderResponse = GetOpenIDConnectProviderResponse'{_goidcprCreateDate = Nothing, _goidcprURL = Nothing, _goidcprThumbprintList = Nothing, _goidcprClientIDList = Nothing};

-- | The date and time when the IAM OpenID Connect provider entity was
-- created in the AWS account.
goidcprCreateDate :: Lens' GetOpenIDConnectProviderResponse (Maybe UTCTime)
goidcprCreateDate = lens _goidcprCreateDate (\ s a -> s{_goidcprCreateDate = a}) . mapping _Time;

-- | The URL that the IAM OpenID Connect provider is associated with. For
-- more information, see CreateOpenIDConnectProvider.
goidcprURL :: Lens' GetOpenIDConnectProviderResponse (Maybe Text)
goidcprURL = lens _goidcprURL (\ s a -> s{_goidcprURL = a});

-- | A list of certificate thumbprints that are associated with the specified
-- IAM OpenID Connect provider. For more information, see
-- CreateOpenIDConnectProvider.
goidcprThumbprintList :: Lens' GetOpenIDConnectProviderResponse [Text]
goidcprThumbprintList = lens _goidcprThumbprintList (\ s a -> s{_goidcprThumbprintList = a}) . _Default;

-- | A list of client IDs (also known as audiences) that are associated with
-- the specified IAM OpenID Connect provider. For more information, see
-- CreateOpenIDConnectProvider.
goidcprClientIDList :: Lens' GetOpenIDConnectProviderResponse [Text]
goidcprClientIDList = lens _goidcprClientIDList (\ s a -> s{_goidcprClientIDList = a}) . _Default;
