{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.ListOpenIDConnectProviders
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

-- | Lists information about the OpenID Connect providers in the AWS account.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListOpenIDConnectProviders.html>
module Network.AWS.IAM.ListOpenIDConnectProviders
    (
    -- * Request
      ListOpenIDConnectProviders
    -- ** Request constructor
    , listOpenIDConnectProviders

    -- * Response
    , ListOpenIDConnectProvidersResponse
    -- ** Response constructor
    , listOpenIDConnectProvidersResponse
    -- ** Response lenses
    , loidcprOpenIDConnectProviderList
    ) where

import Network.AWS.IAM.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listOpenIDConnectProviders' smart constructor.
data ListOpenIDConnectProviders = ListOpenIDConnectProviders' deriving (Eq, Read, Show)

-- | 'ListOpenIDConnectProviders' smart constructor.
listOpenIDConnectProviders :: ListOpenIDConnectProviders
listOpenIDConnectProviders = ListOpenIDConnectProviders';

instance AWSRequest ListOpenIDConnectProviders where
        type Sv ListOpenIDConnectProviders = IAM
        type Rs ListOpenIDConnectProviders =
             ListOpenIDConnectProvidersResponse
        request = post
        response
          = receiveXMLWrapper
              "ListOpenIDConnectProvidersResult"
              (\ s h x ->
                 ListOpenIDConnectProvidersResponse' <$>
                   (x .@? "OpenIDConnectProviderList" .!@ mempty >>=
                      may (parseXMLList "member")))

instance ToHeaders ListOpenIDConnectProviders where
        toHeaders = const mempty

instance ToPath ListOpenIDConnectProviders where
        toPath = const "/"

instance ToQuery ListOpenIDConnectProviders where
        toQuery
          = const
              (mconcat
                 ["Action" =:
                    ("ListOpenIDConnectProviders" :: ByteString),
                  "Version" =: ("2010-05-08" :: ByteString)])

-- | /See:/ 'listOpenIDConnectProvidersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'loidcprOpenIDConnectProviderList'
newtype ListOpenIDConnectProvidersResponse = ListOpenIDConnectProvidersResponse'{_loidcprOpenIDConnectProviderList :: Maybe [OpenIDConnectProviderListEntry]} deriving (Eq, Read, Show)

-- | 'ListOpenIDConnectProvidersResponse' smart constructor.
listOpenIDConnectProvidersResponse :: ListOpenIDConnectProvidersResponse
listOpenIDConnectProvidersResponse = ListOpenIDConnectProvidersResponse'{_loidcprOpenIDConnectProviderList = Nothing};

-- | The list of IAM OpenID Connect providers in the AWS account.
loidcprOpenIDConnectProviderList :: Lens' ListOpenIDConnectProvidersResponse [OpenIDConnectProviderListEntry]
loidcprOpenIDConnectProviderList = lens _loidcprOpenIDConnectProviderList (\ s a -> s{_loidcprOpenIDConnectProviderList = a}) . _Default;
