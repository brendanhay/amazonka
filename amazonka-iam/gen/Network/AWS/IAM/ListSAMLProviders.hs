{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.ListSAMLProviders
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

-- | Lists the SAML providers in the account.
--
-- This operation requires
-- <http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4>.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListSAMLProviders.html>
module Network.AWS.IAM.ListSAMLProviders
    (
    -- * Request
      ListSAMLProviders
    -- ** Request constructor
    , listSAMLProviders

    -- * Response
    , ListSAMLProvidersResponse
    -- ** Response constructor
    , listSAMLProvidersResponse
    -- ** Response lenses
    , lsamlprSAMLProviderList
    , lsamlprStatusCode
    ) where

import Network.AWS.IAM.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listSAMLProviders' smart constructor.
data ListSAMLProviders = ListSAMLProviders' deriving (Eq, Read, Show)

-- | 'ListSAMLProviders' smart constructor.
listSAMLProviders :: ListSAMLProviders
listSAMLProviders = ListSAMLProviders';

instance AWSRequest ListSAMLProviders where
        type Sv ListSAMLProviders = IAM
        type Rs ListSAMLProviders = ListSAMLProvidersResponse
        request = post
        response
          = receiveXMLWrapper "ListSAMLProvidersResult"
              (\ s h x ->
                 ListSAMLProvidersResponse' <$>
                   (x .@? "SAMLProviderList" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance ToHeaders ListSAMLProviders where
        toHeaders = const mempty

instance ToPath ListSAMLProviders where
        toPath = const "/"

instance ToQuery ListSAMLProviders where
        toQuery
          = const
              (mconcat
                 ["Action" =: ("ListSAMLProviders" :: ByteString),
                  "Version" =: ("2010-05-08" :: ByteString)])

-- | Contains the response to a successful ListSAMLProviders request.
--
-- /See:/ 'listSAMLProvidersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsamlprSAMLProviderList'
--
-- * 'lsamlprStatusCode'
data ListSAMLProvidersResponse = ListSAMLProvidersResponse'{_lsamlprSAMLProviderList :: Maybe [SAMLProviderListEntry], _lsamlprStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'ListSAMLProvidersResponse' smart constructor.
listSAMLProvidersResponse :: Int -> ListSAMLProvidersResponse
listSAMLProvidersResponse pStatusCode = ListSAMLProvidersResponse'{_lsamlprSAMLProviderList = Nothing, _lsamlprStatusCode = pStatusCode};

-- | The list of SAML providers for this account.
lsamlprSAMLProviderList :: Lens' ListSAMLProvidersResponse [SAMLProviderListEntry]
lsamlprSAMLProviderList = lens _lsamlprSAMLProviderList (\ s a -> s{_lsamlprSAMLProviderList = a}) . _Default;

-- | FIXME: Undocumented member.
lsamlprStatusCode :: Lens' ListSAMLProvidersResponse Int
lsamlprStatusCode = lens _lsamlprStatusCode (\ s a -> s{_lsamlprStatusCode = a});
