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
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.IAM.Types

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
                      parseXMLList "member"))

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

-- | /See:/ 'listSAMLProvidersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsamlprSAMLProviderList'
newtype ListSAMLProvidersResponse = ListSAMLProvidersResponse'{_lsamlprSAMLProviderList :: Maybe [SAMLProviderListEntry]} deriving (Eq, Read, Show)

-- | 'ListSAMLProvidersResponse' smart constructor.
listSAMLProvidersResponse :: ListSAMLProvidersResponse
listSAMLProvidersResponse = ListSAMLProvidersResponse'{_lsamlprSAMLProviderList = Nothing};

-- | The list of SAML providers for this account.
lsamlprSAMLProviderList :: Lens' ListSAMLProvidersResponse (Maybe [SAMLProviderListEntry])
lsamlprSAMLProviderList = lens _lsamlprSAMLProviderList (\ s a -> s{_lsamlprSAMLProviderList = a});
