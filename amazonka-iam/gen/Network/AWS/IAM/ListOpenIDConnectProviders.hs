{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.IAM.ListOpenIDConnectProviders
-- Copyright   : (c) 2013-2015 Brendan Hay
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
    , loidcprStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listOpenIDConnectProviders' smart constructor.
data ListOpenIDConnectProviders =
    ListOpenIDConnectProviders'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListOpenIDConnectProviders' smart constructor.
listOpenIDConnectProviders :: ListOpenIDConnectProviders
listOpenIDConnectProviders = ListOpenIDConnectProviders'

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
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

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

-- | Contains the response to a successful ListOpenIDConnectProviders
-- request.
--
-- /See:/ 'listOpenIDConnectProvidersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'loidcprOpenIDConnectProviderList'
--
-- * 'loidcprStatus'
data ListOpenIDConnectProvidersResponse = ListOpenIDConnectProvidersResponse'
    { _loidcprOpenIDConnectProviderList :: !(Maybe [OpenIDConnectProviderListEntry])
    , _loidcprStatus                    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListOpenIDConnectProvidersResponse' smart constructor.
listOpenIDConnectProvidersResponse :: Int -> ListOpenIDConnectProvidersResponse
listOpenIDConnectProvidersResponse pStatus =
    ListOpenIDConnectProvidersResponse'
    { _loidcprOpenIDConnectProviderList = Nothing
    , _loidcprStatus = pStatus
    }

-- | The list of IAM OpenID Connect providers in the AWS account.
loidcprOpenIDConnectProviderList :: Lens' ListOpenIDConnectProvidersResponse [OpenIDConnectProviderListEntry]
loidcprOpenIDConnectProviderList = lens _loidcprOpenIDConnectProviderList (\ s a -> s{_loidcprOpenIDConnectProviderList = a}) . _Default;

-- | FIXME: Undocumented member.
loidcprStatus :: Lens' ListOpenIDConnectProvidersResponse Int
loidcprStatus = lens _loidcprStatus (\ s a -> s{_loidcprStatus = a});
