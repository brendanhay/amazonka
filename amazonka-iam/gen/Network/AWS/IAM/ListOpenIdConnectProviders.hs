{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListOpenIdConnectProviders
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists information about the OpenID Connect providers in the AWS account.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListOpenIdConnectProviders.html>
module Network.AWS.IAM.ListOpenIdConnectProviders
    (
    -- * Request
      ListOpenIdConnectProviders
    -- ** Request constructor
    , listOpenIdConnectProviders

    -- * Response
    , ListOpenIdConnectProvidersResponse
    -- ** Response constructor
    , listOpenIdConnectProvidersResponse
    -- ** Response lenses
    , loicprsOpenIdConnectProviderList
    , loicprsStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listOpenIdConnectProviders' smart constructor.
data ListOpenIdConnectProviders =
    ListOpenIdConnectProviders'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListOpenIdConnectProviders' smart constructor.
listOpenIdConnectProviders :: ListOpenIdConnectProviders
listOpenIdConnectProviders = ListOpenIdConnectProviders'

instance AWSRequest ListOpenIdConnectProviders where
        type Sv ListOpenIdConnectProviders = IAM
        type Rs ListOpenIdConnectProviders =
             ListOpenIdConnectProvidersResponse
        request = postQuery
        response
          = receiveXMLWrapper
              "ListOpenIDConnectProvidersResult"
              (\ s h x ->
                 ListOpenIdConnectProvidersResponse' <$>
                   (x .@? "OpenIDConnectProviderList" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance ToHeaders ListOpenIdConnectProviders where
        toHeaders = const mempty

instance ToPath ListOpenIdConnectProviders where
        toPath = const "/"

instance ToQuery ListOpenIdConnectProviders where
        toQuery
          = const
              (mconcat
                 ["Action" =:
                    ("ListOpenIdConnectProviders" :: ByteString),
                  "Version" =: ("2010-05-08" :: ByteString)])

-- | Contains the response to a successful ListOpenIDConnectProviders
-- request.
--
-- /See:/ 'listOpenIdConnectProvidersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'loicprsOpenIdConnectProviderList'
--
-- * 'loicprsStatus'
data ListOpenIdConnectProvidersResponse = ListOpenIdConnectProvidersResponse'
    { _loicprsOpenIdConnectProviderList :: !(Maybe [OpenIdConnectProviderListEntry])
    , _loicprsStatus                    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListOpenIdConnectProvidersResponse' smart constructor.
listOpenIdConnectProvidersResponse :: Int -> ListOpenIdConnectProvidersResponse
listOpenIdConnectProvidersResponse pStatus_ =
    ListOpenIdConnectProvidersResponse'
    { _loicprsOpenIdConnectProviderList = Nothing
    , _loicprsStatus = pStatus_
    }

-- | The list of IAM OpenID Connect providers in the AWS account.
loicprsOpenIdConnectProviderList :: Lens' ListOpenIdConnectProvidersResponse [OpenIdConnectProviderListEntry]
loicprsOpenIdConnectProviderList = lens _loicprsOpenIdConnectProviderList (\ s a -> s{_loicprsOpenIdConnectProviderList = a}) . _Default . _Coerce;

-- | FIXME: Undocumented member.
loicprsStatus :: Lens' ListOpenIdConnectProvidersResponse Int
loicprsStatus = lens _loicprsStatus (\ s a -> s{_loicprsStatus = a});
