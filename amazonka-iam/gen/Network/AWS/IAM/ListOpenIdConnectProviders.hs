{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListOpenIdConnectProviders
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about the IAM OpenID Connect (OIDC) provider resource objects defined in the AWS account.
--
--
module Network.AWS.IAM.ListOpenIdConnectProviders
    (
    -- * Creating a Request
      listOpenIdConnectProviders
    , ListOpenIdConnectProviders

    -- * Destructuring the Response
    , listOpenIdConnectProvidersResponse
    , ListOpenIdConnectProvidersResponse
    -- * Response Lenses
    , loicprsOpenIdConnectProviderList
    , loicprsResponseStatus
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listOpenIdConnectProviders' smart constructor.
data ListOpenIdConnectProviders =
  ListOpenIdConnectProviders'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListOpenIdConnectProviders' with the minimum fields required to make a request.
--
listOpenIdConnectProviders
    :: ListOpenIdConnectProviders
listOpenIdConnectProviders = ListOpenIdConnectProviders'


instance AWSRequest ListOpenIdConnectProviders where
        type Rs ListOpenIdConnectProviders =
             ListOpenIdConnectProvidersResponse
        request = postQuery iam
        response
          = receiveXMLWrapper
              "ListOpenIDConnectProvidersResult"
              (\ s h x ->
                 ListOpenIdConnectProvidersResponse' <$>
                   (x .@? "OpenIDConnectProviderList" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable ListOpenIdConnectProviders where

instance NFData ListOpenIdConnectProviders where

instance ToHeaders ListOpenIdConnectProviders where
        toHeaders = const mempty

instance ToPath ListOpenIdConnectProviders where
        toPath = const "/"

instance ToQuery ListOpenIdConnectProviders where
        toQuery
          = const
              (mconcat
                 ["Action" =:
                    ("ListOpenIDConnectProviders" :: ByteString),
                  "Version" =: ("2010-05-08" :: ByteString)])

-- | Contains the response to a successful 'ListOpenIDConnectProviders' request.
--
--
--
-- /See:/ 'listOpenIdConnectProvidersResponse' smart constructor.
data ListOpenIdConnectProvidersResponse = ListOpenIdConnectProvidersResponse'
  { _loicprsOpenIdConnectProviderList :: !(Maybe [OpenIdConnectProviderListEntry])
  , _loicprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListOpenIdConnectProvidersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'loicprsOpenIdConnectProviderList' - The list of IAM OIDC provider resource objects defined in the AWS account.
--
-- * 'loicprsResponseStatus' - -- | The response status code.
listOpenIdConnectProvidersResponse
    :: Int -- ^ 'loicprsResponseStatus'
    -> ListOpenIdConnectProvidersResponse
listOpenIdConnectProvidersResponse pResponseStatus_ =
  ListOpenIdConnectProvidersResponse'
    { _loicprsOpenIdConnectProviderList = Nothing
    , _loicprsResponseStatus = pResponseStatus_
    }


-- | The list of IAM OIDC provider resource objects defined in the AWS account.
loicprsOpenIdConnectProviderList :: Lens' ListOpenIdConnectProvidersResponse [OpenIdConnectProviderListEntry]
loicprsOpenIdConnectProviderList = lens _loicprsOpenIdConnectProviderList (\ s a -> s{_loicprsOpenIdConnectProviderList = a}) . _Default . _Coerce

-- | -- | The response status code.
loicprsResponseStatus :: Lens' ListOpenIdConnectProvidersResponse Int
loicprsResponseStatus = lens _loicprsResponseStatus (\ s a -> s{_loicprsResponseStatus = a})

instance NFData ListOpenIdConnectProvidersResponse
         where
