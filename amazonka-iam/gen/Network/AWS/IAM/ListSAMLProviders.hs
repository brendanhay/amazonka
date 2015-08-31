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
-- Module      : Network.AWS.IAM.ListSAMLProviders
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the SAML providers in the account.
--
-- This operation requires
-- <http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4>.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListSAMLProviders.html AWS API Reference> for ListSAMLProviders.
module Network.AWS.IAM.ListSAMLProviders
    (
    -- * Creating a Request
      listSAMLProviders
    , ListSAMLProviders

    -- * Destructuring the Response
    , listSAMLProvidersResponse
    , ListSAMLProvidersResponse
    -- * Response Lenses
    , lsamlprsSAMLProviderList
    , lsamlprsResponseStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listSAMLProviders' smart constructor.
data ListSAMLProviders =
    ListSAMLProviders'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListSAMLProviders' with the minimum fields required to make a request.
--
listSAMLProviders
    :: ListSAMLProviders
listSAMLProviders = ListSAMLProviders'

instance AWSRequest ListSAMLProviders where
        type Rs ListSAMLProviders = ListSAMLProvidersResponse
        request = postQuery iAM
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
data ListSAMLProvidersResponse = ListSAMLProvidersResponse'
    { _lsamlprsSAMLProviderList :: !(Maybe [SAMLProviderListEntry])
    , _lsamlprsResponseStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListSAMLProvidersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsamlprsSAMLProviderList'
--
-- * 'lsamlprsResponseStatus'
listSAMLProvidersResponse
    :: Int -- ^ 'lsamlprsResponseStatus'
    -> ListSAMLProvidersResponse
listSAMLProvidersResponse pResponseStatus_ =
    ListSAMLProvidersResponse'
    { _lsamlprsSAMLProviderList = Nothing
    , _lsamlprsResponseStatus = pResponseStatus_
    }

-- | The list of SAML providers for this account.
lsamlprsSAMLProviderList :: Lens' ListSAMLProvidersResponse [SAMLProviderListEntry]
lsamlprsSAMLProviderList = lens _lsamlprsSAMLProviderList (\ s a -> s{_lsamlprsSAMLProviderList = a}) . _Default . _Coerce;

-- | The response status code.
lsamlprsResponseStatus :: Lens' ListSAMLProvidersResponse Int
lsamlprsResponseStatus = lens _lsamlprsResponseStatus (\ s a -> s{_lsamlprsResponseStatus = a});
