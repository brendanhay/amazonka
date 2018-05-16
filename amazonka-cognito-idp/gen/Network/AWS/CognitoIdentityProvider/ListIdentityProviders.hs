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
-- Module      : Network.AWS.CognitoIdentityProvider.ListIdentityProviders
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about all identity providers for a user pool.
--
--
module Network.AWS.CognitoIdentityProvider.ListIdentityProviders
    (
    -- * Creating a Request
      listIdentityProviders
    , ListIdentityProviders
    -- * Request Lenses
    , lipNextToken
    , lipMaxResults
    , lipUserPoolId

    -- * Destructuring the Response
    , listIdentityProvidersResponse
    , ListIdentityProvidersResponse
    -- * Response Lenses
    , liprsNextToken
    , liprsResponseStatus
    , liprsProviders
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listIdentityProviders' smart constructor.
data ListIdentityProviders = ListIdentityProviders'
  { _lipNextToken  :: !(Maybe Text)
  , _lipMaxResults :: !(Maybe Nat)
  , _lipUserPoolId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListIdentityProviders' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lipNextToken' - A pagination token.
--
-- * 'lipMaxResults' - The maximum number of identity providers to return.
--
-- * 'lipUserPoolId' - The user pool ID.
listIdentityProviders
    :: Text -- ^ 'lipUserPoolId'
    -> ListIdentityProviders
listIdentityProviders pUserPoolId_ =
  ListIdentityProviders'
    { _lipNextToken = Nothing
    , _lipMaxResults = Nothing
    , _lipUserPoolId = pUserPoolId_
    }


-- | A pagination token.
lipNextToken :: Lens' ListIdentityProviders (Maybe Text)
lipNextToken = lens _lipNextToken (\ s a -> s{_lipNextToken = a})

-- | The maximum number of identity providers to return.
lipMaxResults :: Lens' ListIdentityProviders (Maybe Natural)
lipMaxResults = lens _lipMaxResults (\ s a -> s{_lipMaxResults = a}) . mapping _Nat

-- | The user pool ID.
lipUserPoolId :: Lens' ListIdentityProviders Text
lipUserPoolId = lens _lipUserPoolId (\ s a -> s{_lipUserPoolId = a})

instance AWSRequest ListIdentityProviders where
        type Rs ListIdentityProviders =
             ListIdentityProvidersResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 ListIdentityProvidersResponse' <$>
                   (x .?> "NextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "Providers" .!@ mempty))

instance Hashable ListIdentityProviders where

instance NFData ListIdentityProviders where

instance ToHeaders ListIdentityProviders where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.ListIdentityProviders"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListIdentityProviders where
        toJSON ListIdentityProviders'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lipNextToken,
                  ("MaxResults" .=) <$> _lipMaxResults,
                  Just ("UserPoolId" .= _lipUserPoolId)])

instance ToPath ListIdentityProviders where
        toPath = const "/"

instance ToQuery ListIdentityProviders where
        toQuery = const mempty

-- | /See:/ 'listIdentityProvidersResponse' smart constructor.
data ListIdentityProvidersResponse = ListIdentityProvidersResponse'
  { _liprsNextToken      :: !(Maybe Text)
  , _liprsResponseStatus :: !Int
  , _liprsProviders      :: ![ProviderDescription]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListIdentityProvidersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'liprsNextToken' - A pagination token.
--
-- * 'liprsResponseStatus' - -- | The response status code.
--
-- * 'liprsProviders' - A list of identity provider objects.
listIdentityProvidersResponse
    :: Int -- ^ 'liprsResponseStatus'
    -> ListIdentityProvidersResponse
listIdentityProvidersResponse pResponseStatus_ =
  ListIdentityProvidersResponse'
    { _liprsNextToken = Nothing
    , _liprsResponseStatus = pResponseStatus_
    , _liprsProviders = mempty
    }


-- | A pagination token.
liprsNextToken :: Lens' ListIdentityProvidersResponse (Maybe Text)
liprsNextToken = lens _liprsNextToken (\ s a -> s{_liprsNextToken = a})

-- | -- | The response status code.
liprsResponseStatus :: Lens' ListIdentityProvidersResponse Int
liprsResponseStatus = lens _liprsResponseStatus (\ s a -> s{_liprsResponseStatus = a})

-- | A list of identity provider objects.
liprsProviders :: Lens' ListIdentityProvidersResponse [ProviderDescription]
liprsProviders = lens _liprsProviders (\ s a -> s{_liprsProviders = a}) . _Coerce

instance NFData ListIdentityProvidersResponse where
