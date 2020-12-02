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
-- Module      : Network.AWS.CognitoIdentity.ListIdentityPools
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the Cognito identity pools registered for your account.
--
--
-- You must use AWS Developer credentials to call this API.
--
module Network.AWS.CognitoIdentity.ListIdentityPools
    (
    -- * Creating a Request
      listIdentityPools
    , ListIdentityPools
    -- * Request Lenses
    , lipNextToken
    , lipMaxResults

    -- * Destructuring the Response
    , listIdentityPoolsResponse
    , ListIdentityPoolsResponse
    -- * Response Lenses
    , liprsIdentityPools
    , liprsNextToken
    , liprsResponseStatus
    ) where

import Network.AWS.CognitoIdentity.Types
import Network.AWS.CognitoIdentity.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Input to the ListIdentityPools action.
--
--
--
-- /See:/ 'listIdentityPools' smart constructor.
data ListIdentityPools = ListIdentityPools'
  { _lipNextToken  :: !(Maybe Text)
  , _lipMaxResults :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListIdentityPools' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lipNextToken' - A pagination token.
--
-- * 'lipMaxResults' - The maximum number of identities to return.
listIdentityPools
    :: Natural -- ^ 'lipMaxResults'
    -> ListIdentityPools
listIdentityPools pMaxResults_ =
  ListIdentityPools'
    {_lipNextToken = Nothing, _lipMaxResults = _Nat # pMaxResults_}


-- | A pagination token.
lipNextToken :: Lens' ListIdentityPools (Maybe Text)
lipNextToken = lens _lipNextToken (\ s a -> s{_lipNextToken = a})

-- | The maximum number of identities to return.
lipMaxResults :: Lens' ListIdentityPools Natural
lipMaxResults = lens _lipMaxResults (\ s a -> s{_lipMaxResults = a}) . _Nat

instance AWSRequest ListIdentityPools where
        type Rs ListIdentityPools = ListIdentityPoolsResponse
        request = postJSON cognitoIdentity
        response
          = receiveJSON
              (\ s h x ->
                 ListIdentityPoolsResponse' <$>
                   (x .?> "IdentityPools" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListIdentityPools where

instance NFData ListIdentityPools where

instance ToHeaders ListIdentityPools where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityService.ListIdentityPools" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListIdentityPools where
        toJSON ListIdentityPools'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lipNextToken,
                  Just ("MaxResults" .= _lipMaxResults)])

instance ToPath ListIdentityPools where
        toPath = const "/"

instance ToQuery ListIdentityPools where
        toQuery = const mempty

-- | The result of a successful ListIdentityPools action.
--
--
--
-- /See:/ 'listIdentityPoolsResponse' smart constructor.
data ListIdentityPoolsResponse = ListIdentityPoolsResponse'
  { _liprsIdentityPools  :: !(Maybe [IdentityPoolShortDescription])
  , _liprsNextToken      :: !(Maybe Text)
  , _liprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListIdentityPoolsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'liprsIdentityPools' - The identity pools returned by the ListIdentityPools action.
--
-- * 'liprsNextToken' - A pagination token.
--
-- * 'liprsResponseStatus' - -- | The response status code.
listIdentityPoolsResponse
    :: Int -- ^ 'liprsResponseStatus'
    -> ListIdentityPoolsResponse
listIdentityPoolsResponse pResponseStatus_ =
  ListIdentityPoolsResponse'
    { _liprsIdentityPools = Nothing
    , _liprsNextToken = Nothing
    , _liprsResponseStatus = pResponseStatus_
    }


-- | The identity pools returned by the ListIdentityPools action.
liprsIdentityPools :: Lens' ListIdentityPoolsResponse [IdentityPoolShortDescription]
liprsIdentityPools = lens _liprsIdentityPools (\ s a -> s{_liprsIdentityPools = a}) . _Default . _Coerce

-- | A pagination token.
liprsNextToken :: Lens' ListIdentityPoolsResponse (Maybe Text)
liprsNextToken = lens _liprsNextToken (\ s a -> s{_liprsNextToken = a})

-- | -- | The response status code.
liprsResponseStatus :: Lens' ListIdentityPoolsResponse Int
liprsResponseStatus = lens _liprsResponseStatus (\ s a -> s{_liprsResponseStatus = a})

instance NFData ListIdentityPoolsResponse where
