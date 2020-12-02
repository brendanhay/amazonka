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
-- Module      : Network.AWS.CognitoSync.ListIdentityPoolUsage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of identity pools registered with Cognito.
--
--
-- ListIdentityPoolUsage can only be called with developer credentials. You cannot make this API call with the temporary user credentials provided by Cognito Identity.
--
module Network.AWS.CognitoSync.ListIdentityPoolUsage
    (
    -- * Creating a Request
      listIdentityPoolUsage
    , ListIdentityPoolUsage
    -- * Request Lenses
    , lipuNextToken
    , lipuMaxResults

    -- * Destructuring the Response
    , listIdentityPoolUsageResponse
    , ListIdentityPoolUsageResponse
    -- * Response Lenses
    , lipursIdentityPoolUsages
    , lipursCount
    , lipursNextToken
    , lipursMaxResults
    , lipursResponseStatus
    ) where

import Network.AWS.CognitoSync.Types
import Network.AWS.CognitoSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A request for usage information on an identity pool.
--
-- /See:/ 'listIdentityPoolUsage' smart constructor.
data ListIdentityPoolUsage = ListIdentityPoolUsage'
  { _lipuNextToken  :: !(Maybe Text)
  , _lipuMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListIdentityPoolUsage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lipuNextToken' - A pagination token for obtaining the next page of results.
--
-- * 'lipuMaxResults' - The maximum number of results to be returned.
listIdentityPoolUsage
    :: ListIdentityPoolUsage
listIdentityPoolUsage =
  ListIdentityPoolUsage' {_lipuNextToken = Nothing, _lipuMaxResults = Nothing}


-- | A pagination token for obtaining the next page of results.
lipuNextToken :: Lens' ListIdentityPoolUsage (Maybe Text)
lipuNextToken = lens _lipuNextToken (\ s a -> s{_lipuNextToken = a})

-- | The maximum number of results to be returned.
lipuMaxResults :: Lens' ListIdentityPoolUsage (Maybe Int)
lipuMaxResults = lens _lipuMaxResults (\ s a -> s{_lipuMaxResults = a})

instance AWSRequest ListIdentityPoolUsage where
        type Rs ListIdentityPoolUsage =
             ListIdentityPoolUsageResponse
        request = get cognitoSync
        response
          = receiveJSON
              (\ s h x ->
                 ListIdentityPoolUsageResponse' <$>
                   (x .?> "IdentityPoolUsages" .!@ mempty) <*>
                     (x .?> "Count")
                     <*> (x .?> "NextToken")
                     <*> (x .?> "MaxResults")
                     <*> (pure (fromEnum s)))

instance Hashable ListIdentityPoolUsage where

instance NFData ListIdentityPoolUsage where

instance ToHeaders ListIdentityPoolUsage where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListIdentityPoolUsage where
        toPath = const "/identitypools"

instance ToQuery ListIdentityPoolUsage where
        toQuery ListIdentityPoolUsage'{..}
          = mconcat
              ["nextToken" =: _lipuNextToken,
               "maxResults" =: _lipuMaxResults]

-- | Returned for a successful ListIdentityPoolUsage request.
--
-- /See:/ 'listIdentityPoolUsageResponse' smart constructor.
data ListIdentityPoolUsageResponse = ListIdentityPoolUsageResponse'
  { _lipursIdentityPoolUsages :: !(Maybe [IdentityPoolUsage])
  , _lipursCount              :: !(Maybe Int)
  , _lipursNextToken          :: !(Maybe Text)
  , _lipursMaxResults         :: !(Maybe Int)
  , _lipursResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListIdentityPoolUsageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lipursIdentityPoolUsages' - Usage information for the identity pools.
--
-- * 'lipursCount' - Total number of identities for the identity pool.
--
-- * 'lipursNextToken' - A pagination token for obtaining the next page of results.
--
-- * 'lipursMaxResults' - The maximum number of results to be returned.
--
-- * 'lipursResponseStatus' - -- | The response status code.
listIdentityPoolUsageResponse
    :: Int -- ^ 'lipursResponseStatus'
    -> ListIdentityPoolUsageResponse
listIdentityPoolUsageResponse pResponseStatus_ =
  ListIdentityPoolUsageResponse'
    { _lipursIdentityPoolUsages = Nothing
    , _lipursCount = Nothing
    , _lipursNextToken = Nothing
    , _lipursMaxResults = Nothing
    , _lipursResponseStatus = pResponseStatus_
    }


-- | Usage information for the identity pools.
lipursIdentityPoolUsages :: Lens' ListIdentityPoolUsageResponse [IdentityPoolUsage]
lipursIdentityPoolUsages = lens _lipursIdentityPoolUsages (\ s a -> s{_lipursIdentityPoolUsages = a}) . _Default . _Coerce

-- | Total number of identities for the identity pool.
lipursCount :: Lens' ListIdentityPoolUsageResponse (Maybe Int)
lipursCount = lens _lipursCount (\ s a -> s{_lipursCount = a})

-- | A pagination token for obtaining the next page of results.
lipursNextToken :: Lens' ListIdentityPoolUsageResponse (Maybe Text)
lipursNextToken = lens _lipursNextToken (\ s a -> s{_lipursNextToken = a})

-- | The maximum number of results to be returned.
lipursMaxResults :: Lens' ListIdentityPoolUsageResponse (Maybe Int)
lipursMaxResults = lens _lipursMaxResults (\ s a -> s{_lipursMaxResults = a})

-- | -- | The response status code.
lipursResponseStatus :: Lens' ListIdentityPoolUsageResponse Int
lipursResponseStatus = lens _lipursResponseStatus (\ s a -> s{_lipursResponseStatus = a})

instance NFData ListIdentityPoolUsageResponse where
