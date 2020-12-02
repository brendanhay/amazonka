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
-- Module      : Network.AWS.Cloud9.ListEnvironments
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of AWS Cloud9 development environment identifiers.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Cloud9.ListEnvironments
    (
    -- * Creating a Request
      listEnvironments
    , ListEnvironments
    -- * Request Lenses
    , leNextToken
    , leMaxResults

    -- * Destructuring the Response
    , listEnvironmentsResponse
    , ListEnvironmentsResponse
    -- * Response Lenses
    , lersEnvironmentIds
    , lersNextToken
    , lersResponseStatus
    ) where

import Network.AWS.Cloud9.Types
import Network.AWS.Cloud9.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listEnvironments' smart constructor.
data ListEnvironments = ListEnvironments'
  { _leNextToken  :: !(Maybe Text)
  , _leMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListEnvironments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'leNextToken' - During a previous call, if there are more than 25 items in the list, only the first 25 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- * 'leMaxResults' - The maximum number of environments to get identifiers for.
listEnvironments
    :: ListEnvironments
listEnvironments =
  ListEnvironments' {_leNextToken = Nothing, _leMaxResults = Nothing}


-- | During a previous call, if there are more than 25 items in the list, only the first 25 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
leNextToken :: Lens' ListEnvironments (Maybe Text)
leNextToken = lens _leNextToken (\ s a -> s{_leNextToken = a})

-- | The maximum number of environments to get identifiers for.
leMaxResults :: Lens' ListEnvironments (Maybe Natural)
leMaxResults = lens _leMaxResults (\ s a -> s{_leMaxResults = a}) . mapping _Nat

instance AWSPager ListEnvironments where
        page rq rs
          | stop (rs ^. lersNextToken) = Nothing
          | stop (rs ^. lersEnvironmentIds) = Nothing
          | otherwise =
            Just $ rq & leNextToken .~ rs ^. lersNextToken

instance AWSRequest ListEnvironments where
        type Rs ListEnvironments = ListEnvironmentsResponse
        request = postJSON cloud9
        response
          = receiveJSON
              (\ s h x ->
                 ListEnvironmentsResponse' <$>
                   (x .?> "environmentIds" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListEnvironments where

instance NFData ListEnvironments where

instance ToHeaders ListEnvironments where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCloud9WorkspaceManagementService.ListEnvironments"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListEnvironments where
        toJSON ListEnvironments'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _leNextToken,
                  ("maxResults" .=) <$> _leMaxResults])

instance ToPath ListEnvironments where
        toPath = const "/"

instance ToQuery ListEnvironments where
        toQuery = const mempty

-- | /See:/ 'listEnvironmentsResponse' smart constructor.
data ListEnvironmentsResponse = ListEnvironmentsResponse'
  { _lersEnvironmentIds :: !(Maybe [Text])
  , _lersNextToken      :: !(Maybe Text)
  , _lersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListEnvironmentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lersEnvironmentIds' - The list of environment identifiers.
--
-- * 'lersNextToken' - If there are more than 25 items in the list, only the first 25 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call.
--
-- * 'lersResponseStatus' - -- | The response status code.
listEnvironmentsResponse
    :: Int -- ^ 'lersResponseStatus'
    -> ListEnvironmentsResponse
listEnvironmentsResponse pResponseStatus_ =
  ListEnvironmentsResponse'
    { _lersEnvironmentIds = Nothing
    , _lersNextToken = Nothing
    , _lersResponseStatus = pResponseStatus_
    }


-- | The list of environment identifiers.
lersEnvironmentIds :: Lens' ListEnvironmentsResponse [Text]
lersEnvironmentIds = lens _lersEnvironmentIds (\ s a -> s{_lersEnvironmentIds = a}) . _Default . _Coerce

-- | If there are more than 25 items in the list, only the first 25 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call.
lersNextToken :: Lens' ListEnvironmentsResponse (Maybe Text)
lersNextToken = lens _lersNextToken (\ s a -> s{_lersNextToken = a})

-- | -- | The response status code.
lersResponseStatus :: Lens' ListEnvironmentsResponse Int
lersResponseStatus = lens _lersResponseStatus (\ s a -> s{_lersResponseStatus = a})

instance NFData ListEnvironmentsResponse where
