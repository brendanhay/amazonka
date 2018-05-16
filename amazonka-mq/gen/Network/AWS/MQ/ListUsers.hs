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
-- Module      : Network.AWS.MQ.ListUsers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all ActiveMQ users.
module Network.AWS.MQ.ListUsers
    (
    -- * Creating a Request
      listUsers
    , ListUsers
    -- * Request Lenses
    , luNextToken
    , luMaxResults
    , luBrokerId

    -- * Destructuring the Response
    , listUsersResponse
    , ListUsersResponse
    -- * Response Lenses
    , lursUsers
    , lursNextToken
    , lursBrokerId
    , lursMaxResults
    , lursResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MQ.Types
import Network.AWS.MQ.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listUsers' smart constructor.
data ListUsers = ListUsers'
  { _luNextToken  :: !(Maybe Text)
  , _luMaxResults :: !(Maybe Nat)
  , _luBrokerId   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListUsers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'luNextToken' - The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- * 'luMaxResults' - The maximum number of ActiveMQ users that can be returned per page (20 by default). This value must be an integer from 5 to 100.
--
-- * 'luBrokerId' - The unique ID that Amazon MQ generates for the broker.
listUsers
    :: Text -- ^ 'luBrokerId'
    -> ListUsers
listUsers pBrokerId_ =
  ListUsers'
    {_luNextToken = Nothing, _luMaxResults = Nothing, _luBrokerId = pBrokerId_}


-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
luNextToken :: Lens' ListUsers (Maybe Text)
luNextToken = lens _luNextToken (\ s a -> s{_luNextToken = a})

-- | The maximum number of ActiveMQ users that can be returned per page (20 by default). This value must be an integer from 5 to 100.
luMaxResults :: Lens' ListUsers (Maybe Natural)
luMaxResults = lens _luMaxResults (\ s a -> s{_luMaxResults = a}) . mapping _Nat

-- | The unique ID that Amazon MQ generates for the broker.
luBrokerId :: Lens' ListUsers Text
luBrokerId = lens _luBrokerId (\ s a -> s{_luBrokerId = a})

instance AWSRequest ListUsers where
        type Rs ListUsers = ListUsersResponse
        request = get mq
        response
          = receiveJSON
              (\ s h x ->
                 ListUsersResponse' <$>
                   (x .?> "users" .!@ mempty) <*> (x .?> "nextToken")
                     <*> (x .?> "brokerId")
                     <*> (x .?> "maxResults")
                     <*> (pure (fromEnum s)))

instance Hashable ListUsers where

instance NFData ListUsers where

instance ToHeaders ListUsers where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListUsers where
        toPath ListUsers'{..}
          = mconcat
              ["/v1/brokers/", toBS _luBrokerId, "/users"]

instance ToQuery ListUsers where
        toQuery ListUsers'{..}
          = mconcat
              ["nextToken" =: _luNextToken,
               "maxResults" =: _luMaxResults]

-- | /See:/ 'listUsersResponse' smart constructor.
data ListUsersResponse = ListUsersResponse'
  { _lursUsers          :: !(Maybe [UserSummary])
  , _lursNextToken      :: !(Maybe Text)
  , _lursBrokerId       :: !(Maybe Text)
  , _lursMaxResults     :: !(Maybe Int)
  , _lursResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListUsersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lursUsers' - Required. The list of all ActiveMQ usernames for the specified broker.
--
-- * 'lursNextToken' - The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- * 'lursBrokerId' - Required. The unique ID that Amazon MQ generates for the broker.
--
-- * 'lursMaxResults' - Required. The maximum number of ActiveMQ users that can be returned per page (20 by default). This value must be an integer from 5 to 100.
--
-- * 'lursResponseStatus' - -- | The response status code.
listUsersResponse
    :: Int -- ^ 'lursResponseStatus'
    -> ListUsersResponse
listUsersResponse pResponseStatus_ =
  ListUsersResponse'
    { _lursUsers = Nothing
    , _lursNextToken = Nothing
    , _lursBrokerId = Nothing
    , _lursMaxResults = Nothing
    , _lursResponseStatus = pResponseStatus_
    }


-- | Required. The list of all ActiveMQ usernames for the specified broker.
lursUsers :: Lens' ListUsersResponse [UserSummary]
lursUsers = lens _lursUsers (\ s a -> s{_lursUsers = a}) . _Default . _Coerce

-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
lursNextToken :: Lens' ListUsersResponse (Maybe Text)
lursNextToken = lens _lursNextToken (\ s a -> s{_lursNextToken = a})

-- | Required. The unique ID that Amazon MQ generates for the broker.
lursBrokerId :: Lens' ListUsersResponse (Maybe Text)
lursBrokerId = lens _lursBrokerId (\ s a -> s{_lursBrokerId = a})

-- | Required. The maximum number of ActiveMQ users that can be returned per page (20 by default). This value must be an integer from 5 to 100.
lursMaxResults :: Lens' ListUsersResponse (Maybe Int)
lursMaxResults = lens _lursMaxResults (\ s a -> s{_lursMaxResults = a})

-- | -- | The response status code.
lursResponseStatus :: Lens' ListUsersResponse Int
lursResponseStatus = lens _lursResponseStatus (\ s a -> s{_lursResponseStatus = a})

instance NFData ListUsersResponse where
