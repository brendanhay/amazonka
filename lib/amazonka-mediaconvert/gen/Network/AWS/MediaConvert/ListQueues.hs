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
-- Module      : Network.AWS.MediaConvert.ListQueues
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a JSON array of up to twenty of your queues. This will return the queues themselves, not just a list of them. To retrieve the next twenty queues, use the nextToken string returned with the array.
module Network.AWS.MediaConvert.ListQueues
    (
    -- * Creating a Request
      listQueues
    , ListQueues
    -- * Request Lenses
    , lqListBy
    , lqNextToken
    , lqOrder
    , lqMaxResults

    -- * Destructuring the Response
    , listQueuesResponse
    , ListQueuesResponse
    -- * Response Lenses
    , lqrsQueues
    , lqrsNextToken
    , lqrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types
import Network.AWS.MediaConvert.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listQueues' smart constructor.
data ListQueues = ListQueues'
  { _lqListBy     :: !(Maybe QueueListBy)
  , _lqNextToken  :: !(Maybe Text)
  , _lqOrder      :: !(Maybe Order)
  , _lqMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListQueues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lqListBy' - Undocumented member.
--
-- * 'lqNextToken' - Use this string, provided with the response to a previous request, to request the next batch of queues.
--
-- * 'lqOrder' - Undocumented member.
--
-- * 'lqMaxResults' - Optional. Number of queues, up to twenty, that will be returned at one time.
listQueues
    :: ListQueues
listQueues =
  ListQueues'
    { _lqListBy = Nothing
    , _lqNextToken = Nothing
    , _lqOrder = Nothing
    , _lqMaxResults = Nothing
    }


-- | Undocumented member.
lqListBy :: Lens' ListQueues (Maybe QueueListBy)
lqListBy = lens _lqListBy (\ s a -> s{_lqListBy = a})

-- | Use this string, provided with the response to a previous request, to request the next batch of queues.
lqNextToken :: Lens' ListQueues (Maybe Text)
lqNextToken = lens _lqNextToken (\ s a -> s{_lqNextToken = a})

-- | Undocumented member.
lqOrder :: Lens' ListQueues (Maybe Order)
lqOrder = lens _lqOrder (\ s a -> s{_lqOrder = a})

-- | Optional. Number of queues, up to twenty, that will be returned at one time.
lqMaxResults :: Lens' ListQueues (Maybe Int)
lqMaxResults = lens _lqMaxResults (\ s a -> s{_lqMaxResults = a})

instance AWSRequest ListQueues where
        type Rs ListQueues = ListQueuesResponse
        request = get mediaConvert
        response
          = receiveJSON
              (\ s h x ->
                 ListQueuesResponse' <$>
                   (x .?> "queues" .!@ mempty) <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListQueues where

instance NFData ListQueues where

instance ToHeaders ListQueues where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListQueues where
        toPath = const "/2017-08-29/queues"

instance ToQuery ListQueues where
        toQuery ListQueues'{..}
          = mconcat
              ["listBy" =: _lqListBy, "nextToken" =: _lqNextToken,
               "order" =: _lqOrder, "maxResults" =: _lqMaxResults]

-- | /See:/ 'listQueuesResponse' smart constructor.
data ListQueuesResponse = ListQueuesResponse'
  { _lqrsQueues         :: !(Maybe [Queue])
  , _lqrsNextToken      :: !(Maybe Text)
  , _lqrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListQueuesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lqrsQueues' - List of queues
--
-- * 'lqrsNextToken' - Use this string to request the next batch of queues.
--
-- * 'lqrsResponseStatus' - -- | The response status code.
listQueuesResponse
    :: Int -- ^ 'lqrsResponseStatus'
    -> ListQueuesResponse
listQueuesResponse pResponseStatus_ =
  ListQueuesResponse'
    { _lqrsQueues = Nothing
    , _lqrsNextToken = Nothing
    , _lqrsResponseStatus = pResponseStatus_
    }


-- | List of queues
lqrsQueues :: Lens' ListQueuesResponse [Queue]
lqrsQueues = lens _lqrsQueues (\ s a -> s{_lqrsQueues = a}) . _Default . _Coerce

-- | Use this string to request the next batch of queues.
lqrsNextToken :: Lens' ListQueuesResponse (Maybe Text)
lqrsNextToken = lens _lqrsNextToken (\ s a -> s{_lqrsNextToken = a})

-- | -- | The response status code.
lqrsResponseStatus :: Lens' ListQueuesResponse Int
lqrsResponseStatus = lens _lqrsResponseStatus (\ s a -> s{_lqrsResponseStatus = a})

instance NFData ListQueuesResponse where
