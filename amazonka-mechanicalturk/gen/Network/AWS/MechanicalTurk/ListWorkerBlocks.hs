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
-- Module      : Network.AWS.MechanicalTurk.ListWorkerBlocks
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListWorkersBlocks@ operation retrieves a list of Workers who are blocked from working on your HITs.
--
--
--
-- This operation returns paginated results.
module Network.AWS.MechanicalTurk.ListWorkerBlocks
    (
    -- * Creating a Request
      listWorkerBlocks
    , ListWorkerBlocks
    -- * Request Lenses
    , lwbNextToken
    , lwbMaxResults

    -- * Destructuring the Response
    , listWorkerBlocksResponse
    , ListWorkerBlocksResponse
    -- * Response Lenses
    , lwbrsWorkerBlocks
    , lwbrsNextToken
    , lwbrsNumResults
    , lwbrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types
import Network.AWS.MechanicalTurk.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listWorkerBlocks' smart constructor.
data ListWorkerBlocks = ListWorkerBlocks'
  { _lwbNextToken  :: !(Maybe Text)
  , _lwbMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListWorkerBlocks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lwbNextToken' - Pagination token
--
-- * 'lwbMaxResults' - Undocumented member.
listWorkerBlocks
    :: ListWorkerBlocks
listWorkerBlocks =
  ListWorkerBlocks' {_lwbNextToken = Nothing, _lwbMaxResults = Nothing}


-- | Pagination token
lwbNextToken :: Lens' ListWorkerBlocks (Maybe Text)
lwbNextToken = lens _lwbNextToken (\ s a -> s{_lwbNextToken = a})

-- | Undocumented member.
lwbMaxResults :: Lens' ListWorkerBlocks (Maybe Natural)
lwbMaxResults = lens _lwbMaxResults (\ s a -> s{_lwbMaxResults = a}) . mapping _Nat

instance AWSPager ListWorkerBlocks where
        page rq rs
          | stop (rs ^. lwbrsNextToken) = Nothing
          | stop (rs ^. lwbrsWorkerBlocks) = Nothing
          | otherwise =
            Just $ rq & lwbNextToken .~ rs ^. lwbrsNextToken

instance AWSRequest ListWorkerBlocks where
        type Rs ListWorkerBlocks = ListWorkerBlocksResponse
        request = postJSON mechanicalTurk
        response
          = receiveJSON
              (\ s h x ->
                 ListWorkerBlocksResponse' <$>
                   (x .?> "WorkerBlocks" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (x .?> "NumResults")
                     <*> (pure (fromEnum s)))

instance Hashable ListWorkerBlocks where

instance NFData ListWorkerBlocks where

instance ToHeaders ListWorkerBlocks where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MTurkRequesterServiceV20170117.ListWorkerBlocks" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListWorkerBlocks where
        toJSON ListWorkerBlocks'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lwbNextToken,
                  ("MaxResults" .=) <$> _lwbMaxResults])

instance ToPath ListWorkerBlocks where
        toPath = const "/"

instance ToQuery ListWorkerBlocks where
        toQuery = const mempty

-- | /See:/ 'listWorkerBlocksResponse' smart constructor.
data ListWorkerBlocksResponse = ListWorkerBlocksResponse'
  { _lwbrsWorkerBlocks   :: !(Maybe [WorkerBlock])
  , _lwbrsNextToken      :: !(Maybe Text)
  , _lwbrsNumResults     :: !(Maybe Int)
  , _lwbrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListWorkerBlocksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lwbrsWorkerBlocks' - The list of WorkerBlocks, containing the collection of Worker IDs and reasons for blocking.
--
-- * 'lwbrsNextToken' - Undocumented member.
--
-- * 'lwbrsNumResults' - The number of assignments on the page in the filtered results list, equivalent to the number of assignments returned by this call.
--
-- * 'lwbrsResponseStatus' - -- | The response status code.
listWorkerBlocksResponse
    :: Int -- ^ 'lwbrsResponseStatus'
    -> ListWorkerBlocksResponse
listWorkerBlocksResponse pResponseStatus_ =
  ListWorkerBlocksResponse'
    { _lwbrsWorkerBlocks = Nothing
    , _lwbrsNextToken = Nothing
    , _lwbrsNumResults = Nothing
    , _lwbrsResponseStatus = pResponseStatus_
    }


-- | The list of WorkerBlocks, containing the collection of Worker IDs and reasons for blocking.
lwbrsWorkerBlocks :: Lens' ListWorkerBlocksResponse [WorkerBlock]
lwbrsWorkerBlocks = lens _lwbrsWorkerBlocks (\ s a -> s{_lwbrsWorkerBlocks = a}) . _Default . _Coerce

-- | Undocumented member.
lwbrsNextToken :: Lens' ListWorkerBlocksResponse (Maybe Text)
lwbrsNextToken = lens _lwbrsNextToken (\ s a -> s{_lwbrsNextToken = a})

-- | The number of assignments on the page in the filtered results list, equivalent to the number of assignments returned by this call.
lwbrsNumResults :: Lens' ListWorkerBlocksResponse (Maybe Int)
lwbrsNumResults = lens _lwbrsNumResults (\ s a -> s{_lwbrsNumResults = a})

-- | -- | The response status code.
lwbrsResponseStatus :: Lens' ListWorkerBlocksResponse Int
lwbrsResponseStatus = lens _lwbrsResponseStatus (\ s a -> s{_lwbrsResponseStatus = a})

instance NFData ListWorkerBlocksResponse where
