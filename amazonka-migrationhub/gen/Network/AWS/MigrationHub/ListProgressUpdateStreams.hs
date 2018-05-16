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
-- Module      : Network.AWS.MigrationHub.ListProgressUpdateStreams
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists progress update streams associated with the user account making this call.
--
--
module Network.AWS.MigrationHub.ListProgressUpdateStreams
    (
    -- * Creating a Request
      listProgressUpdateStreams
    , ListProgressUpdateStreams
    -- * Request Lenses
    , lpusNextToken
    , lpusMaxResults

    -- * Destructuring the Response
    , listProgressUpdateStreamsResponse
    , ListProgressUpdateStreamsResponse
    -- * Response Lenses
    , lpusrsProgressUpdateStreamSummaryList
    , lpusrsNextToken
    , lpusrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MigrationHub.Types
import Network.AWS.MigrationHub.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listProgressUpdateStreams' smart constructor.
data ListProgressUpdateStreams = ListProgressUpdateStreams'
  { _lpusNextToken  :: !(Maybe Text)
  , _lpusMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListProgressUpdateStreams' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpusNextToken' - If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
--
-- * 'lpusMaxResults' - Filter to limit the maximum number of results to list per page.
listProgressUpdateStreams
    :: ListProgressUpdateStreams
listProgressUpdateStreams =
  ListProgressUpdateStreams'
    {_lpusNextToken = Nothing, _lpusMaxResults = Nothing}


-- | If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
lpusNextToken :: Lens' ListProgressUpdateStreams (Maybe Text)
lpusNextToken = lens _lpusNextToken (\ s a -> s{_lpusNextToken = a})

-- | Filter to limit the maximum number of results to list per page.
lpusMaxResults :: Lens' ListProgressUpdateStreams (Maybe Natural)
lpusMaxResults = lens _lpusMaxResults (\ s a -> s{_lpusMaxResults = a}) . mapping _Nat

instance AWSRequest ListProgressUpdateStreams where
        type Rs ListProgressUpdateStreams =
             ListProgressUpdateStreamsResponse
        request = postJSON migrationHub
        response
          = receiveJSON
              (\ s h x ->
                 ListProgressUpdateStreamsResponse' <$>
                   (x .?> "ProgressUpdateStreamSummaryList" .!@ mempty)
                     <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListProgressUpdateStreams where

instance NFData ListProgressUpdateStreams where

instance ToHeaders ListProgressUpdateStreams where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSMigrationHub.ListProgressUpdateStreams" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListProgressUpdateStreams where
        toJSON ListProgressUpdateStreams'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lpusNextToken,
                  ("MaxResults" .=) <$> _lpusMaxResults])

instance ToPath ListProgressUpdateStreams where
        toPath = const "/"

instance ToQuery ListProgressUpdateStreams where
        toQuery = const mempty

-- | /See:/ 'listProgressUpdateStreamsResponse' smart constructor.
data ListProgressUpdateStreamsResponse = ListProgressUpdateStreamsResponse'
  { _lpusrsProgressUpdateStreamSummaryList :: !(Maybe [ProgressUpdateStreamSummary])
  , _lpusrsNextToken :: !(Maybe Text)
  , _lpusrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListProgressUpdateStreamsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpusrsProgressUpdateStreamSummaryList' - List of progress update streams up to the max number of results passed in the input.
--
-- * 'lpusrsNextToken' - If there are more streams created than the max result, return the next token to be passed to the next call as a bookmark of where to start from.
--
-- * 'lpusrsResponseStatus' - -- | The response status code.
listProgressUpdateStreamsResponse
    :: Int -- ^ 'lpusrsResponseStatus'
    -> ListProgressUpdateStreamsResponse
listProgressUpdateStreamsResponse pResponseStatus_ =
  ListProgressUpdateStreamsResponse'
    { _lpusrsProgressUpdateStreamSummaryList = Nothing
    , _lpusrsNextToken = Nothing
    , _lpusrsResponseStatus = pResponseStatus_
    }


-- | List of progress update streams up to the max number of results passed in the input.
lpusrsProgressUpdateStreamSummaryList :: Lens' ListProgressUpdateStreamsResponse [ProgressUpdateStreamSummary]
lpusrsProgressUpdateStreamSummaryList = lens _lpusrsProgressUpdateStreamSummaryList (\ s a -> s{_lpusrsProgressUpdateStreamSummaryList = a}) . _Default . _Coerce

-- | If there are more streams created than the max result, return the next token to be passed to the next call as a bookmark of where to start from.
lpusrsNextToken :: Lens' ListProgressUpdateStreamsResponse (Maybe Text)
lpusrsNextToken = lens _lpusrsNextToken (\ s a -> s{_lpusrsNextToken = a})

-- | -- | The response status code.
lpusrsResponseStatus :: Lens' ListProgressUpdateStreamsResponse Int
lpusrsResponseStatus = lens _lpusrsResponseStatus (\ s a -> s{_lpusrsResponseStatus = a})

instance NFData ListProgressUpdateStreamsResponse
         where
