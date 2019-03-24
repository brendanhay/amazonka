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
-- Module      : Network.AWS.Polly.ListSpeechSynthesisTasks
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of SpeechSynthesisTask objects ordered by their creation date. This operation can filter the tasks by their status, for example, allowing users to list only tasks that are completed.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Polly.ListSpeechSynthesisTasks
    (
    -- * Creating a Request
      listSpeechSynthesisTasks
    , ListSpeechSynthesisTasks
    -- * Request Lenses
    , lsstStatus
    , lsstNextToken
    , lsstMaxResults

    -- * Destructuring the Response
    , listSpeechSynthesisTasksResponse
    , ListSpeechSynthesisTasksResponse
    -- * Response Lenses
    , lsstrsNextToken
    , lsstrsSynthesisTasks
    , lsstrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Polly.Types
import Network.AWS.Polly.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listSpeechSynthesisTasks' smart constructor.
data ListSpeechSynthesisTasks = ListSpeechSynthesisTasks'
  { _lsstStatus     :: !(Maybe TaskStatus)
  , _lsstNextToken  :: !(Maybe Text)
  , _lsstMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSpeechSynthesisTasks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsstStatus' - Status of the speech synthesis tasks returned in a List operation
--
-- * 'lsstNextToken' - The pagination token to use in the next request to continue the listing of speech synthesis tasks.
--
-- * 'lsstMaxResults' - Maximum number of speech synthesis tasks returned in a List operation.
listSpeechSynthesisTasks
    :: ListSpeechSynthesisTasks
listSpeechSynthesisTasks =
  ListSpeechSynthesisTasks'
    {_lsstStatus = Nothing, _lsstNextToken = Nothing, _lsstMaxResults = Nothing}


-- | Status of the speech synthesis tasks returned in a List operation
lsstStatus :: Lens' ListSpeechSynthesisTasks (Maybe TaskStatus)
lsstStatus = lens _lsstStatus (\ s a -> s{_lsstStatus = a})

-- | The pagination token to use in the next request to continue the listing of speech synthesis tasks.
lsstNextToken :: Lens' ListSpeechSynthesisTasks (Maybe Text)
lsstNextToken = lens _lsstNextToken (\ s a -> s{_lsstNextToken = a})

-- | Maximum number of speech synthesis tasks returned in a List operation.
lsstMaxResults :: Lens' ListSpeechSynthesisTasks (Maybe Natural)
lsstMaxResults = lens _lsstMaxResults (\ s a -> s{_lsstMaxResults = a}) . mapping _Nat

instance AWSPager ListSpeechSynthesisTasks where
        page rq rs
          | stop (rs ^. lsstrsNextToken) = Nothing
          | stop (rs ^. lsstrsSynthesisTasks) = Nothing
          | otherwise =
            Just $ rq & lsstNextToken .~ rs ^. lsstrsNextToken

instance AWSRequest ListSpeechSynthesisTasks where
        type Rs ListSpeechSynthesisTasks =
             ListSpeechSynthesisTasksResponse
        request = get polly
        response
          = receiveJSON
              (\ s h x ->
                 ListSpeechSynthesisTasksResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "SynthesisTasks" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListSpeechSynthesisTasks where

instance NFData ListSpeechSynthesisTasks where

instance ToHeaders ListSpeechSynthesisTasks where
        toHeaders = const mempty

instance ToPath ListSpeechSynthesisTasks where
        toPath = const "/v1/synthesisTasks"

instance ToQuery ListSpeechSynthesisTasks where
        toQuery ListSpeechSynthesisTasks'{..}
          = mconcat
              ["Status" =: _lsstStatus,
               "NextToken" =: _lsstNextToken,
               "MaxResults" =: _lsstMaxResults]

-- | /See:/ 'listSpeechSynthesisTasksResponse' smart constructor.
data ListSpeechSynthesisTasksResponse = ListSpeechSynthesisTasksResponse'
  { _lsstrsNextToken      :: !(Maybe Text)
  , _lsstrsSynthesisTasks :: !(Maybe [SynthesisTask])
  , _lsstrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSpeechSynthesisTasksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsstrsNextToken' - An opaque pagination token returned from the previous List operation in this request. If present, this indicates where to continue the listing.
--
-- * 'lsstrsSynthesisTasks' - List of SynthesisTask objects that provides information from the specified task in the list request, including output format, creation time, task status, and so on.
--
-- * 'lsstrsResponseStatus' - -- | The response status code.
listSpeechSynthesisTasksResponse
    :: Int -- ^ 'lsstrsResponseStatus'
    -> ListSpeechSynthesisTasksResponse
listSpeechSynthesisTasksResponse pResponseStatus_ =
  ListSpeechSynthesisTasksResponse'
    { _lsstrsNextToken = Nothing
    , _lsstrsSynthesisTasks = Nothing
    , _lsstrsResponseStatus = pResponseStatus_
    }


-- | An opaque pagination token returned from the previous List operation in this request. If present, this indicates where to continue the listing.
lsstrsNextToken :: Lens' ListSpeechSynthesisTasksResponse (Maybe Text)
lsstrsNextToken = lens _lsstrsNextToken (\ s a -> s{_lsstrsNextToken = a})

-- | List of SynthesisTask objects that provides information from the specified task in the list request, including output format, creation time, task status, and so on.
lsstrsSynthesisTasks :: Lens' ListSpeechSynthesisTasksResponse [SynthesisTask]
lsstrsSynthesisTasks = lens _lsstrsSynthesisTasks (\ s a -> s{_lsstrsSynthesisTasks = a}) . _Default . _Coerce

-- | -- | The response status code.
lsstrsResponseStatus :: Lens' ListSpeechSynthesisTasksResponse Int
lsstrsResponseStatus = lens _lsstrsResponseStatus (\ s a -> s{_lsstrsResponseStatus = a})

instance NFData ListSpeechSynthesisTasksResponse
         where
