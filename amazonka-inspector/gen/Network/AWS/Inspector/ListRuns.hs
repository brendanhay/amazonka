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
-- Module      : Network.AWS.Inspector.ListRuns
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the assessment runs associated with the assessments specified by
-- the assessment ARNs.
module Network.AWS.Inspector.ListRuns
    (
    -- * Creating a Request
      listRuns
    , ListRuns
    -- * Request Lenses
    , lrAssessmentARNs
    , lrNextToken
    , lrFilter
    , lrMaxResults

    -- * Destructuring the Response
    , listRunsResponse
    , ListRunsResponse
    -- * Response Lenses
    , lrrsRunARNList
    , lrrsNextToken
    , lrrsResponseStatus
    ) where

import           Network.AWS.Inspector.Types
import           Network.AWS.Inspector.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listRuns' smart constructor.
data ListRuns = ListRuns'
    { _lrAssessmentARNs :: !(Maybe [Text])
    , _lrNextToken      :: !(Maybe Text)
    , _lrFilter         :: !(Maybe RunsFilter)
    , _lrMaxResults     :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListRuns' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrAssessmentARNs'
--
-- * 'lrNextToken'
--
-- * 'lrFilter'
--
-- * 'lrMaxResults'
listRuns
    :: ListRuns
listRuns =
    ListRuns'
    { _lrAssessmentARNs = Nothing
    , _lrNextToken = Nothing
    , _lrFilter = Nothing
    , _lrMaxResults = Nothing
    }

-- | The ARNs specifying the assessments whose runs you want to list.
lrAssessmentARNs :: Lens' ListRuns [Text]
lrAssessmentARNs = lens _lrAssessmentARNs (\ s a -> s{_lrAssessmentARNs = a}) . _Default . _Coerce;

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to \'null\' on your first call to the __ListRuns__
-- action. Subsequent calls to the action fill __nextToken__ in the request
-- with the value of __NextToken__ from previous response to continue
-- listing data.
lrNextToken :: Lens' ListRuns (Maybe Text)
lrNextToken = lens _lrNextToken (\ s a -> s{_lrNextToken = a});

-- | You can use this parameter to specify a subset of data to be included in
-- the action\'s response.
--
-- For a record to match a filter, all specified filter attributes must
-- match. When multiple values are specified for a filter attribute, any of
-- the values can match.
lrFilter :: Lens' ListRuns (Maybe RunsFilter)
lrFilter = lens _lrFilter (\ s a -> s{_lrFilter = a});

-- | You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 10. The maximum value is 500.
lrMaxResults :: Lens' ListRuns (Maybe Int)
lrMaxResults = lens _lrMaxResults (\ s a -> s{_lrMaxResults = a});

instance AWSRequest ListRuns where
        type Rs ListRuns = ListRunsResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 ListRunsResponse' <$>
                   (x .?> "runArnList" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListRuns

instance ToHeaders ListRuns where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.ListRuns" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListRuns where
        toJSON ListRuns'{..}
          = object
              (catMaybes
                 [("assessmentArns" .=) <$> _lrAssessmentARNs,
                  ("nextToken" .=) <$> _lrNextToken,
                  ("filter" .=) <$> _lrFilter,
                  ("maxResults" .=) <$> _lrMaxResults])

instance ToPath ListRuns where
        toPath = const "/"

instance ToQuery ListRuns where
        toQuery = const mempty

-- | /See:/ 'listRunsResponse' smart constructor.
data ListRunsResponse = ListRunsResponse'
    { _lrrsRunARNList     :: !(Maybe [Text])
    , _lrrsNextToken      :: !(Maybe Text)
    , _lrrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListRunsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrrsRunARNList'
--
-- * 'lrrsNextToken'
--
-- * 'lrrsResponseStatus'
listRunsResponse
    :: Int -- ^ 'lrrsResponseStatus'
    -> ListRunsResponse
listRunsResponse pResponseStatus_ =
    ListRunsResponse'
    { _lrrsRunARNList = Nothing
    , _lrrsNextToken = Nothing
    , _lrrsResponseStatus = pResponseStatus_
    }

-- | A list of ARNs specifying the assessment runs returned by the action.
lrrsRunARNList :: Lens' ListRunsResponse [Text]
lrrsRunARNList = lens _lrrsRunARNList (\ s a -> s{_lrrsRunARNList = a}) . _Default . _Coerce;

-- | When a response is generated, if there is more data to be listed, this
-- parameter is present in the response and contains the value to use for
-- the __nextToken__ parameter in a subsequent pagination request. If there
-- is no more data to be listed, this parameter is set to \'null\'.
lrrsNextToken :: Lens' ListRunsResponse (Maybe Text)
lrrsNextToken = lens _lrrsNextToken (\ s a -> s{_lrrsNextToken = a});

-- | The response status code.
lrrsResponseStatus :: Lens' ListRunsResponse Int
lrrsResponseStatus = lens _lrrsResponseStatus (\ s a -> s{_lrrsResponseStatus = a});
