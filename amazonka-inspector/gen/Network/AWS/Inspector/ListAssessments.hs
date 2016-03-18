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
-- Module      : Network.AWS.Inspector.ListAssessments
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the assessments corresponding to applications specified by the
-- applications\' ARNs.
module Network.AWS.Inspector.ListAssessments
    (
    -- * Creating a Request
      listAssessments
    , ListAssessments
    -- * Request Lenses
    , laApplicationARNs
    , laNextToken
    , laFilter
    , laMaxResults

    -- * Destructuring the Response
    , listAssessmentsResponse
    , ListAssessmentsResponse
    -- * Response Lenses
    , larsAssessmentARNList
    , larsNextToken
    , larsResponseStatus
    ) where

import           Network.AWS.Inspector.Types
import           Network.AWS.Inspector.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listAssessments' smart constructor.
data ListAssessments = ListAssessments'
    { _laApplicationARNs :: !(Maybe [Text])
    , _laNextToken       :: !(Maybe Text)
    , _laFilter          :: !(Maybe AssessmentsFilter)
    , _laMaxResults      :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListAssessments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laApplicationARNs'
--
-- * 'laNextToken'
--
-- * 'laFilter'
--
-- * 'laMaxResults'
listAssessments
    :: ListAssessments
listAssessments =
    ListAssessments'
    { _laApplicationARNs = Nothing
    , _laNextToken = Nothing
    , _laFilter = Nothing
    , _laMaxResults = Nothing
    }

-- | A list of ARNs specifying the applications the assessments of which you
-- want to list.
laApplicationARNs :: Lens' ListAssessments [Text]
laApplicationARNs = lens _laApplicationARNs (\ s a -> s{_laApplicationARNs = a}) . _Default . _Coerce;

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to \'null\' on your first call to the __ListAssessments__
-- action. Subsequent calls to the action fill __nextToken__ in the request
-- with the value of __NextToken__ from previous response to continue
-- listing data.
laNextToken :: Lens' ListAssessments (Maybe Text)
laNextToken = lens _laNextToken (\ s a -> s{_laNextToken = a});

-- | You can use this parameter to specify a subset of data to be included in
-- the action\'s response.
--
-- For a record to match a filter, all specified filter attributes must
-- match. When multiple values are specified for a filter attribute, any of
-- the values can match.
laFilter :: Lens' ListAssessments (Maybe AssessmentsFilter)
laFilter = lens _laFilter (\ s a -> s{_laFilter = a});

-- | You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 10. The maximum value is 500.
laMaxResults :: Lens' ListAssessments (Maybe Int)
laMaxResults = lens _laMaxResults (\ s a -> s{_laMaxResults = a});

instance AWSRequest ListAssessments where
        type Rs ListAssessments = ListAssessmentsResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 ListAssessmentsResponse' <$>
                   (x .?> "assessmentArnList" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListAssessments where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.ListAssessments" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListAssessments where
        toJSON ListAssessments'{..}
          = object
              (catMaybes
                 [("applicationArns" .=) <$> _laApplicationARNs,
                  ("nextToken" .=) <$> _laNextToken,
                  ("filter" .=) <$> _laFilter,
                  ("maxResults" .=) <$> _laMaxResults])

instance ToPath ListAssessments where
        toPath = const "/"

instance ToQuery ListAssessments where
        toQuery = const mempty

-- | /See:/ 'listAssessmentsResponse' smart constructor.
data ListAssessmentsResponse = ListAssessmentsResponse'
    { _larsAssessmentARNList :: !(Maybe [Text])
    , _larsNextToken         :: !(Maybe Text)
    , _larsResponseStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListAssessmentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'larsAssessmentARNList'
--
-- * 'larsNextToken'
--
-- * 'larsResponseStatus'
listAssessmentsResponse
    :: Int -- ^ 'larsResponseStatus'
    -> ListAssessmentsResponse
listAssessmentsResponse pResponseStatus_ =
    ListAssessmentsResponse'
    { _larsAssessmentARNList = Nothing
    , _larsNextToken = Nothing
    , _larsResponseStatus = pResponseStatus_
    }

-- | A list of ARNs specifying the assessments returned by the action.
larsAssessmentARNList :: Lens' ListAssessmentsResponse [Text]
larsAssessmentARNList = lens _larsAssessmentARNList (\ s a -> s{_larsAssessmentARNList = a}) . _Default . _Coerce;

-- | When a response is generated, if there is more data to be listed, this
-- parameter is present in the response and contains the value to use for
-- the __nextToken__ parameter in a subsequent pagination request. If there
-- is no more data to be listed, this parameter is set to \'null\'.
larsNextToken :: Lens' ListAssessmentsResponse (Maybe Text)
larsNextToken = lens _larsNextToken (\ s a -> s{_larsNextToken = a});

-- | The response status code.
larsResponseStatus :: Lens' ListAssessmentsResponse Int
larsResponseStatus = lens _larsResponseStatus (\ s a -> s{_larsResponseStatus = a});
