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
-- Module      : Network.AWS.Inspector.ListAttachedAssessments
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the assessments attached to the rules package specified by the
-- rules package ARN.
--
-- /See:/ <http://docs.aws.amazon.com/inspector/latest/APIReference/API_ListAttachedAssessments.html AWS API Reference> for ListAttachedAssessments.
module Network.AWS.Inspector.ListAttachedAssessments
    (
    -- * Creating a Request
      listAttachedAssessments
    , ListAttachedAssessments
    -- * Request Lenses
    , laaNextToken
    , laaRulesPackageARN
    , laaFilter
    , laaMaxResults

    -- * Destructuring the Response
    , listAttachedAssessmentsResponse
    , ListAttachedAssessmentsResponse
    -- * Response Lenses
    , laasrsAssessmentARNList
    , laasrsNextToken
    , laasrsResponseStatus
    ) where

import           Network.AWS.Inspector.Types
import           Network.AWS.Inspector.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listAttachedAssessments' smart constructor.
data ListAttachedAssessments = ListAttachedAssessments'
    { _laaNextToken       :: !(Maybe Text)
    , _laaRulesPackageARN :: !(Maybe Text)
    , _laaFilter          :: !(Maybe AssessmentsFilter)
    , _laaMaxResults      :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListAttachedAssessments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laaNextToken'
--
-- * 'laaRulesPackageARN'
--
-- * 'laaFilter'
--
-- * 'laaMaxResults'
listAttachedAssessments
    :: ListAttachedAssessments
listAttachedAssessments =
    ListAttachedAssessments'
    { _laaNextToken = Nothing
    , _laaRulesPackageARN = Nothing
    , _laaFilter = Nothing
    , _laaMaxResults = Nothing
    }

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to \'null\' on your first call to the
-- __ListAttachedAssessments__ action. Subsequent calls to the action fill
-- __nextToken__ in the request with the value of __NextToken__ from
-- previous response to continue listing data.
laaNextToken :: Lens' ListAttachedAssessments (Maybe Text)
laaNextToken = lens _laaNextToken (\ s a -> s{_laaNextToken = a});

-- | The ARN specifying the rules package whose assessments you want to list.
laaRulesPackageARN :: Lens' ListAttachedAssessments (Maybe Text)
laaRulesPackageARN = lens _laaRulesPackageARN (\ s a -> s{_laaRulesPackageARN = a});

-- | You can use this parameter to specify a subset of data to be included in
-- the action\'s response.
--
-- For a record to match a filter, all specified filter attributes must
-- match. When multiple values are specified for a filter attribute, any of
-- the values can match.
laaFilter :: Lens' ListAttachedAssessments (Maybe AssessmentsFilter)
laaFilter = lens _laaFilter (\ s a -> s{_laaFilter = a});

-- | You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 10. The maximum value is 500.
laaMaxResults :: Lens' ListAttachedAssessments (Maybe Int)
laaMaxResults = lens _laaMaxResults (\ s a -> s{_laaMaxResults = a});

instance AWSRequest ListAttachedAssessments where
        type Rs ListAttachedAssessments =
             ListAttachedAssessmentsResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 ListAttachedAssessmentsResponse' <$>
                   (x .?> "assessmentArnList" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListAttachedAssessments where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.ListAttachedAssessments" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListAttachedAssessments where
        toJSON ListAttachedAssessments'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _laaNextToken,
                  ("rulesPackageArn" .=) <$> _laaRulesPackageARN,
                  ("filter" .=) <$> _laaFilter,
                  ("maxResults" .=) <$> _laaMaxResults])

instance ToPath ListAttachedAssessments where
        toPath = const "/"

instance ToQuery ListAttachedAssessments where
        toQuery = const mempty

-- | /See:/ 'listAttachedAssessmentsResponse' smart constructor.
data ListAttachedAssessmentsResponse = ListAttachedAssessmentsResponse'
    { _laasrsAssessmentARNList :: !(Maybe [Text])
    , _laasrsNextToken         :: !(Maybe Text)
    , _laasrsResponseStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListAttachedAssessmentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laasrsAssessmentARNList'
--
-- * 'laasrsNextToken'
--
-- * 'laasrsResponseStatus'
listAttachedAssessmentsResponse
    :: Int -- ^ 'laasrsResponseStatus'
    -> ListAttachedAssessmentsResponse
listAttachedAssessmentsResponse pResponseStatus_ =
    ListAttachedAssessmentsResponse'
    { _laasrsAssessmentARNList = Nothing
    , _laasrsNextToken = Nothing
    , _laasrsResponseStatus = pResponseStatus_
    }

-- | A list of ARNs specifying the assessments returned by the action.
laasrsAssessmentARNList :: Lens' ListAttachedAssessmentsResponse [Text]
laasrsAssessmentARNList = lens _laasrsAssessmentARNList (\ s a -> s{_laasrsAssessmentARNList = a}) . _Default . _Coerce;

-- | When a response is generated, if there is more data to be listed, this
-- parameter is present in the response and contains the value to use for
-- the __nextToken__ parameter in a subsequent pagination request. If there
-- is no more data to be listed, this parameter is set to \'null\'.
laasrsNextToken :: Lens' ListAttachedAssessmentsResponse (Maybe Text)
laasrsNextToken = lens _laasrsNextToken (\ s a -> s{_laasrsNextToken = a});

-- | The response status code.
laasrsResponseStatus :: Lens' ListAttachedAssessmentsResponse Int
laasrsResponseStatus = lens _laasrsResponseStatus (\ s a -> s{_laasrsResponseStatus = a});
