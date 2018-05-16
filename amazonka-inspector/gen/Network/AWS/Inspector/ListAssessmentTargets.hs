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
-- Module      : Network.AWS.Inspector.ListAssessmentTargets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the ARNs of the assessment targets within this AWS account. For more information about assessment targets, see <http://docs.aws.amazon.com/inspector/latest/userguide/inspector_applications.html Amazon Inspector Assessment Targets> .
--
--
--
-- This operation returns paginated results.
module Network.AWS.Inspector.ListAssessmentTargets
    (
    -- * Creating a Request
      listAssessmentTargets
    , ListAssessmentTargets
    -- * Request Lenses
    , lNextToken
    , lFilter
    , lMaxResults

    -- * Destructuring the Response
    , listAssessmentTargetsResponse
    , ListAssessmentTargetsResponse
    -- * Response Lenses
    , lrsNextToken
    , lrsResponseStatus
    , lrsAssessmentTargetARNs
    ) where

import Network.AWS.Inspector.Types
import Network.AWS.Inspector.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listAssessmentTargets' smart constructor.
data ListAssessmentTargets = ListAssessmentTargets'
  { _lNextToken  :: !(Maybe Text)
  , _lFilter     :: !(Maybe AssessmentTargetFilter)
  , _lMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAssessmentTargets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lNextToken' - You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __ListAssessmentTargets__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
--
-- * 'lFilter' - You can use this parameter to specify a subset of data to be included in the action's response. For a record to match a filter, all specified filter attributes must match. When multiple values are specified for a filter attribute, any of the values can match.
--
-- * 'lMaxResults' - You can use this parameter to indicate the maximum number of items you want in the response. The default value is 10. The maximum value is 500.
listAssessmentTargets
    :: ListAssessmentTargets
listAssessmentTargets =
  ListAssessmentTargets'
    {_lNextToken = Nothing, _lFilter = Nothing, _lMaxResults = Nothing}


-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __ListAssessmentTargets__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
lNextToken :: Lens' ListAssessmentTargets (Maybe Text)
lNextToken = lens _lNextToken (\ s a -> s{_lNextToken = a})

-- | You can use this parameter to specify a subset of data to be included in the action's response. For a record to match a filter, all specified filter attributes must match. When multiple values are specified for a filter attribute, any of the values can match.
lFilter :: Lens' ListAssessmentTargets (Maybe AssessmentTargetFilter)
lFilter = lens _lFilter (\ s a -> s{_lFilter = a})

-- | You can use this parameter to indicate the maximum number of items you want in the response. The default value is 10. The maximum value is 500.
lMaxResults :: Lens' ListAssessmentTargets (Maybe Int)
lMaxResults = lens _lMaxResults (\ s a -> s{_lMaxResults = a})

instance AWSPager ListAssessmentTargets where
        page rq rs
          | stop (rs ^. lrsNextToken) = Nothing
          | stop (rs ^. lrsAssessmentTargetARNs) = Nothing
          | otherwise =
            Just $ rq & lNextToken .~ rs ^. lrsNextToken

instance AWSRequest ListAssessmentTargets where
        type Rs ListAssessmentTargets =
             ListAssessmentTargetsResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 ListAssessmentTargetsResponse' <$>
                   (x .?> "nextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "assessmentTargetArns" .!@ mempty))

instance Hashable ListAssessmentTargets where

instance NFData ListAssessmentTargets where

instance ToHeaders ListAssessmentTargets where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.ListAssessmentTargets" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListAssessmentTargets where
        toJSON ListAssessmentTargets'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _lNextToken,
                  ("filter" .=) <$> _lFilter,
                  ("maxResults" .=) <$> _lMaxResults])

instance ToPath ListAssessmentTargets where
        toPath = const "/"

instance ToQuery ListAssessmentTargets where
        toQuery = const mempty

-- | /See:/ 'listAssessmentTargetsResponse' smart constructor.
data ListAssessmentTargetsResponse = ListAssessmentTargetsResponse'
  { _lrsNextToken            :: !(Maybe Text)
  , _lrsResponseStatus       :: !Int
  , _lrsAssessmentTargetARNs :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAssessmentTargetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrsNextToken' - When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
--
-- * 'lrsResponseStatus' - -- | The response status code.
--
-- * 'lrsAssessmentTargetARNs' - A list of ARNs that specifies the assessment targets that are returned by the action.
listAssessmentTargetsResponse
    :: Int -- ^ 'lrsResponseStatus'
    -> ListAssessmentTargetsResponse
listAssessmentTargetsResponse pResponseStatus_ =
  ListAssessmentTargetsResponse'
    { _lrsNextToken = Nothing
    , _lrsResponseStatus = pResponseStatus_
    , _lrsAssessmentTargetARNs = mempty
    }


-- | When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
lrsNextToken :: Lens' ListAssessmentTargetsResponse (Maybe Text)
lrsNextToken = lens _lrsNextToken (\ s a -> s{_lrsNextToken = a})

-- | -- | The response status code.
lrsResponseStatus :: Lens' ListAssessmentTargetsResponse Int
lrsResponseStatus = lens _lrsResponseStatus (\ s a -> s{_lrsResponseStatus = a})

-- | A list of ARNs that specifies the assessment targets that are returned by the action.
lrsAssessmentTargetARNs :: Lens' ListAssessmentTargetsResponse [Text]
lrsAssessmentTargetARNs = lens _lrsAssessmentTargetARNs (\ s a -> s{_lrsAssessmentTargetARNs = a}) . _Coerce

instance NFData ListAssessmentTargetsResponse where
