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
-- Module      : Network.AWS.Inspector.ListAssessmentTemplates
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the assessment templates that correspond to the assessment targets that are specified by the ARNs of the assessment targets.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Inspector.ListAssessmentTemplates
    (
    -- * Creating a Request
      listAssessmentTemplates
    , ListAssessmentTemplates
    -- * Request Lenses
    , latNextToken
    , latFilter
    , latMaxResults
    , latAssessmentTargetARNs

    -- * Destructuring the Response
    , listAssessmentTemplatesResponse
    , ListAssessmentTemplatesResponse
    -- * Response Lenses
    , latrsNextToken
    , latrsResponseStatus
    , latrsAssessmentTemplateARNs
    ) where

import Network.AWS.Inspector.Types
import Network.AWS.Inspector.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listAssessmentTemplates' smart constructor.
data ListAssessmentTemplates = ListAssessmentTemplates'
  { _latNextToken            :: !(Maybe Text)
  , _latFilter               :: !(Maybe AssessmentTemplateFilter)
  , _latMaxResults           :: !(Maybe Int)
  , _latAssessmentTargetARNs :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAssessmentTemplates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'latNextToken' - You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __ListAssessmentTemplates__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
--
-- * 'latFilter' - You can use this parameter to specify a subset of data to be included in the action's response. For a record to match a filter, all specified filter attributes must match. When multiple values are specified for a filter attribute, any of the values can match.
--
-- * 'latMaxResults' - You can use this parameter to indicate the maximum number of items you want in the response. The default value is 10. The maximum value is 500.
--
-- * 'latAssessmentTargetARNs' - A list of ARNs that specifies the assessment targets whose assessment templates you want to list.
listAssessmentTemplates
    :: ListAssessmentTemplates
listAssessmentTemplates =
  ListAssessmentTemplates'
    { _latNextToken = Nothing
    , _latFilter = Nothing
    , _latMaxResults = Nothing
    , _latAssessmentTargetARNs = Nothing
    }


-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __ListAssessmentTemplates__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
latNextToken :: Lens' ListAssessmentTemplates (Maybe Text)
latNextToken = lens _latNextToken (\ s a -> s{_latNextToken = a})

-- | You can use this parameter to specify a subset of data to be included in the action's response. For a record to match a filter, all specified filter attributes must match. When multiple values are specified for a filter attribute, any of the values can match.
latFilter :: Lens' ListAssessmentTemplates (Maybe AssessmentTemplateFilter)
latFilter = lens _latFilter (\ s a -> s{_latFilter = a})

-- | You can use this parameter to indicate the maximum number of items you want in the response. The default value is 10. The maximum value is 500.
latMaxResults :: Lens' ListAssessmentTemplates (Maybe Int)
latMaxResults = lens _latMaxResults (\ s a -> s{_latMaxResults = a})

-- | A list of ARNs that specifies the assessment targets whose assessment templates you want to list.
latAssessmentTargetARNs :: Lens' ListAssessmentTemplates [Text]
latAssessmentTargetARNs = lens _latAssessmentTargetARNs (\ s a -> s{_latAssessmentTargetARNs = a}) . _Default . _Coerce

instance AWSPager ListAssessmentTemplates where
        page rq rs
          | stop (rs ^. latrsNextToken) = Nothing
          | stop (rs ^. latrsAssessmentTemplateARNs) = Nothing
          | otherwise =
            Just $ rq & latNextToken .~ rs ^. latrsNextToken

instance AWSRequest ListAssessmentTemplates where
        type Rs ListAssessmentTemplates =
             ListAssessmentTemplatesResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 ListAssessmentTemplatesResponse' <$>
                   (x .?> "nextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "assessmentTemplateArns" .!@ mempty))

instance Hashable ListAssessmentTemplates where

instance NFData ListAssessmentTemplates where

instance ToHeaders ListAssessmentTemplates where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.ListAssessmentTemplates" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListAssessmentTemplates where
        toJSON ListAssessmentTemplates'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _latNextToken,
                  ("filter" .=) <$> _latFilter,
                  ("maxResults" .=) <$> _latMaxResults,
                  ("assessmentTargetArns" .=) <$>
                    _latAssessmentTargetARNs])

instance ToPath ListAssessmentTemplates where
        toPath = const "/"

instance ToQuery ListAssessmentTemplates where
        toQuery = const mempty

-- | /See:/ 'listAssessmentTemplatesResponse' smart constructor.
data ListAssessmentTemplatesResponse = ListAssessmentTemplatesResponse'
  { _latrsNextToken              :: !(Maybe Text)
  , _latrsResponseStatus         :: !Int
  , _latrsAssessmentTemplateARNs :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAssessmentTemplatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'latrsNextToken' - When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
--
-- * 'latrsResponseStatus' - -- | The response status code.
--
-- * 'latrsAssessmentTemplateARNs' - A list of ARNs that specifies the assessment templates returned by the action.
listAssessmentTemplatesResponse
    :: Int -- ^ 'latrsResponseStatus'
    -> ListAssessmentTemplatesResponse
listAssessmentTemplatesResponse pResponseStatus_ =
  ListAssessmentTemplatesResponse'
    { _latrsNextToken = Nothing
    , _latrsResponseStatus = pResponseStatus_
    , _latrsAssessmentTemplateARNs = mempty
    }


-- | When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
latrsNextToken :: Lens' ListAssessmentTemplatesResponse (Maybe Text)
latrsNextToken = lens _latrsNextToken (\ s a -> s{_latrsNextToken = a})

-- | -- | The response status code.
latrsResponseStatus :: Lens' ListAssessmentTemplatesResponse Int
latrsResponseStatus = lens _latrsResponseStatus (\ s a -> s{_latrsResponseStatus = a})

-- | A list of ARNs that specifies the assessment templates returned by the action.
latrsAssessmentTemplateARNs :: Lens' ListAssessmentTemplatesResponse [Text]
latrsAssessmentTemplateARNs = lens _latrsAssessmentTemplateARNs (\ s a -> s{_latrsAssessmentTemplateARNs = a}) . _Coerce

instance NFData ListAssessmentTemplatesResponse where
