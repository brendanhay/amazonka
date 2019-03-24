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
-- Module      : Network.AWS.Inspector.GetExclusionsPreview
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the exclusions preview (a list of ExclusionPreview objects) specified by the preview token. You can obtain the preview token by running the CreateExclusionsPreview API.
--
--
module Network.AWS.Inspector.GetExclusionsPreview
    (
    -- * Creating a Request
      getExclusionsPreview
    , GetExclusionsPreview
    -- * Request Lenses
    , gepLocale
    , gepNextToken
    , gepMaxResults
    , gepAssessmentTemplateARN
    , gepPreviewToken

    -- * Destructuring the Response
    , getExclusionsPreviewResponse
    , GetExclusionsPreviewResponse
    -- * Response Lenses
    , geprsExclusionPreviews
    , geprsNextToken
    , geprsResponseStatus
    , geprsPreviewStatus
    ) where

import Network.AWS.Inspector.Types
import Network.AWS.Inspector.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getExclusionsPreview' smart constructor.
data GetExclusionsPreview = GetExclusionsPreview'
  { _gepLocale                :: !(Maybe Locale)
  , _gepNextToken             :: !(Maybe Text)
  , _gepMaxResults            :: !(Maybe Int)
  , _gepAssessmentTemplateARN :: !Text
  , _gepPreviewToken          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetExclusionsPreview' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gepLocale' - The locale into which you want to translate the exclusion's title, description, and recommendation.
--
-- * 'gepNextToken' - You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the GetExclusionsPreviewRequest action. Subsequent calls to the action fill nextToken in the request with the value of nextToken from the previous response to continue listing data.
--
-- * 'gepMaxResults' - You can use this parameter to indicate the maximum number of items you want in the response. The default value is 100. The maximum value is 500.
--
-- * 'gepAssessmentTemplateARN' - The ARN that specifies the assessment template for which the exclusions preview was requested.
--
-- * 'gepPreviewToken' - The unique identifier associated of the exclusions preview.
getExclusionsPreview
    :: Text -- ^ 'gepAssessmentTemplateARN'
    -> Text -- ^ 'gepPreviewToken'
    -> GetExclusionsPreview
getExclusionsPreview pAssessmentTemplateARN_ pPreviewToken_ =
  GetExclusionsPreview'
    { _gepLocale = Nothing
    , _gepNextToken = Nothing
    , _gepMaxResults = Nothing
    , _gepAssessmentTemplateARN = pAssessmentTemplateARN_
    , _gepPreviewToken = pPreviewToken_
    }


-- | The locale into which you want to translate the exclusion's title, description, and recommendation.
gepLocale :: Lens' GetExclusionsPreview (Maybe Locale)
gepLocale = lens _gepLocale (\ s a -> s{_gepLocale = a})

-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the GetExclusionsPreviewRequest action. Subsequent calls to the action fill nextToken in the request with the value of nextToken from the previous response to continue listing data.
gepNextToken :: Lens' GetExclusionsPreview (Maybe Text)
gepNextToken = lens _gepNextToken (\ s a -> s{_gepNextToken = a})

-- | You can use this parameter to indicate the maximum number of items you want in the response. The default value is 100. The maximum value is 500.
gepMaxResults :: Lens' GetExclusionsPreview (Maybe Int)
gepMaxResults = lens _gepMaxResults (\ s a -> s{_gepMaxResults = a})

-- | The ARN that specifies the assessment template for which the exclusions preview was requested.
gepAssessmentTemplateARN :: Lens' GetExclusionsPreview Text
gepAssessmentTemplateARN = lens _gepAssessmentTemplateARN (\ s a -> s{_gepAssessmentTemplateARN = a})

-- | The unique identifier associated of the exclusions preview.
gepPreviewToken :: Lens' GetExclusionsPreview Text
gepPreviewToken = lens _gepPreviewToken (\ s a -> s{_gepPreviewToken = a})

instance AWSRequest GetExclusionsPreview where
        type Rs GetExclusionsPreview =
             GetExclusionsPreviewResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 GetExclusionsPreviewResponse' <$>
                   (x .?> "exclusionPreviews" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s))
                     <*> (x .:> "previewStatus"))

instance Hashable GetExclusionsPreview where

instance NFData GetExclusionsPreview where

instance ToHeaders GetExclusionsPreview where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.GetExclusionsPreview" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetExclusionsPreview where
        toJSON GetExclusionsPreview'{..}
          = object
              (catMaybes
                 [("locale" .=) <$> _gepLocale,
                  ("nextToken" .=) <$> _gepNextToken,
                  ("maxResults" .=) <$> _gepMaxResults,
                  Just
                    ("assessmentTemplateArn" .=
                       _gepAssessmentTemplateARN),
                  Just ("previewToken" .= _gepPreviewToken)])

instance ToPath GetExclusionsPreview where
        toPath = const "/"

instance ToQuery GetExclusionsPreview where
        toQuery = const mempty

-- | /See:/ 'getExclusionsPreviewResponse' smart constructor.
data GetExclusionsPreviewResponse = GetExclusionsPreviewResponse'
  { _geprsExclusionPreviews :: !(Maybe [ExclusionPreview])
  , _geprsNextToken         :: !(Maybe Text)
  , _geprsResponseStatus    :: !Int
  , _geprsPreviewStatus     :: !PreviewStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetExclusionsPreviewResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'geprsExclusionPreviews' - Information about the exclusions included in the preview.
--
-- * 'geprsNextToken' - When a response is generated, if there is more data to be listed, this parameters is present in the response and contains the value to use for the nextToken parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
--
-- * 'geprsResponseStatus' - -- | The response status code.
--
-- * 'geprsPreviewStatus' - Specifies the status of the request to generate an exclusions preview.
getExclusionsPreviewResponse
    :: Int -- ^ 'geprsResponseStatus'
    -> PreviewStatus -- ^ 'geprsPreviewStatus'
    -> GetExclusionsPreviewResponse
getExclusionsPreviewResponse pResponseStatus_ pPreviewStatus_ =
  GetExclusionsPreviewResponse'
    { _geprsExclusionPreviews = Nothing
    , _geprsNextToken = Nothing
    , _geprsResponseStatus = pResponseStatus_
    , _geprsPreviewStatus = pPreviewStatus_
    }


-- | Information about the exclusions included in the preview.
geprsExclusionPreviews :: Lens' GetExclusionsPreviewResponse [ExclusionPreview]
geprsExclusionPreviews = lens _geprsExclusionPreviews (\ s a -> s{_geprsExclusionPreviews = a}) . _Default . _Coerce

-- | When a response is generated, if there is more data to be listed, this parameters is present in the response and contains the value to use for the nextToken parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
geprsNextToken :: Lens' GetExclusionsPreviewResponse (Maybe Text)
geprsNextToken = lens _geprsNextToken (\ s a -> s{_geprsNextToken = a})

-- | -- | The response status code.
geprsResponseStatus :: Lens' GetExclusionsPreviewResponse Int
geprsResponseStatus = lens _geprsResponseStatus (\ s a -> s{_geprsResponseStatus = a})

-- | Specifies the status of the request to generate an exclusions preview.
geprsPreviewStatus :: Lens' GetExclusionsPreviewResponse PreviewStatus
geprsPreviewStatus = lens _geprsPreviewStatus (\ s a -> s{_geprsPreviewStatus = a})

instance NFData GetExclusionsPreviewResponse where
