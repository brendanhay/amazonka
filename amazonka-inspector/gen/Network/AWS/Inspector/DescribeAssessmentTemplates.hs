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
-- Module      : Network.AWS.Inspector.DescribeAssessmentTemplates
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the assessment templates that are specified by the ARNs of the assessment templates.
--
--
module Network.AWS.Inspector.DescribeAssessmentTemplates
    (
    -- * Creating a Request
      describeAssessmentTemplates
    , DescribeAssessmentTemplates
    -- * Request Lenses
    , datAssessmentTemplateARNs

    -- * Destructuring the Response
    , describeAssessmentTemplatesResponse
    , DescribeAssessmentTemplatesResponse
    -- * Response Lenses
    , datrsResponseStatus
    , datrsAssessmentTemplates
    , datrsFailedItems
    ) where

import Network.AWS.Inspector.Types
import Network.AWS.Inspector.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeAssessmentTemplates' smart constructor.
newtype DescribeAssessmentTemplates = DescribeAssessmentTemplates'
  { _datAssessmentTemplateARNs :: List1 Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAssessmentTemplates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'datAssessmentTemplateARNs' - Undocumented member.
describeAssessmentTemplates
    :: NonEmpty Text -- ^ 'datAssessmentTemplateARNs'
    -> DescribeAssessmentTemplates
describeAssessmentTemplates pAssessmentTemplateARNs_ =
  DescribeAssessmentTemplates'
    {_datAssessmentTemplateARNs = _List1 # pAssessmentTemplateARNs_}


-- | Undocumented member.
datAssessmentTemplateARNs :: Lens' DescribeAssessmentTemplates (NonEmpty Text)
datAssessmentTemplateARNs = lens _datAssessmentTemplateARNs (\ s a -> s{_datAssessmentTemplateARNs = a}) . _List1

instance AWSRequest DescribeAssessmentTemplates where
        type Rs DescribeAssessmentTemplates =
             DescribeAssessmentTemplatesResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 DescribeAssessmentTemplatesResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .?> "assessmentTemplates" .!@ mempty)
                     <*> (x .?> "failedItems" .!@ mempty))

instance Hashable DescribeAssessmentTemplates where

instance NFData DescribeAssessmentTemplates where

instance ToHeaders DescribeAssessmentTemplates where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.DescribeAssessmentTemplates" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeAssessmentTemplates where
        toJSON DescribeAssessmentTemplates'{..}
          = object
              (catMaybes
                 [Just
                    ("assessmentTemplateArns" .=
                       _datAssessmentTemplateARNs)])

instance ToPath DescribeAssessmentTemplates where
        toPath = const "/"

instance ToQuery DescribeAssessmentTemplates where
        toQuery = const mempty

-- | /See:/ 'describeAssessmentTemplatesResponse' smart constructor.
data DescribeAssessmentTemplatesResponse = DescribeAssessmentTemplatesResponse'
  { _datrsResponseStatus      :: !Int
  , _datrsAssessmentTemplates :: ![AssessmentTemplate]
  , _datrsFailedItems         :: !(Map Text FailedItemDetails)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAssessmentTemplatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'datrsResponseStatus' - -- | The response status code.
--
-- * 'datrsAssessmentTemplates' - Information about the assessment templates.
--
-- * 'datrsFailedItems' - Assessment template details that cannot be described. An error code is provided for each failed item.
describeAssessmentTemplatesResponse
    :: Int -- ^ 'datrsResponseStatus'
    -> DescribeAssessmentTemplatesResponse
describeAssessmentTemplatesResponse pResponseStatus_ =
  DescribeAssessmentTemplatesResponse'
    { _datrsResponseStatus = pResponseStatus_
    , _datrsAssessmentTemplates = mempty
    , _datrsFailedItems = mempty
    }


-- | -- | The response status code.
datrsResponseStatus :: Lens' DescribeAssessmentTemplatesResponse Int
datrsResponseStatus = lens _datrsResponseStatus (\ s a -> s{_datrsResponseStatus = a})

-- | Information about the assessment templates.
datrsAssessmentTemplates :: Lens' DescribeAssessmentTemplatesResponse [AssessmentTemplate]
datrsAssessmentTemplates = lens _datrsAssessmentTemplates (\ s a -> s{_datrsAssessmentTemplates = a}) . _Coerce

-- | Assessment template details that cannot be described. An error code is provided for each failed item.
datrsFailedItems :: Lens' DescribeAssessmentTemplatesResponse (HashMap Text FailedItemDetails)
datrsFailedItems = lens _datrsFailedItems (\ s a -> s{_datrsFailedItems = a}) . _Map

instance NFData DescribeAssessmentTemplatesResponse
         where
