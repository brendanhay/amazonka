{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.CreateExclusionsPreview
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the generation of an exclusions preview for the specified assessment template. The exclusions preview lists the potential exclusions (ExclusionPreview) that Inspector can detect before it runs the assessment.
module Network.AWS.Inspector.CreateExclusionsPreview
  ( -- * Creating a Request
    createExclusionsPreview,
    CreateExclusionsPreview,

    -- * Request Lenses
    cepAssessmentTemplateARN,

    -- * Destructuring the Response
    createExclusionsPreviewResponse,
    CreateExclusionsPreviewResponse,

    -- * Response Lenses
    ceprsResponseStatus,
    ceprsPreviewToken,
  )
where

import Network.AWS.Inspector.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createExclusionsPreview' smart constructor.
newtype CreateExclusionsPreview = CreateExclusionsPreview'
  { _cepAssessmentTemplateARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateExclusionsPreview' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cepAssessmentTemplateARN' - The ARN that specifies the assessment template for which you want to create an exclusions preview.
createExclusionsPreview ::
  -- | 'cepAssessmentTemplateARN'
  Text ->
  CreateExclusionsPreview
createExclusionsPreview pAssessmentTemplateARN_ =
  CreateExclusionsPreview'
    { _cepAssessmentTemplateARN =
        pAssessmentTemplateARN_
    }

-- | The ARN that specifies the assessment template for which you want to create an exclusions preview.
cepAssessmentTemplateARN :: Lens' CreateExclusionsPreview Text
cepAssessmentTemplateARN = lens _cepAssessmentTemplateARN (\s a -> s {_cepAssessmentTemplateARN = a})

instance AWSRequest CreateExclusionsPreview where
  type Rs CreateExclusionsPreview = CreateExclusionsPreviewResponse
  request = postJSON inspector
  response =
    receiveJSON
      ( \s h x ->
          CreateExclusionsPreviewResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "previewToken")
      )

instance Hashable CreateExclusionsPreview

instance NFData CreateExclusionsPreview

instance ToHeaders CreateExclusionsPreview where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("InspectorService.CreateExclusionsPreview" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateExclusionsPreview where
  toJSON CreateExclusionsPreview' {..} =
    object
      ( catMaybes
          [Just ("assessmentTemplateArn" .= _cepAssessmentTemplateARN)]
      )

instance ToPath CreateExclusionsPreview where
  toPath = const "/"

instance ToQuery CreateExclusionsPreview where
  toQuery = const mempty

-- | /See:/ 'createExclusionsPreviewResponse' smart constructor.
data CreateExclusionsPreviewResponse = CreateExclusionsPreviewResponse'
  { _ceprsResponseStatus ::
      !Int,
    _ceprsPreviewToken :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateExclusionsPreviewResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ceprsResponseStatus' - -- | The response status code.
--
-- * 'ceprsPreviewToken' - Specifies the unique identifier of the requested exclusions preview. You can use the unique identifier to retrieve the exclusions preview when running the GetExclusionsPreview API.
createExclusionsPreviewResponse ::
  -- | 'ceprsResponseStatus'
  Int ->
  -- | 'ceprsPreviewToken'
  Text ->
  CreateExclusionsPreviewResponse
createExclusionsPreviewResponse pResponseStatus_ pPreviewToken_ =
  CreateExclusionsPreviewResponse'
    { _ceprsResponseStatus =
        pResponseStatus_,
      _ceprsPreviewToken = pPreviewToken_
    }

-- | -- | The response status code.
ceprsResponseStatus :: Lens' CreateExclusionsPreviewResponse Int
ceprsResponseStatus = lens _ceprsResponseStatus (\s a -> s {_ceprsResponseStatus = a})

-- | Specifies the unique identifier of the requested exclusions preview. You can use the unique identifier to retrieve the exclusions preview when running the GetExclusionsPreview API.
ceprsPreviewToken :: Lens' CreateExclusionsPreviewResponse Text
ceprsPreviewToken = lens _ceprsPreviewToken (\s a -> s {_ceprsPreviewToken = a})

instance NFData CreateExclusionsPreviewResponse
