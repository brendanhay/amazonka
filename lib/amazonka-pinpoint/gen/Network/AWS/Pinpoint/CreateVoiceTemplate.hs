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
-- Module      : Network.AWS.Pinpoint.CreateVoiceTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a message template for messages that are sent through the voice channel.
module Network.AWS.Pinpoint.CreateVoiceTemplate
  ( -- * Creating a Request
    createVoiceTemplate,
    CreateVoiceTemplate,

    -- * Request Lenses
    cvtTemplateName,
    cvtVoiceTemplateRequest,

    -- * Destructuring the Response
    createVoiceTemplateResponse,
    CreateVoiceTemplateResponse,

    -- * Response Lenses
    cvtrsResponseStatus,
    cvtrsCreateTemplateMessageBody,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createVoiceTemplate' smart constructor.
data CreateVoiceTemplate = CreateVoiceTemplate'
  { _cvtTemplateName ::
      !Text,
    _cvtVoiceTemplateRequest :: !VoiceTemplateRequest
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateVoiceTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvtTemplateName' - The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- * 'cvtVoiceTemplateRequest' - Undocumented member.
createVoiceTemplate ::
  -- | 'cvtTemplateName'
  Text ->
  -- | 'cvtVoiceTemplateRequest'
  VoiceTemplateRequest ->
  CreateVoiceTemplate
createVoiceTemplate pTemplateName_ pVoiceTemplateRequest_ =
  CreateVoiceTemplate'
    { _cvtTemplateName = pTemplateName_,
      _cvtVoiceTemplateRequest = pVoiceTemplateRequest_
    }

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
cvtTemplateName :: Lens' CreateVoiceTemplate Text
cvtTemplateName = lens _cvtTemplateName (\s a -> s {_cvtTemplateName = a})

-- | Undocumented member.
cvtVoiceTemplateRequest :: Lens' CreateVoiceTemplate VoiceTemplateRequest
cvtVoiceTemplateRequest = lens _cvtVoiceTemplateRequest (\s a -> s {_cvtVoiceTemplateRequest = a})

instance AWSRequest CreateVoiceTemplate where
  type Rs CreateVoiceTemplate = CreateVoiceTemplateResponse
  request = postJSON pinpoint
  response =
    receiveJSON
      ( \s h x ->
          CreateVoiceTemplateResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable CreateVoiceTemplate

instance NFData CreateVoiceTemplate

instance ToHeaders CreateVoiceTemplate where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON CreateVoiceTemplate where
  toJSON CreateVoiceTemplate' {..} =
    object
      ( catMaybes
          [Just ("VoiceTemplateRequest" .= _cvtVoiceTemplateRequest)]
      )

instance ToPath CreateVoiceTemplate where
  toPath CreateVoiceTemplate' {..} =
    mconcat ["/v1/templates/", toBS _cvtTemplateName, "/voice"]

instance ToQuery CreateVoiceTemplate where
  toQuery = const mempty

-- | /See:/ 'createVoiceTemplateResponse' smart constructor.
data CreateVoiceTemplateResponse = CreateVoiceTemplateResponse'
  { _cvtrsResponseStatus ::
      !Int,
    _cvtrsCreateTemplateMessageBody ::
      !CreateTemplateMessageBody
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateVoiceTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvtrsResponseStatus' - -- | The response status code.
--
-- * 'cvtrsCreateTemplateMessageBody' - Undocumented member.
createVoiceTemplateResponse ::
  -- | 'cvtrsResponseStatus'
  Int ->
  -- | 'cvtrsCreateTemplateMessageBody'
  CreateTemplateMessageBody ->
  CreateVoiceTemplateResponse
createVoiceTemplateResponse
  pResponseStatus_
  pCreateTemplateMessageBody_ =
    CreateVoiceTemplateResponse'
      { _cvtrsResponseStatus =
          pResponseStatus_,
        _cvtrsCreateTemplateMessageBody = pCreateTemplateMessageBody_
      }

-- | -- | The response status code.
cvtrsResponseStatus :: Lens' CreateVoiceTemplateResponse Int
cvtrsResponseStatus = lens _cvtrsResponseStatus (\s a -> s {_cvtrsResponseStatus = a})

-- | Undocumented member.
cvtrsCreateTemplateMessageBody :: Lens' CreateVoiceTemplateResponse CreateTemplateMessageBody
cvtrsCreateTemplateMessageBody = lens _cvtrsCreateTemplateMessageBody (\s a -> s {_cvtrsCreateTemplateMessageBody = a})

instance NFData CreateVoiceTemplateResponse
