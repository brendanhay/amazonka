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
-- Module      : Network.AWS.Pinpoint.CreateSmsTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a message template for messages that are sent through the SMS channel.
module Network.AWS.Pinpoint.CreateSmsTemplate
  ( -- * Creating a Request
    createSmsTemplate,
    CreateSmsTemplate,

    -- * Request Lenses
    cstTemplateName,
    cstSMSTemplateRequest,

    -- * Destructuring the Response
    createSmsTemplateResponse,
    CreateSmsTemplateResponse,

    -- * Response Lenses
    cstrsResponseStatus,
    cstrsCreateTemplateMessageBody,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createSmsTemplate' smart constructor.
data CreateSmsTemplate = CreateSmsTemplate'
  { _cstTemplateName ::
      !Text,
    _cstSMSTemplateRequest :: !SMSTemplateRequest
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateSmsTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cstTemplateName' - The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- * 'cstSMSTemplateRequest' - Undocumented member.
createSmsTemplate ::
  -- | 'cstTemplateName'
  Text ->
  -- | 'cstSMSTemplateRequest'
  SMSTemplateRequest ->
  CreateSmsTemplate
createSmsTemplate pTemplateName_ pSMSTemplateRequest_ =
  CreateSmsTemplate'
    { _cstTemplateName = pTemplateName_,
      _cstSMSTemplateRequest = pSMSTemplateRequest_
    }

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
cstTemplateName :: Lens' CreateSmsTemplate Text
cstTemplateName = lens _cstTemplateName (\s a -> s {_cstTemplateName = a})

-- | Undocumented member.
cstSMSTemplateRequest :: Lens' CreateSmsTemplate SMSTemplateRequest
cstSMSTemplateRequest = lens _cstSMSTemplateRequest (\s a -> s {_cstSMSTemplateRequest = a})

instance AWSRequest CreateSmsTemplate where
  type Rs CreateSmsTemplate = CreateSmsTemplateResponse
  request = postJSON pinpoint
  response =
    receiveJSON
      ( \s h x ->
          CreateSmsTemplateResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable CreateSmsTemplate

instance NFData CreateSmsTemplate

instance ToHeaders CreateSmsTemplate where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON CreateSmsTemplate where
  toJSON CreateSmsTemplate' {..} =
    object
      (catMaybes [Just ("SMSTemplateRequest" .= _cstSMSTemplateRequest)])

instance ToPath CreateSmsTemplate where
  toPath CreateSmsTemplate' {..} =
    mconcat ["/v1/templates/", toBS _cstTemplateName, "/sms"]

instance ToQuery CreateSmsTemplate where
  toQuery = const mempty

-- | /See:/ 'createSmsTemplateResponse' smart constructor.
data CreateSmsTemplateResponse = CreateSmsTemplateResponse'
  { _cstrsResponseStatus ::
      !Int,
    _cstrsCreateTemplateMessageBody ::
      !CreateTemplateMessageBody
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateSmsTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cstrsResponseStatus' - -- | The response status code.
--
-- * 'cstrsCreateTemplateMessageBody' - Undocumented member.
createSmsTemplateResponse ::
  -- | 'cstrsResponseStatus'
  Int ->
  -- | 'cstrsCreateTemplateMessageBody'
  CreateTemplateMessageBody ->
  CreateSmsTemplateResponse
createSmsTemplateResponse
  pResponseStatus_
  pCreateTemplateMessageBody_ =
    CreateSmsTemplateResponse'
      { _cstrsResponseStatus =
          pResponseStatus_,
        _cstrsCreateTemplateMessageBody = pCreateTemplateMessageBody_
      }

-- | -- | The response status code.
cstrsResponseStatus :: Lens' CreateSmsTemplateResponse Int
cstrsResponseStatus = lens _cstrsResponseStatus (\s a -> s {_cstrsResponseStatus = a})

-- | Undocumented member.
cstrsCreateTemplateMessageBody :: Lens' CreateSmsTemplateResponse CreateTemplateMessageBody
cstrsCreateTemplateMessageBody = lens _cstrsCreateTemplateMessageBody (\s a -> s {_cstrsCreateTemplateMessageBody = a})

instance NFData CreateSmsTemplateResponse
