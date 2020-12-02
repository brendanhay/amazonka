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
-- Module      : Network.AWS.Pinpoint.CreateEmailTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a message template for messages that are sent through the email channel.
module Network.AWS.Pinpoint.CreateEmailTemplate
  ( -- * Creating a Request
    createEmailTemplate,
    CreateEmailTemplate,

    -- * Request Lenses
    cetTemplateName,
    cetEmailTemplateRequest,

    -- * Destructuring the Response
    createEmailTemplateResponse,
    CreateEmailTemplateResponse,

    -- * Response Lenses
    cetrsResponseStatus,
    cetrsCreateTemplateMessageBody,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createEmailTemplate' smart constructor.
data CreateEmailTemplate = CreateEmailTemplate'
  { _cetTemplateName ::
      !Text,
    _cetEmailTemplateRequest :: !EmailTemplateRequest
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateEmailTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cetTemplateName' - The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- * 'cetEmailTemplateRequest' - Undocumented member.
createEmailTemplate ::
  -- | 'cetTemplateName'
  Text ->
  -- | 'cetEmailTemplateRequest'
  EmailTemplateRequest ->
  CreateEmailTemplate
createEmailTemplate pTemplateName_ pEmailTemplateRequest_ =
  CreateEmailTemplate'
    { _cetTemplateName = pTemplateName_,
      _cetEmailTemplateRequest = pEmailTemplateRequest_
    }

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
cetTemplateName :: Lens' CreateEmailTemplate Text
cetTemplateName = lens _cetTemplateName (\s a -> s {_cetTemplateName = a})

-- | Undocumented member.
cetEmailTemplateRequest :: Lens' CreateEmailTemplate EmailTemplateRequest
cetEmailTemplateRequest = lens _cetEmailTemplateRequest (\s a -> s {_cetEmailTemplateRequest = a})

instance AWSRequest CreateEmailTemplate where
  type Rs CreateEmailTemplate = CreateEmailTemplateResponse
  request = postJSON pinpoint
  response =
    receiveJSON
      ( \s h x ->
          CreateEmailTemplateResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable CreateEmailTemplate

instance NFData CreateEmailTemplate

instance ToHeaders CreateEmailTemplate where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON CreateEmailTemplate where
  toJSON CreateEmailTemplate' {..} =
    object
      ( catMaybes
          [Just ("EmailTemplateRequest" .= _cetEmailTemplateRequest)]
      )

instance ToPath CreateEmailTemplate where
  toPath CreateEmailTemplate' {..} =
    mconcat ["/v1/templates/", toBS _cetTemplateName, "/email"]

instance ToQuery CreateEmailTemplate where
  toQuery = const mempty

-- | /See:/ 'createEmailTemplateResponse' smart constructor.
data CreateEmailTemplateResponse = CreateEmailTemplateResponse'
  { _cetrsResponseStatus ::
      !Int,
    _cetrsCreateTemplateMessageBody ::
      !CreateTemplateMessageBody
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateEmailTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cetrsResponseStatus' - -- | The response status code.
--
-- * 'cetrsCreateTemplateMessageBody' - Undocumented member.
createEmailTemplateResponse ::
  -- | 'cetrsResponseStatus'
  Int ->
  -- | 'cetrsCreateTemplateMessageBody'
  CreateTemplateMessageBody ->
  CreateEmailTemplateResponse
createEmailTemplateResponse
  pResponseStatus_
  pCreateTemplateMessageBody_ =
    CreateEmailTemplateResponse'
      { _cetrsResponseStatus =
          pResponseStatus_,
        _cetrsCreateTemplateMessageBody = pCreateTemplateMessageBody_
      }

-- | -- | The response status code.
cetrsResponseStatus :: Lens' CreateEmailTemplateResponse Int
cetrsResponseStatus = lens _cetrsResponseStatus (\s a -> s {_cetrsResponseStatus = a})

-- | Undocumented member.
cetrsCreateTemplateMessageBody :: Lens' CreateEmailTemplateResponse CreateTemplateMessageBody
cetrsCreateTemplateMessageBody = lens _cetrsCreateTemplateMessageBody (\s a -> s {_cetrsCreateTemplateMessageBody = a})

instance NFData CreateEmailTemplateResponse
