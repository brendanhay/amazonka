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
-- Module      : Network.AWS.Pinpoint.CreatePushTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a message template for messages that are sent through a push notification channel.
module Network.AWS.Pinpoint.CreatePushTemplate
  ( -- * Creating a Request
    createPushTemplate,
    CreatePushTemplate,

    -- * Request Lenses
    cptTemplateName,
    cptPushNotificationTemplateRequest,

    -- * Destructuring the Response
    createPushTemplateResponse,
    CreatePushTemplateResponse,

    -- * Response Lenses
    cptrsResponseStatus,
    cptrsCreateTemplateMessageBody,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createPushTemplate' smart constructor.
data CreatePushTemplate = CreatePushTemplate'
  { _cptTemplateName ::
      !Text,
    _cptPushNotificationTemplateRequest ::
      !PushNotificationTemplateRequest
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreatePushTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cptTemplateName' - The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- * 'cptPushNotificationTemplateRequest' - Undocumented member.
createPushTemplate ::
  -- | 'cptTemplateName'
  Text ->
  -- | 'cptPushNotificationTemplateRequest'
  PushNotificationTemplateRequest ->
  CreatePushTemplate
createPushTemplate pTemplateName_ pPushNotificationTemplateRequest_ =
  CreatePushTemplate'
    { _cptTemplateName = pTemplateName_,
      _cptPushNotificationTemplateRequest =
        pPushNotificationTemplateRequest_
    }

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
cptTemplateName :: Lens' CreatePushTemplate Text
cptTemplateName = lens _cptTemplateName (\s a -> s {_cptTemplateName = a})

-- | Undocumented member.
cptPushNotificationTemplateRequest :: Lens' CreatePushTemplate PushNotificationTemplateRequest
cptPushNotificationTemplateRequest = lens _cptPushNotificationTemplateRequest (\s a -> s {_cptPushNotificationTemplateRequest = a})

instance AWSRequest CreatePushTemplate where
  type Rs CreatePushTemplate = CreatePushTemplateResponse
  request = postJSON pinpoint
  response =
    receiveJSON
      ( \s h x ->
          CreatePushTemplateResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable CreatePushTemplate

instance NFData CreatePushTemplate

instance ToHeaders CreatePushTemplate where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON CreatePushTemplate where
  toJSON CreatePushTemplate' {..} =
    object
      ( catMaybes
          [ Just
              ( "PushNotificationTemplateRequest"
                  .= _cptPushNotificationTemplateRequest
              )
          ]
      )

instance ToPath CreatePushTemplate where
  toPath CreatePushTemplate' {..} =
    mconcat ["/v1/templates/", toBS _cptTemplateName, "/push"]

instance ToQuery CreatePushTemplate where
  toQuery = const mempty

-- | /See:/ 'createPushTemplateResponse' smart constructor.
data CreatePushTemplateResponse = CreatePushTemplateResponse'
  { _cptrsResponseStatus ::
      !Int,
    _cptrsCreateTemplateMessageBody ::
      !CreateTemplateMessageBody
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreatePushTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cptrsResponseStatus' - -- | The response status code.
--
-- * 'cptrsCreateTemplateMessageBody' - Undocumented member.
createPushTemplateResponse ::
  -- | 'cptrsResponseStatus'
  Int ->
  -- | 'cptrsCreateTemplateMessageBody'
  CreateTemplateMessageBody ->
  CreatePushTemplateResponse
createPushTemplateResponse
  pResponseStatus_
  pCreateTemplateMessageBody_ =
    CreatePushTemplateResponse'
      { _cptrsResponseStatus =
          pResponseStatus_,
        _cptrsCreateTemplateMessageBody = pCreateTemplateMessageBody_
      }

-- | -- | The response status code.
cptrsResponseStatus :: Lens' CreatePushTemplateResponse Int
cptrsResponseStatus = lens _cptrsResponseStatus (\s a -> s {_cptrsResponseStatus = a})

-- | Undocumented member.
cptrsCreateTemplateMessageBody :: Lens' CreatePushTemplateResponse CreateTemplateMessageBody
cptrsCreateTemplateMessageBody = lens _cptrsCreateTemplateMessageBody (\s a -> s {_cptrsCreateTemplateMessageBody = a})

instance NFData CreatePushTemplateResponse
