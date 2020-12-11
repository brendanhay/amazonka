{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
  ( -- * Creating a request
    CreateSmsTemplate (..),
    mkCreateSmsTemplate,

    -- ** Request lenses
    cstTemplateName,
    cstSMSTemplateRequest,

    -- * Destructuring the response
    CreateSmsTemplateResponse (..),
    mkCreateSmsTemplateResponse,

    -- ** Response lenses
    cstrsResponseStatus,
    cstrsCreateTemplateMessageBody,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateSmsTemplate' smart constructor.
data CreateSmsTemplate = CreateSmsTemplate'
  { templateName ::
      Lude.Text,
    sMSTemplateRequest :: SMSTemplateRequest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSmsTemplate' with the minimum fields required to make a request.
--
-- * 'sMSTemplateRequest' - Undocumented field.
-- * 'templateName' - The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
mkCreateSmsTemplate ::
  -- | 'templateName'
  Lude.Text ->
  -- | 'sMSTemplateRequest'
  SMSTemplateRequest ->
  CreateSmsTemplate
mkCreateSmsTemplate pTemplateName_ pSMSTemplateRequest_ =
  CreateSmsTemplate'
    { templateName = pTemplateName_,
      sMSTemplateRequest = pSMSTemplateRequest_
    }

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstTemplateName :: Lens.Lens' CreateSmsTemplate Lude.Text
cstTemplateName = Lens.lens (templateName :: CreateSmsTemplate -> Lude.Text) (\s a -> s {templateName = a} :: CreateSmsTemplate)
{-# DEPRECATED cstTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'sMSTemplateRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstSMSTemplateRequest :: Lens.Lens' CreateSmsTemplate SMSTemplateRequest
cstSMSTemplateRequest = Lens.lens (sMSTemplateRequest :: CreateSmsTemplate -> SMSTemplateRequest) (\s a -> s {sMSTemplateRequest = a} :: CreateSmsTemplate)
{-# DEPRECATED cstSMSTemplateRequest "Use generic-lens or generic-optics with 'sMSTemplateRequest' instead." #-}

instance Lude.AWSRequest CreateSmsTemplate where
  type Rs CreateSmsTemplate = CreateSmsTemplateResponse
  request = Req.postJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateSmsTemplateResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders CreateSmsTemplate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateSmsTemplate where
  toJSON CreateSmsTemplate' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("SMSTemplateRequest" Lude..= sMSTemplateRequest)]
      )

instance Lude.ToPath CreateSmsTemplate where
  toPath CreateSmsTemplate' {..} =
    Lude.mconcat ["/v1/templates/", Lude.toBS templateName, "/sms"]

instance Lude.ToQuery CreateSmsTemplate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateSmsTemplateResponse' smart constructor.
data CreateSmsTemplateResponse = CreateSmsTemplateResponse'
  { responseStatus ::
      Lude.Int,
    createTemplateMessageBody ::
      CreateTemplateMessageBody
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSmsTemplateResponse' with the minimum fields required to make a request.
--
-- * 'createTemplateMessageBody' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkCreateSmsTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'createTemplateMessageBody'
  CreateTemplateMessageBody ->
  CreateSmsTemplateResponse
mkCreateSmsTemplateResponse
  pResponseStatus_
  pCreateTemplateMessageBody_ =
    CreateSmsTemplateResponse'
      { responseStatus = pResponseStatus_,
        createTemplateMessageBody = pCreateTemplateMessageBody_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstrsResponseStatus :: Lens.Lens' CreateSmsTemplateResponse Lude.Int
cstrsResponseStatus = Lens.lens (responseStatus :: CreateSmsTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateSmsTemplateResponse)
{-# DEPRECATED cstrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'createTemplateMessageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cstrsCreateTemplateMessageBody :: Lens.Lens' CreateSmsTemplateResponse CreateTemplateMessageBody
cstrsCreateTemplateMessageBody = Lens.lens (createTemplateMessageBody :: CreateSmsTemplateResponse -> CreateTemplateMessageBody) (\s a -> s {createTemplateMessageBody = a} :: CreateSmsTemplateResponse)
{-# DEPRECATED cstrsCreateTemplateMessageBody "Use generic-lens or generic-optics with 'createTemplateMessageBody' instead." #-}
