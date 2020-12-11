{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
  ( -- * Creating a request
    CreateEmailTemplate (..),
    mkCreateEmailTemplate,

    -- ** Request lenses
    cetTemplateName,
    cetEmailTemplateRequest,

    -- * Destructuring the response
    CreateEmailTemplateResponse (..),
    mkCreateEmailTemplateResponse,

    -- ** Response lenses
    cetrsResponseStatus,
    cetrsCreateTemplateMessageBody,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateEmailTemplate' smart constructor.
data CreateEmailTemplate = CreateEmailTemplate'
  { templateName ::
      Lude.Text,
    emailTemplateRequest :: EmailTemplateRequest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateEmailTemplate' with the minimum fields required to make a request.
--
-- * 'emailTemplateRequest' - Undocumented field.
-- * 'templateName' - The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
mkCreateEmailTemplate ::
  -- | 'templateName'
  Lude.Text ->
  -- | 'emailTemplateRequest'
  EmailTemplateRequest ->
  CreateEmailTemplate
mkCreateEmailTemplate pTemplateName_ pEmailTemplateRequest_ =
  CreateEmailTemplate'
    { templateName = pTemplateName_,
      emailTemplateRequest = pEmailTemplateRequest_
    }

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetTemplateName :: Lens.Lens' CreateEmailTemplate Lude.Text
cetTemplateName = Lens.lens (templateName :: CreateEmailTemplate -> Lude.Text) (\s a -> s {templateName = a} :: CreateEmailTemplate)
{-# DEPRECATED cetTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'emailTemplateRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetEmailTemplateRequest :: Lens.Lens' CreateEmailTemplate EmailTemplateRequest
cetEmailTemplateRequest = Lens.lens (emailTemplateRequest :: CreateEmailTemplate -> EmailTemplateRequest) (\s a -> s {emailTemplateRequest = a} :: CreateEmailTemplate)
{-# DEPRECATED cetEmailTemplateRequest "Use generic-lens or generic-optics with 'emailTemplateRequest' instead." #-}

instance Lude.AWSRequest CreateEmailTemplate where
  type Rs CreateEmailTemplate = CreateEmailTemplateResponse
  request = Req.postJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateEmailTemplateResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders CreateEmailTemplate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateEmailTemplate where
  toJSON CreateEmailTemplate' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("EmailTemplateRequest" Lude..= emailTemplateRequest)]
      )

instance Lude.ToPath CreateEmailTemplate where
  toPath CreateEmailTemplate' {..} =
    Lude.mconcat ["/v1/templates/", Lude.toBS templateName, "/email"]

instance Lude.ToQuery CreateEmailTemplate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateEmailTemplateResponse' smart constructor.
data CreateEmailTemplateResponse = CreateEmailTemplateResponse'
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

-- | Creates a value of 'CreateEmailTemplateResponse' with the minimum fields required to make a request.
--
-- * 'createTemplateMessageBody' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkCreateEmailTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'createTemplateMessageBody'
  CreateTemplateMessageBody ->
  CreateEmailTemplateResponse
mkCreateEmailTemplateResponse
  pResponseStatus_
  pCreateTemplateMessageBody_ =
    CreateEmailTemplateResponse'
      { responseStatus = pResponseStatus_,
        createTemplateMessageBody = pCreateTemplateMessageBody_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetrsResponseStatus :: Lens.Lens' CreateEmailTemplateResponse Lude.Int
cetrsResponseStatus = Lens.lens (responseStatus :: CreateEmailTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateEmailTemplateResponse)
{-# DEPRECATED cetrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'createTemplateMessageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetrsCreateTemplateMessageBody :: Lens.Lens' CreateEmailTemplateResponse CreateTemplateMessageBody
cetrsCreateTemplateMessageBody = Lens.lens (createTemplateMessageBody :: CreateEmailTemplateResponse -> CreateTemplateMessageBody) (\s a -> s {createTemplateMessageBody = a} :: CreateEmailTemplateResponse)
{-# DEPRECATED cetrsCreateTemplateMessageBody "Use generic-lens or generic-optics with 'createTemplateMessageBody' instead." #-}
