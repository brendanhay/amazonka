{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
  ( -- * Creating a request
    CreateVoiceTemplate (..),
    mkCreateVoiceTemplate,

    -- ** Request lenses
    cvtTemplateName,
    cvtVoiceTemplateRequest,

    -- * Destructuring the response
    CreateVoiceTemplateResponse (..),
    mkCreateVoiceTemplateResponse,

    -- ** Response lenses
    cvtrsCreateTemplateMessageBody,
    cvtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateVoiceTemplate' smart constructor.
data CreateVoiceTemplate = CreateVoiceTemplate'
  { -- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
    templateName :: Lude.Text,
    voiceTemplateRequest :: VoiceTemplateRequest
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateVoiceTemplate' with the minimum fields required to make a request.
--
-- * 'templateName' - The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
-- * 'voiceTemplateRequest' -
mkCreateVoiceTemplate ::
  -- | 'templateName'
  Lude.Text ->
  -- | 'voiceTemplateRequest'
  VoiceTemplateRequest ->
  CreateVoiceTemplate
mkCreateVoiceTemplate pTemplateName_ pVoiceTemplateRequest_ =
  CreateVoiceTemplate'
    { templateName = pTemplateName_,
      voiceTemplateRequest = pVoiceTemplateRequest_
    }

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvtTemplateName :: Lens.Lens' CreateVoiceTemplate Lude.Text
cvtTemplateName = Lens.lens (templateName :: CreateVoiceTemplate -> Lude.Text) (\s a -> s {templateName = a} :: CreateVoiceTemplate)
{-# DEPRECATED cvtTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'voiceTemplateRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvtVoiceTemplateRequest :: Lens.Lens' CreateVoiceTemplate VoiceTemplateRequest
cvtVoiceTemplateRequest = Lens.lens (voiceTemplateRequest :: CreateVoiceTemplate -> VoiceTemplateRequest) (\s a -> s {voiceTemplateRequest = a} :: CreateVoiceTemplate)
{-# DEPRECATED cvtVoiceTemplateRequest "Use generic-lens or generic-optics with 'voiceTemplateRequest' instead." #-}

instance Lude.AWSRequest CreateVoiceTemplate where
  type Rs CreateVoiceTemplate = CreateVoiceTemplateResponse
  request = Req.postJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateVoiceTemplateResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateVoiceTemplate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateVoiceTemplate where
  toJSON CreateVoiceTemplate' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("VoiceTemplateRequest" Lude..= voiceTemplateRequest)]
      )

instance Lude.ToPath CreateVoiceTemplate where
  toPath CreateVoiceTemplate' {..} =
    Lude.mconcat ["/v1/templates/", Lude.toBS templateName, "/voice"]

instance Lude.ToQuery CreateVoiceTemplate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateVoiceTemplateResponse' smart constructor.
data CreateVoiceTemplateResponse = CreateVoiceTemplateResponse'
  { createTemplateMessageBody :: CreateTemplateMessageBody,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateVoiceTemplateResponse' with the minimum fields required to make a request.
--
-- * 'createTemplateMessageBody' -
-- * 'responseStatus' - The response status code.
mkCreateVoiceTemplateResponse ::
  -- | 'createTemplateMessageBody'
  CreateTemplateMessageBody ->
  -- | 'responseStatus'
  Lude.Int ->
  CreateVoiceTemplateResponse
mkCreateVoiceTemplateResponse
  pCreateTemplateMessageBody_
  pResponseStatus_ =
    CreateVoiceTemplateResponse'
      { createTemplateMessageBody =
          pCreateTemplateMessageBody_,
        responseStatus = pResponseStatus_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'createTemplateMessageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvtrsCreateTemplateMessageBody :: Lens.Lens' CreateVoiceTemplateResponse CreateTemplateMessageBody
cvtrsCreateTemplateMessageBody = Lens.lens (createTemplateMessageBody :: CreateVoiceTemplateResponse -> CreateTemplateMessageBody) (\s a -> s {createTemplateMessageBody = a} :: CreateVoiceTemplateResponse)
{-# DEPRECATED cvtrsCreateTemplateMessageBody "Use generic-lens or generic-optics with 'createTemplateMessageBody' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvtrsResponseStatus :: Lens.Lens' CreateVoiceTemplateResponse Lude.Int
cvtrsResponseStatus = Lens.lens (responseStatus :: CreateVoiceTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateVoiceTemplateResponse)
{-# DEPRECATED cvtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
