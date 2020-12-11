{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
  ( -- * Creating a request
    CreatePushTemplate (..),
    mkCreatePushTemplate,

    -- ** Request lenses
    cptTemplateName,
    cptPushNotificationTemplateRequest,

    -- * Destructuring the response
    CreatePushTemplateResponse (..),
    mkCreatePushTemplateResponse,

    -- ** Response lenses
    cptrsResponseStatus,
    cptrsCreateTemplateMessageBody,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreatePushTemplate' smart constructor.
data CreatePushTemplate = CreatePushTemplate'
  { templateName ::
      Lude.Text,
    pushNotificationTemplateRequest ::
      PushNotificationTemplateRequest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePushTemplate' with the minimum fields required to make a request.
--
-- * 'pushNotificationTemplateRequest' - Undocumented field.
-- * 'templateName' - The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
mkCreatePushTemplate ::
  -- | 'templateName'
  Lude.Text ->
  -- | 'pushNotificationTemplateRequest'
  PushNotificationTemplateRequest ->
  CreatePushTemplate
mkCreatePushTemplate
  pTemplateName_
  pPushNotificationTemplateRequest_ =
    CreatePushTemplate'
      { templateName = pTemplateName_,
        pushNotificationTemplateRequest =
          pPushNotificationTemplateRequest_
      }

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptTemplateName :: Lens.Lens' CreatePushTemplate Lude.Text
cptTemplateName = Lens.lens (templateName :: CreatePushTemplate -> Lude.Text) (\s a -> s {templateName = a} :: CreatePushTemplate)
{-# DEPRECATED cptTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'pushNotificationTemplateRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptPushNotificationTemplateRequest :: Lens.Lens' CreatePushTemplate PushNotificationTemplateRequest
cptPushNotificationTemplateRequest = Lens.lens (pushNotificationTemplateRequest :: CreatePushTemplate -> PushNotificationTemplateRequest) (\s a -> s {pushNotificationTemplateRequest = a} :: CreatePushTemplate)
{-# DEPRECATED cptPushNotificationTemplateRequest "Use generic-lens or generic-optics with 'pushNotificationTemplateRequest' instead." #-}

instance Lude.AWSRequest CreatePushTemplate where
  type Rs CreatePushTemplate = CreatePushTemplateResponse
  request = Req.postJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreatePushTemplateResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders CreatePushTemplate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreatePushTemplate where
  toJSON CreatePushTemplate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "PushNotificationTemplateRequest"
                  Lude..= pushNotificationTemplateRequest
              )
          ]
      )

instance Lude.ToPath CreatePushTemplate where
  toPath CreatePushTemplate' {..} =
    Lude.mconcat ["/v1/templates/", Lude.toBS templateName, "/push"]

instance Lude.ToQuery CreatePushTemplate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreatePushTemplateResponse' smart constructor.
data CreatePushTemplateResponse = CreatePushTemplateResponse'
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

-- | Creates a value of 'CreatePushTemplateResponse' with the minimum fields required to make a request.
--
-- * 'createTemplateMessageBody' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkCreatePushTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'createTemplateMessageBody'
  CreateTemplateMessageBody ->
  CreatePushTemplateResponse
mkCreatePushTemplateResponse
  pResponseStatus_
  pCreateTemplateMessageBody_ =
    CreatePushTemplateResponse'
      { responseStatus = pResponseStatus_,
        createTemplateMessageBody = pCreateTemplateMessageBody_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptrsResponseStatus :: Lens.Lens' CreatePushTemplateResponse Lude.Int
cptrsResponseStatus = Lens.lens (responseStatus :: CreatePushTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePushTemplateResponse)
{-# DEPRECATED cptrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'createTemplateMessageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptrsCreateTemplateMessageBody :: Lens.Lens' CreatePushTemplateResponse CreateTemplateMessageBody
cptrsCreateTemplateMessageBody = Lens.lens (createTemplateMessageBody :: CreatePushTemplateResponse -> CreateTemplateMessageBody) (\s a -> s {createTemplateMessageBody = a} :: CreatePushTemplateResponse)
{-# DEPRECATED cptrsCreateTemplateMessageBody "Use generic-lens or generic-optics with 'createTemplateMessageBody' instead." #-}
