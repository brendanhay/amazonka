{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdatePushTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing message template for messages that are sent through a push notification channel.
module Network.AWS.Pinpoint.UpdatePushTemplate
  ( -- * Creating a request
    UpdatePushTemplate (..),
    mkUpdatePushTemplate,

    -- ** Request lenses
    uptVersion,
    uptCreateNewVersion,
    uptTemplateName,
    uptPushNotificationTemplateRequest,

    -- * Destructuring the response
    UpdatePushTemplateResponse (..),
    mkUpdatePushTemplateResponse,

    -- ** Response lenses
    uptrsResponseStatus,
    uptrsMessageBody,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdatePushTemplate' smart constructor.
data UpdatePushTemplate = UpdatePushTemplate'
  { version ::
      Lude.Maybe Lude.Text,
    createNewVersion :: Lude.Maybe Lude.Bool,
    templateName :: Lude.Text,
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

-- | Creates a value of 'UpdatePushTemplate' with the minimum fields required to make a request.
--
-- * 'createNewVersion' - Specifies whether to save the updates as a new version of the message template. Valid values are: true, save the updates as a new version; and, false, save the updates to (overwrite) the latest existing version of the template.
--
-- If you don't specify a value for this parameter, Amazon Pinpoint saves the updates to (overwrites) the latest existing version of the template. If you specify a value of true for this parameter, don't specify a value for the version parameter. Otherwise, an error will occur.
-- * 'pushNotificationTemplateRequest' - Undocumented field.
-- * 'templateName' - The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
-- * 'version' - The unique identifier for the version of the message template to update, retrieve information about, or delete. To retrieve identifiers and other information for all the versions of a template, use the <link>Template Versions resource.
--
-- If specified, this value must match the identifier for an existing template version. If specified for an update operation, this value must match the identifier for the latest existing version of the template. This restriction helps ensure that race conditions don't occur.
-- If you don't specify a value for this parameter, Amazon Pinpoint does the following:
--
--     * For a get operation, retrieves information about the active version of the template.
--
--
--     * For an update operation, saves the updates to (overwrites) the latest existing version of the template, if the create-new-version parameter isn't used or is set to false.
--
--
--     * For a delete operation, deletes the template, including all versions of the template.
mkUpdatePushTemplate ::
  -- | 'templateName'
  Lude.Text ->
  -- | 'pushNotificationTemplateRequest'
  PushNotificationTemplateRequest ->
  UpdatePushTemplate
mkUpdatePushTemplate
  pTemplateName_
  pPushNotificationTemplateRequest_ =
    UpdatePushTemplate'
      { version = Lude.Nothing,
        createNewVersion = Lude.Nothing,
        templateName = pTemplateName_,
        pushNotificationTemplateRequest =
          pPushNotificationTemplateRequest_
      }

-- | The unique identifier for the version of the message template to update, retrieve information about, or delete. To retrieve identifiers and other information for all the versions of a template, use the <link>Template Versions resource.
--
-- If specified, this value must match the identifier for an existing template version. If specified for an update operation, this value must match the identifier for the latest existing version of the template. This restriction helps ensure that race conditions don't occur.
-- If you don't specify a value for this parameter, Amazon Pinpoint does the following:
--
--     * For a get operation, retrieves information about the active version of the template.
--
--
--     * For an update operation, saves the updates to (overwrites) the latest existing version of the template, if the create-new-version parameter isn't used or is set to false.
--
--
--     * For a delete operation, deletes the template, including all versions of the template.
--
--
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptVersion :: Lens.Lens' UpdatePushTemplate (Lude.Maybe Lude.Text)
uptVersion = Lens.lens (version :: UpdatePushTemplate -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: UpdatePushTemplate)
{-# DEPRECATED uptVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | Specifies whether to save the updates as a new version of the message template. Valid values are: true, save the updates as a new version; and, false, save the updates to (overwrite) the latest existing version of the template.
--
-- If you don't specify a value for this parameter, Amazon Pinpoint saves the updates to (overwrites) the latest existing version of the template. If you specify a value of true for this parameter, don't specify a value for the version parameter. Otherwise, an error will occur.
--
-- /Note:/ Consider using 'createNewVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptCreateNewVersion :: Lens.Lens' UpdatePushTemplate (Lude.Maybe Lude.Bool)
uptCreateNewVersion = Lens.lens (createNewVersion :: UpdatePushTemplate -> Lude.Maybe Lude.Bool) (\s a -> s {createNewVersion = a} :: UpdatePushTemplate)
{-# DEPRECATED uptCreateNewVersion "Use generic-lens or generic-optics with 'createNewVersion' instead." #-}

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptTemplateName :: Lens.Lens' UpdatePushTemplate Lude.Text
uptTemplateName = Lens.lens (templateName :: UpdatePushTemplate -> Lude.Text) (\s a -> s {templateName = a} :: UpdatePushTemplate)
{-# DEPRECATED uptTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'pushNotificationTemplateRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptPushNotificationTemplateRequest :: Lens.Lens' UpdatePushTemplate PushNotificationTemplateRequest
uptPushNotificationTemplateRequest = Lens.lens (pushNotificationTemplateRequest :: UpdatePushTemplate -> PushNotificationTemplateRequest) (\s a -> s {pushNotificationTemplateRequest = a} :: UpdatePushTemplate)
{-# DEPRECATED uptPushNotificationTemplateRequest "Use generic-lens or generic-optics with 'pushNotificationTemplateRequest' instead." #-}

instance Lude.AWSRequest UpdatePushTemplate where
  type Rs UpdatePushTemplate = UpdatePushTemplateResponse
  request = Req.putJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdatePushTemplateResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders UpdatePushTemplate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdatePushTemplate where
  toJSON UpdatePushTemplate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "PushNotificationTemplateRequest"
                  Lude..= pushNotificationTemplateRequest
              )
          ]
      )

instance Lude.ToPath UpdatePushTemplate where
  toPath UpdatePushTemplate' {..} =
    Lude.mconcat ["/v1/templates/", Lude.toBS templateName, "/push"]

instance Lude.ToQuery UpdatePushTemplate where
  toQuery UpdatePushTemplate' {..} =
    Lude.mconcat
      [ "version" Lude.=: version,
        "create-new-version" Lude.=: createNewVersion
      ]

-- | /See:/ 'mkUpdatePushTemplateResponse' smart constructor.
data UpdatePushTemplateResponse = UpdatePushTemplateResponse'
  { responseStatus ::
      Lude.Int,
    messageBody :: MessageBody
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePushTemplateResponse' with the minimum fields required to make a request.
--
-- * 'messageBody' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkUpdatePushTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'messageBody'
  MessageBody ->
  UpdatePushTemplateResponse
mkUpdatePushTemplateResponse pResponseStatus_ pMessageBody_ =
  UpdatePushTemplateResponse'
    { responseStatus = pResponseStatus_,
      messageBody = pMessageBody_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptrsResponseStatus :: Lens.Lens' UpdatePushTemplateResponse Lude.Int
uptrsResponseStatus = Lens.lens (responseStatus :: UpdatePushTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdatePushTemplateResponse)
{-# DEPRECATED uptrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'messageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptrsMessageBody :: Lens.Lens' UpdatePushTemplateResponse MessageBody
uptrsMessageBody = Lens.lens (messageBody :: UpdatePushTemplateResponse -> MessageBody) (\s a -> s {messageBody = a} :: UpdatePushTemplateResponse)
{-# DEPRECATED uptrsMessageBody "Use generic-lens or generic-optics with 'messageBody' instead." #-}
