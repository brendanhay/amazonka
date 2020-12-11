{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateVoiceTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing message template for messages that are sent through the voice channel.
module Network.AWS.Pinpoint.UpdateVoiceTemplate
  ( -- * Creating a request
    UpdateVoiceTemplate (..),
    mkUpdateVoiceTemplate,

    -- ** Request lenses
    uvtVersion,
    uvtCreateNewVersion,
    uvtTemplateName,
    uvtVoiceTemplateRequest,

    -- * Destructuring the response
    UpdateVoiceTemplateResponse (..),
    mkUpdateVoiceTemplateResponse,

    -- ** Response lenses
    uvtrsResponseStatus,
    uvtrsMessageBody,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateVoiceTemplate' smart constructor.
data UpdateVoiceTemplate = UpdateVoiceTemplate'
  { version ::
      Lude.Maybe Lude.Text,
    createNewVersion :: Lude.Maybe Lude.Bool,
    templateName :: Lude.Text,
    voiceTemplateRequest :: VoiceTemplateRequest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateVoiceTemplate' with the minimum fields required to make a request.
--
-- * 'createNewVersion' - Specifies whether to save the updates as a new version of the message template. Valid values are: true, save the updates as a new version; and, false, save the updates to (overwrite) the latest existing version of the template.
--
-- If you don't specify a value for this parameter, Amazon Pinpoint saves the updates to (overwrites) the latest existing version of the template. If you specify a value of true for this parameter, don't specify a value for the version parameter. Otherwise, an error will occur.
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
--
--
-- * 'voiceTemplateRequest' - Undocumented field.
mkUpdateVoiceTemplate ::
  -- | 'templateName'
  Lude.Text ->
  -- | 'voiceTemplateRequest'
  VoiceTemplateRequest ->
  UpdateVoiceTemplate
mkUpdateVoiceTemplate pTemplateName_ pVoiceTemplateRequest_ =
  UpdateVoiceTemplate'
    { version = Lude.Nothing,
      createNewVersion = Lude.Nothing,
      templateName = pTemplateName_,
      voiceTemplateRequest = pVoiceTemplateRequest_
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
uvtVersion :: Lens.Lens' UpdateVoiceTemplate (Lude.Maybe Lude.Text)
uvtVersion = Lens.lens (version :: UpdateVoiceTemplate -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: UpdateVoiceTemplate)
{-# DEPRECATED uvtVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | Specifies whether to save the updates as a new version of the message template. Valid values are: true, save the updates as a new version; and, false, save the updates to (overwrite) the latest existing version of the template.
--
-- If you don't specify a value for this parameter, Amazon Pinpoint saves the updates to (overwrites) the latest existing version of the template. If you specify a value of true for this parameter, don't specify a value for the version parameter. Otherwise, an error will occur.
--
-- /Note:/ Consider using 'createNewVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvtCreateNewVersion :: Lens.Lens' UpdateVoiceTemplate (Lude.Maybe Lude.Bool)
uvtCreateNewVersion = Lens.lens (createNewVersion :: UpdateVoiceTemplate -> Lude.Maybe Lude.Bool) (\s a -> s {createNewVersion = a} :: UpdateVoiceTemplate)
{-# DEPRECATED uvtCreateNewVersion "Use generic-lens or generic-optics with 'createNewVersion' instead." #-}

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvtTemplateName :: Lens.Lens' UpdateVoiceTemplate Lude.Text
uvtTemplateName = Lens.lens (templateName :: UpdateVoiceTemplate -> Lude.Text) (\s a -> s {templateName = a} :: UpdateVoiceTemplate)
{-# DEPRECATED uvtTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'voiceTemplateRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvtVoiceTemplateRequest :: Lens.Lens' UpdateVoiceTemplate VoiceTemplateRequest
uvtVoiceTemplateRequest = Lens.lens (voiceTemplateRequest :: UpdateVoiceTemplate -> VoiceTemplateRequest) (\s a -> s {voiceTemplateRequest = a} :: UpdateVoiceTemplate)
{-# DEPRECATED uvtVoiceTemplateRequest "Use generic-lens or generic-optics with 'voiceTemplateRequest' instead." #-}

instance Lude.AWSRequest UpdateVoiceTemplate where
  type Rs UpdateVoiceTemplate = UpdateVoiceTemplateResponse
  request = Req.putJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateVoiceTemplateResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders UpdateVoiceTemplate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateVoiceTemplate where
  toJSON UpdateVoiceTemplate' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("VoiceTemplateRequest" Lude..= voiceTemplateRequest)]
      )

instance Lude.ToPath UpdateVoiceTemplate where
  toPath UpdateVoiceTemplate' {..} =
    Lude.mconcat ["/v1/templates/", Lude.toBS templateName, "/voice"]

instance Lude.ToQuery UpdateVoiceTemplate where
  toQuery UpdateVoiceTemplate' {..} =
    Lude.mconcat
      [ "version" Lude.=: version,
        "create-new-version" Lude.=: createNewVersion
      ]

-- | /See:/ 'mkUpdateVoiceTemplateResponse' smart constructor.
data UpdateVoiceTemplateResponse = UpdateVoiceTemplateResponse'
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

-- | Creates a value of 'UpdateVoiceTemplateResponse' with the minimum fields required to make a request.
--
-- * 'messageBody' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkUpdateVoiceTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'messageBody'
  MessageBody ->
  UpdateVoiceTemplateResponse
mkUpdateVoiceTemplateResponse pResponseStatus_ pMessageBody_ =
  UpdateVoiceTemplateResponse'
    { responseStatus = pResponseStatus_,
      messageBody = pMessageBody_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvtrsResponseStatus :: Lens.Lens' UpdateVoiceTemplateResponse Lude.Int
uvtrsResponseStatus = Lens.lens (responseStatus :: UpdateVoiceTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateVoiceTemplateResponse)
{-# DEPRECATED uvtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'messageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvtrsMessageBody :: Lens.Lens' UpdateVoiceTemplateResponse MessageBody
uvtrsMessageBody = Lens.lens (messageBody :: UpdateVoiceTemplateResponse -> MessageBody) (\s a -> s {messageBody = a} :: UpdateVoiceTemplateResponse)
{-# DEPRECATED uvtrsMessageBody "Use generic-lens or generic-optics with 'messageBody' instead." #-}
