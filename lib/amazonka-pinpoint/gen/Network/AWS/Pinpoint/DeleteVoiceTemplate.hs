{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteVoiceTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a message template for messages that were sent through the voice channel.
module Network.AWS.Pinpoint.DeleteVoiceTemplate
  ( -- * Creating a request
    DeleteVoiceTemplate (..),
    mkDeleteVoiceTemplate,

    -- ** Request lenses
    dvtVersion,
    dvtTemplateName,

    -- * Destructuring the response
    DeleteVoiceTemplateResponse (..),
    mkDeleteVoiceTemplateResponse,

    -- ** Response lenses
    dvtrsResponseStatus,
    dvtrsMessageBody,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteVoiceTemplate' smart constructor.
data DeleteVoiceTemplate = DeleteVoiceTemplate'
  { version ::
      Lude.Maybe Lude.Text,
    templateName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVoiceTemplate' with the minimum fields required to make a request.
--
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
mkDeleteVoiceTemplate ::
  -- | 'templateName'
  Lude.Text ->
  DeleteVoiceTemplate
mkDeleteVoiceTemplate pTemplateName_ =
  DeleteVoiceTemplate'
    { version = Lude.Nothing,
      templateName = pTemplateName_
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
dvtVersion :: Lens.Lens' DeleteVoiceTemplate (Lude.Maybe Lude.Text)
dvtVersion = Lens.lens (version :: DeleteVoiceTemplate -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: DeleteVoiceTemplate)
{-# DEPRECATED dvtVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvtTemplateName :: Lens.Lens' DeleteVoiceTemplate Lude.Text
dvtTemplateName = Lens.lens (templateName :: DeleteVoiceTemplate -> Lude.Text) (\s a -> s {templateName = a} :: DeleteVoiceTemplate)
{-# DEPRECATED dvtTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

instance Lude.AWSRequest DeleteVoiceTemplate where
  type Rs DeleteVoiceTemplate = DeleteVoiceTemplateResponse
  request = Req.delete pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteVoiceTemplateResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders DeleteVoiceTemplate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteVoiceTemplate where
  toPath DeleteVoiceTemplate' {..} =
    Lude.mconcat ["/v1/templates/", Lude.toBS templateName, "/voice"]

instance Lude.ToQuery DeleteVoiceTemplate where
  toQuery DeleteVoiceTemplate' {..} =
    Lude.mconcat ["version" Lude.=: version]

-- | /See:/ 'mkDeleteVoiceTemplateResponse' smart constructor.
data DeleteVoiceTemplateResponse = DeleteVoiceTemplateResponse'
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

-- | Creates a value of 'DeleteVoiceTemplateResponse' with the minimum fields required to make a request.
--
-- * 'messageBody' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDeleteVoiceTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'messageBody'
  MessageBody ->
  DeleteVoiceTemplateResponse
mkDeleteVoiceTemplateResponse pResponseStatus_ pMessageBody_ =
  DeleteVoiceTemplateResponse'
    { responseStatus = pResponseStatus_,
      messageBody = pMessageBody_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvtrsResponseStatus :: Lens.Lens' DeleteVoiceTemplateResponse Lude.Int
dvtrsResponseStatus = Lens.lens (responseStatus :: DeleteVoiceTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteVoiceTemplateResponse)
{-# DEPRECATED dvtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'messageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvtrsMessageBody :: Lens.Lens' DeleteVoiceTemplateResponse MessageBody
dvtrsMessageBody = Lens.lens (messageBody :: DeleteVoiceTemplateResponse -> MessageBody) (\s a -> s {messageBody = a} :: DeleteVoiceTemplateResponse)
{-# DEPRECATED dvtrsMessageBody "Use generic-lens or generic-optics with 'messageBody' instead." #-}
