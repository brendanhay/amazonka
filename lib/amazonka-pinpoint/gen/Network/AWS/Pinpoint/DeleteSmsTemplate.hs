{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteSmsTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a message template for messages that were sent through the SMS channel.
module Network.AWS.Pinpoint.DeleteSmsTemplate
  ( -- * Creating a request
    DeleteSmsTemplate (..),
    mkDeleteSmsTemplate,

    -- ** Request lenses
    dstVersion,
    dstTemplateName,

    -- * Destructuring the response
    DeleteSmsTemplateResponse (..),
    mkDeleteSmsTemplateResponse,

    -- ** Response lenses
    dstrsResponseStatus,
    dstrsMessageBody,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteSmsTemplate' smart constructor.
data DeleteSmsTemplate = DeleteSmsTemplate'
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

-- | Creates a value of 'DeleteSmsTemplate' with the minimum fields required to make a request.
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
mkDeleteSmsTemplate ::
  -- | 'templateName'
  Lude.Text ->
  DeleteSmsTemplate
mkDeleteSmsTemplate pTemplateName_ =
  DeleteSmsTemplate'
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
dstVersion :: Lens.Lens' DeleteSmsTemplate (Lude.Maybe Lude.Text)
dstVersion = Lens.lens (version :: DeleteSmsTemplate -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: DeleteSmsTemplate)
{-# DEPRECATED dstVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dstTemplateName :: Lens.Lens' DeleteSmsTemplate Lude.Text
dstTemplateName = Lens.lens (templateName :: DeleteSmsTemplate -> Lude.Text) (\s a -> s {templateName = a} :: DeleteSmsTemplate)
{-# DEPRECATED dstTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

instance Lude.AWSRequest DeleteSmsTemplate where
  type Rs DeleteSmsTemplate = DeleteSmsTemplateResponse
  request = Req.delete pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteSmsTemplateResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders DeleteSmsTemplate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteSmsTemplate where
  toPath DeleteSmsTemplate' {..} =
    Lude.mconcat ["/v1/templates/", Lude.toBS templateName, "/sms"]

instance Lude.ToQuery DeleteSmsTemplate where
  toQuery DeleteSmsTemplate' {..} =
    Lude.mconcat ["version" Lude.=: version]

-- | /See:/ 'mkDeleteSmsTemplateResponse' smart constructor.
data DeleteSmsTemplateResponse = DeleteSmsTemplateResponse'
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

-- | Creates a value of 'DeleteSmsTemplateResponse' with the minimum fields required to make a request.
--
-- * 'messageBody' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDeleteSmsTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'messageBody'
  MessageBody ->
  DeleteSmsTemplateResponse
mkDeleteSmsTemplateResponse pResponseStatus_ pMessageBody_ =
  DeleteSmsTemplateResponse'
    { responseStatus = pResponseStatus_,
      messageBody = pMessageBody_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dstrsResponseStatus :: Lens.Lens' DeleteSmsTemplateResponse Lude.Int
dstrsResponseStatus = Lens.lens (responseStatus :: DeleteSmsTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteSmsTemplateResponse)
{-# DEPRECATED dstrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'messageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dstrsMessageBody :: Lens.Lens' DeleteSmsTemplateResponse MessageBody
dstrsMessageBody = Lens.lens (messageBody :: DeleteSmsTemplateResponse -> MessageBody) (\s a -> s {messageBody = a} :: DeleteSmsTemplateResponse)
{-# DEPRECATED dstrsMessageBody "Use generic-lens or generic-optics with 'messageBody' instead." #-}
