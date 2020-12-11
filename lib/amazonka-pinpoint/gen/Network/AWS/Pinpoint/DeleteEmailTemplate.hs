{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteEmailTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a message template for messages that were sent through the email channel.
module Network.AWS.Pinpoint.DeleteEmailTemplate
  ( -- * Creating a request
    DeleteEmailTemplate (..),
    mkDeleteEmailTemplate,

    -- ** Request lenses
    detVersion,
    detTemplateName,

    -- * Destructuring the response
    DeleteEmailTemplateResponse (..),
    mkDeleteEmailTemplateResponse,

    -- ** Response lenses
    detrsResponseStatus,
    detrsMessageBody,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteEmailTemplate' smart constructor.
data DeleteEmailTemplate = DeleteEmailTemplate'
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

-- | Creates a value of 'DeleteEmailTemplate' with the minimum fields required to make a request.
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
mkDeleteEmailTemplate ::
  -- | 'templateName'
  Lude.Text ->
  DeleteEmailTemplate
mkDeleteEmailTemplate pTemplateName_ =
  DeleteEmailTemplate'
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
detVersion :: Lens.Lens' DeleteEmailTemplate (Lude.Maybe Lude.Text)
detVersion = Lens.lens (version :: DeleteEmailTemplate -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: DeleteEmailTemplate)
{-# DEPRECATED detVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detTemplateName :: Lens.Lens' DeleteEmailTemplate Lude.Text
detTemplateName = Lens.lens (templateName :: DeleteEmailTemplate -> Lude.Text) (\s a -> s {templateName = a} :: DeleteEmailTemplate)
{-# DEPRECATED detTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

instance Lude.AWSRequest DeleteEmailTemplate where
  type Rs DeleteEmailTemplate = DeleteEmailTemplateResponse
  request = Req.delete pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteEmailTemplateResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders DeleteEmailTemplate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteEmailTemplate where
  toPath DeleteEmailTemplate' {..} =
    Lude.mconcat ["/v1/templates/", Lude.toBS templateName, "/email"]

instance Lude.ToQuery DeleteEmailTemplate where
  toQuery DeleteEmailTemplate' {..} =
    Lude.mconcat ["version" Lude.=: version]

-- | /See:/ 'mkDeleteEmailTemplateResponse' smart constructor.
data DeleteEmailTemplateResponse = DeleteEmailTemplateResponse'
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

-- | Creates a value of 'DeleteEmailTemplateResponse' with the minimum fields required to make a request.
--
-- * 'messageBody' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDeleteEmailTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'messageBody'
  MessageBody ->
  DeleteEmailTemplateResponse
mkDeleteEmailTemplateResponse pResponseStatus_ pMessageBody_ =
  DeleteEmailTemplateResponse'
    { responseStatus = pResponseStatus_,
      messageBody = pMessageBody_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrsResponseStatus :: Lens.Lens' DeleteEmailTemplateResponse Lude.Int
detrsResponseStatus = Lens.lens (responseStatus :: DeleteEmailTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteEmailTemplateResponse)
{-# DEPRECATED detrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'messageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrsMessageBody :: Lens.Lens' DeleteEmailTemplateResponse MessageBody
detrsMessageBody = Lens.lens (messageBody :: DeleteEmailTemplateResponse -> MessageBody) (\s a -> s {messageBody = a} :: DeleteEmailTemplateResponse)
{-# DEPRECATED detrsMessageBody "Use generic-lens or generic-optics with 'messageBody' instead." #-}
