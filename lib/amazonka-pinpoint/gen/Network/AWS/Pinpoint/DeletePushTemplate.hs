{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeletePushTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a message template for messages that were sent through a push notification channel.
module Network.AWS.Pinpoint.DeletePushTemplate
  ( -- * Creating a request
    DeletePushTemplate (..),
    mkDeletePushTemplate,

    -- ** Request lenses
    dptTemplateName,
    dptVersion,

    -- * Destructuring the response
    DeletePushTemplateResponse (..),
    mkDeletePushTemplateResponse,

    -- ** Response lenses
    dptrsMessageBody,
    dptrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeletePushTemplate' smart constructor.
data DeletePushTemplate = DeletePushTemplate'
  { -- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
    templateName :: Lude.Text,
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
    version :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePushTemplate' with the minimum fields required to make a request.
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
mkDeletePushTemplate ::
  -- | 'templateName'
  Lude.Text ->
  DeletePushTemplate
mkDeletePushTemplate pTemplateName_ =
  DeletePushTemplate'
    { templateName = pTemplateName_,
      version = Lude.Nothing
    }

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptTemplateName :: Lens.Lens' DeletePushTemplate Lude.Text
dptTemplateName = Lens.lens (templateName :: DeletePushTemplate -> Lude.Text) (\s a -> s {templateName = a} :: DeletePushTemplate)
{-# DEPRECATED dptTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

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
dptVersion :: Lens.Lens' DeletePushTemplate (Lude.Maybe Lude.Text)
dptVersion = Lens.lens (version :: DeletePushTemplate -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: DeletePushTemplate)
{-# DEPRECATED dptVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.AWSRequest DeletePushTemplate where
  type Rs DeletePushTemplate = DeletePushTemplateResponse
  request = Req.delete pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeletePushTemplateResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeletePushTemplate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeletePushTemplate where
  toPath DeletePushTemplate' {..} =
    Lude.mconcat ["/v1/templates/", Lude.toBS templateName, "/push"]

instance Lude.ToQuery DeletePushTemplate where
  toQuery DeletePushTemplate' {..} =
    Lude.mconcat ["version" Lude.=: version]

-- | /See:/ 'mkDeletePushTemplateResponse' smart constructor.
data DeletePushTemplateResponse = DeletePushTemplateResponse'
  { messageBody :: MessageBody,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePushTemplateResponse' with the minimum fields required to make a request.
--
-- * 'messageBody' -
-- * 'responseStatus' - The response status code.
mkDeletePushTemplateResponse ::
  -- | 'messageBody'
  MessageBody ->
  -- | 'responseStatus'
  Lude.Int ->
  DeletePushTemplateResponse
mkDeletePushTemplateResponse pMessageBody_ pResponseStatus_ =
  DeletePushTemplateResponse'
    { messageBody = pMessageBody_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'messageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrsMessageBody :: Lens.Lens' DeletePushTemplateResponse MessageBody
dptrsMessageBody = Lens.lens (messageBody :: DeletePushTemplateResponse -> MessageBody) (\s a -> s {messageBody = a} :: DeletePushTemplateResponse)
{-# DEPRECATED dptrsMessageBody "Use generic-lens or generic-optics with 'messageBody' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrsResponseStatus :: Lens.Lens' DeletePushTemplateResponse Lude.Int
dptrsResponseStatus = Lens.lens (responseStatus :: DeletePushTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeletePushTemplateResponse)
{-# DEPRECATED dptrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
