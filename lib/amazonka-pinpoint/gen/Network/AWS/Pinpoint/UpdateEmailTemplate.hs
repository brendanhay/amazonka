{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateEmailTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing message template for messages that are sent through the email channel.
module Network.AWS.Pinpoint.UpdateEmailTemplate
  ( -- * Creating a request
    UpdateEmailTemplate (..),
    mkUpdateEmailTemplate,

    -- ** Request lenses
    uetVersion,
    uetCreateNewVersion,
    uetTemplateName,
    uetEmailTemplateRequest,

    -- * Destructuring the response
    UpdateEmailTemplateResponse (..),
    mkUpdateEmailTemplateResponse,

    -- ** Response lenses
    uetrsResponseStatus,
    uetrsMessageBody,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateEmailTemplate' smart constructor.
data UpdateEmailTemplate = UpdateEmailTemplate'
  { version ::
      Lude.Maybe Lude.Text,
    createNewVersion :: Lude.Maybe Lude.Bool,
    templateName :: Lude.Text,
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

-- | Creates a value of 'UpdateEmailTemplate' with the minimum fields required to make a request.
--
-- * 'createNewVersion' - Specifies whether to save the updates as a new version of the message template. Valid values are: true, save the updates as a new version; and, false, save the updates to (overwrite) the latest existing version of the template.
--
-- If you don't specify a value for this parameter, Amazon Pinpoint saves the updates to (overwrites) the latest existing version of the template. If you specify a value of true for this parameter, don't specify a value for the version parameter. Otherwise, an error will occur.
-- * 'emailTemplateRequest' - Undocumented field.
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
mkUpdateEmailTemplate ::
  -- | 'templateName'
  Lude.Text ->
  -- | 'emailTemplateRequest'
  EmailTemplateRequest ->
  UpdateEmailTemplate
mkUpdateEmailTemplate pTemplateName_ pEmailTemplateRequest_ =
  UpdateEmailTemplate'
    { version = Lude.Nothing,
      createNewVersion = Lude.Nothing,
      templateName = pTemplateName_,
      emailTemplateRequest = pEmailTemplateRequest_
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
uetVersion :: Lens.Lens' UpdateEmailTemplate (Lude.Maybe Lude.Text)
uetVersion = Lens.lens (version :: UpdateEmailTemplate -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: UpdateEmailTemplate)
{-# DEPRECATED uetVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | Specifies whether to save the updates as a new version of the message template. Valid values are: true, save the updates as a new version; and, false, save the updates to (overwrite) the latest existing version of the template.
--
-- If you don't specify a value for this parameter, Amazon Pinpoint saves the updates to (overwrites) the latest existing version of the template. If you specify a value of true for this parameter, don't specify a value for the version parameter. Otherwise, an error will occur.
--
-- /Note:/ Consider using 'createNewVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uetCreateNewVersion :: Lens.Lens' UpdateEmailTemplate (Lude.Maybe Lude.Bool)
uetCreateNewVersion = Lens.lens (createNewVersion :: UpdateEmailTemplate -> Lude.Maybe Lude.Bool) (\s a -> s {createNewVersion = a} :: UpdateEmailTemplate)
{-# DEPRECATED uetCreateNewVersion "Use generic-lens or generic-optics with 'createNewVersion' instead." #-}

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uetTemplateName :: Lens.Lens' UpdateEmailTemplate Lude.Text
uetTemplateName = Lens.lens (templateName :: UpdateEmailTemplate -> Lude.Text) (\s a -> s {templateName = a} :: UpdateEmailTemplate)
{-# DEPRECATED uetTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'emailTemplateRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uetEmailTemplateRequest :: Lens.Lens' UpdateEmailTemplate EmailTemplateRequest
uetEmailTemplateRequest = Lens.lens (emailTemplateRequest :: UpdateEmailTemplate -> EmailTemplateRequest) (\s a -> s {emailTemplateRequest = a} :: UpdateEmailTemplate)
{-# DEPRECATED uetEmailTemplateRequest "Use generic-lens or generic-optics with 'emailTemplateRequest' instead." #-}

instance Lude.AWSRequest UpdateEmailTemplate where
  type Rs UpdateEmailTemplate = UpdateEmailTemplateResponse
  request = Req.putJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateEmailTemplateResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders UpdateEmailTemplate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateEmailTemplate where
  toJSON UpdateEmailTemplate' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("EmailTemplateRequest" Lude..= emailTemplateRequest)]
      )

instance Lude.ToPath UpdateEmailTemplate where
  toPath UpdateEmailTemplate' {..} =
    Lude.mconcat ["/v1/templates/", Lude.toBS templateName, "/email"]

instance Lude.ToQuery UpdateEmailTemplate where
  toQuery UpdateEmailTemplate' {..} =
    Lude.mconcat
      [ "version" Lude.=: version,
        "create-new-version" Lude.=: createNewVersion
      ]

-- | /See:/ 'mkUpdateEmailTemplateResponse' smart constructor.
data UpdateEmailTemplateResponse = UpdateEmailTemplateResponse'
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

-- | Creates a value of 'UpdateEmailTemplateResponse' with the minimum fields required to make a request.
--
-- * 'messageBody' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkUpdateEmailTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'messageBody'
  MessageBody ->
  UpdateEmailTemplateResponse
mkUpdateEmailTemplateResponse pResponseStatus_ pMessageBody_ =
  UpdateEmailTemplateResponse'
    { responseStatus = pResponseStatus_,
      messageBody = pMessageBody_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uetrsResponseStatus :: Lens.Lens' UpdateEmailTemplateResponse Lude.Int
uetrsResponseStatus = Lens.lens (responseStatus :: UpdateEmailTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateEmailTemplateResponse)
{-# DEPRECATED uetrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'messageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uetrsMessageBody :: Lens.Lens' UpdateEmailTemplateResponse MessageBody
uetrsMessageBody = Lens.lens (messageBody :: UpdateEmailTemplateResponse -> MessageBody) (\s a -> s {messageBody = a} :: UpdateEmailTemplateResponse)
{-# DEPRECATED uetrsMessageBody "Use generic-lens or generic-optics with 'messageBody' instead." #-}
