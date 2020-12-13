{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateSmsTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing message template for messages that are sent through the SMS channel.
module Network.AWS.Pinpoint.UpdateSmsTemplate
  ( -- * Creating a request
    UpdateSmsTemplate (..),
    mkUpdateSmsTemplate,

    -- ** Request lenses
    ustSMSTemplateRequest,
    ustTemplateName,
    ustVersion,
    ustCreateNewVersion,

    -- * Destructuring the response
    UpdateSmsTemplateResponse (..),
    mkUpdateSmsTemplateResponse,

    -- ** Response lenses
    ustrsMessageBody,
    ustrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateSmsTemplate' smart constructor.
data UpdateSmsTemplate = UpdateSmsTemplate'
  { sMSTemplateRequest :: SMSTemplateRequest,
    -- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
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
    version :: Lude.Maybe Lude.Text,
    -- | Specifies whether to save the updates as a new version of the message template. Valid values are: true, save the updates as a new version; and, false, save the updates to (overwrite) the latest existing version of the template.
    --
    -- If you don't specify a value for this parameter, Amazon Pinpoint saves the updates to (overwrites) the latest existing version of the template. If you specify a value of true for this parameter, don't specify a value for the version parameter. Otherwise, an error will occur.
    createNewVersion :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSmsTemplate' with the minimum fields required to make a request.
--
-- * 'sMSTemplateRequest' -
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
-- * 'createNewVersion' - Specifies whether to save the updates as a new version of the message template. Valid values are: true, save the updates as a new version; and, false, save the updates to (overwrite) the latest existing version of the template.
--
-- If you don't specify a value for this parameter, Amazon Pinpoint saves the updates to (overwrites) the latest existing version of the template. If you specify a value of true for this parameter, don't specify a value for the version parameter. Otherwise, an error will occur.
mkUpdateSmsTemplate ::
  -- | 'sMSTemplateRequest'
  SMSTemplateRequest ->
  -- | 'templateName'
  Lude.Text ->
  UpdateSmsTemplate
mkUpdateSmsTemplate pSMSTemplateRequest_ pTemplateName_ =
  UpdateSmsTemplate'
    { sMSTemplateRequest = pSMSTemplateRequest_,
      templateName = pTemplateName_,
      version = Lude.Nothing,
      createNewVersion = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'sMSTemplateRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ustSMSTemplateRequest :: Lens.Lens' UpdateSmsTemplate SMSTemplateRequest
ustSMSTemplateRequest = Lens.lens (sMSTemplateRequest :: UpdateSmsTemplate -> SMSTemplateRequest) (\s a -> s {sMSTemplateRequest = a} :: UpdateSmsTemplate)
{-# DEPRECATED ustSMSTemplateRequest "Use generic-lens or generic-optics with 'sMSTemplateRequest' instead." #-}

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ustTemplateName :: Lens.Lens' UpdateSmsTemplate Lude.Text
ustTemplateName = Lens.lens (templateName :: UpdateSmsTemplate -> Lude.Text) (\s a -> s {templateName = a} :: UpdateSmsTemplate)
{-# DEPRECATED ustTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

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
ustVersion :: Lens.Lens' UpdateSmsTemplate (Lude.Maybe Lude.Text)
ustVersion = Lens.lens (version :: UpdateSmsTemplate -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: UpdateSmsTemplate)
{-# DEPRECATED ustVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | Specifies whether to save the updates as a new version of the message template. Valid values are: true, save the updates as a new version; and, false, save the updates to (overwrite) the latest existing version of the template.
--
-- If you don't specify a value for this parameter, Amazon Pinpoint saves the updates to (overwrites) the latest existing version of the template. If you specify a value of true for this parameter, don't specify a value for the version parameter. Otherwise, an error will occur.
--
-- /Note:/ Consider using 'createNewVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ustCreateNewVersion :: Lens.Lens' UpdateSmsTemplate (Lude.Maybe Lude.Bool)
ustCreateNewVersion = Lens.lens (createNewVersion :: UpdateSmsTemplate -> Lude.Maybe Lude.Bool) (\s a -> s {createNewVersion = a} :: UpdateSmsTemplate)
{-# DEPRECATED ustCreateNewVersion "Use generic-lens or generic-optics with 'createNewVersion' instead." #-}

instance Lude.AWSRequest UpdateSmsTemplate where
  type Rs UpdateSmsTemplate = UpdateSmsTemplateResponse
  request = Req.putJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateSmsTemplateResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateSmsTemplate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateSmsTemplate where
  toJSON UpdateSmsTemplate' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("SMSTemplateRequest" Lude..= sMSTemplateRequest)]
      )

instance Lude.ToPath UpdateSmsTemplate where
  toPath UpdateSmsTemplate' {..} =
    Lude.mconcat ["/v1/templates/", Lude.toBS templateName, "/sms"]

instance Lude.ToQuery UpdateSmsTemplate where
  toQuery UpdateSmsTemplate' {..} =
    Lude.mconcat
      [ "version" Lude.=: version,
        "create-new-version" Lude.=: createNewVersion
      ]

-- | /See:/ 'mkUpdateSmsTemplateResponse' smart constructor.
data UpdateSmsTemplateResponse = UpdateSmsTemplateResponse'
  { messageBody :: MessageBody,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSmsTemplateResponse' with the minimum fields required to make a request.
--
-- * 'messageBody' -
-- * 'responseStatus' - The response status code.
mkUpdateSmsTemplateResponse ::
  -- | 'messageBody'
  MessageBody ->
  -- | 'responseStatus'
  Lude.Int ->
  UpdateSmsTemplateResponse
mkUpdateSmsTemplateResponse pMessageBody_ pResponseStatus_ =
  UpdateSmsTemplateResponse'
    { messageBody = pMessageBody_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'messageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ustrsMessageBody :: Lens.Lens' UpdateSmsTemplateResponse MessageBody
ustrsMessageBody = Lens.lens (messageBody :: UpdateSmsTemplateResponse -> MessageBody) (\s a -> s {messageBody = a} :: UpdateSmsTemplateResponse)
{-# DEPRECATED ustrsMessageBody "Use generic-lens or generic-optics with 'messageBody' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ustrsResponseStatus :: Lens.Lens' UpdateSmsTemplateResponse Lude.Int
ustrsResponseStatus = Lens.lens (responseStatus :: UpdateSmsTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateSmsTemplateResponse)
{-# DEPRECATED ustrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
