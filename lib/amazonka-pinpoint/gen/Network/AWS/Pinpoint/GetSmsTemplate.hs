{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetSmsTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the content and settings of a message template for messages that are sent through the SMS channel.
module Network.AWS.Pinpoint.GetSmsTemplate
  ( -- * Creating a request
    GetSmsTemplate (..),
    mkGetSmsTemplate,

    -- ** Request lenses
    gstVersion,
    gstTemplateName,

    -- * Destructuring the response
    GetSmsTemplateResponse (..),
    mkGetSmsTemplateResponse,

    -- ** Response lenses
    gstrsResponseStatus,
    gstrsSMSTemplateResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSmsTemplate' smart constructor.
data GetSmsTemplate = GetSmsTemplate'
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

-- | Creates a value of 'GetSmsTemplate' with the minimum fields required to make a request.
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
mkGetSmsTemplate ::
  -- | 'templateName'
  Lude.Text ->
  GetSmsTemplate
mkGetSmsTemplate pTemplateName_ =
  GetSmsTemplate'
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
gstVersion :: Lens.Lens' GetSmsTemplate (Lude.Maybe Lude.Text)
gstVersion = Lens.lens (version :: GetSmsTemplate -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: GetSmsTemplate)
{-# DEPRECATED gstVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstTemplateName :: Lens.Lens' GetSmsTemplate Lude.Text
gstTemplateName = Lens.lens (templateName :: GetSmsTemplate -> Lude.Text) (\s a -> s {templateName = a} :: GetSmsTemplate)
{-# DEPRECATED gstTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

instance Lude.AWSRequest GetSmsTemplate where
  type Rs GetSmsTemplate = GetSmsTemplateResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSmsTemplateResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders GetSmsTemplate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetSmsTemplate where
  toPath GetSmsTemplate' {..} =
    Lude.mconcat ["/v1/templates/", Lude.toBS templateName, "/sms"]

instance Lude.ToQuery GetSmsTemplate where
  toQuery GetSmsTemplate' {..} =
    Lude.mconcat ["version" Lude.=: version]

-- | /See:/ 'mkGetSmsTemplateResponse' smart constructor.
data GetSmsTemplateResponse = GetSmsTemplateResponse'
  { responseStatus ::
      Lude.Int,
    sMSTemplateResponse :: SMSTemplateResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSmsTemplateResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'sMSTemplateResponse' - Undocumented field.
mkGetSmsTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'sMSTemplateResponse'
  SMSTemplateResponse ->
  GetSmsTemplateResponse
mkGetSmsTemplateResponse pResponseStatus_ pSMSTemplateResponse_ =
  GetSmsTemplateResponse'
    { responseStatus = pResponseStatus_,
      sMSTemplateResponse = pSMSTemplateResponse_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrsResponseStatus :: Lens.Lens' GetSmsTemplateResponse Lude.Int
gstrsResponseStatus = Lens.lens (responseStatus :: GetSmsTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSmsTemplateResponse)
{-# DEPRECATED gstrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'sMSTemplateResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrsSMSTemplateResponse :: Lens.Lens' GetSmsTemplateResponse SMSTemplateResponse
gstrsSMSTemplateResponse = Lens.lens (sMSTemplateResponse :: GetSmsTemplateResponse -> SMSTemplateResponse) (\s a -> s {sMSTemplateResponse = a} :: GetSmsTemplateResponse)
{-# DEPRECATED gstrsSMSTemplateResponse "Use generic-lens or generic-optics with 'sMSTemplateResponse' instead." #-}
