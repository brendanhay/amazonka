{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetEmailTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the content and settings of a message template for messages that are sent through the email channel.
module Network.AWS.Pinpoint.GetEmailTemplate
  ( -- * Creating a request
    GetEmailTemplate (..),
    mkGetEmailTemplate,

    -- ** Request lenses
    getTemplateName,
    getVersion,

    -- * Destructuring the response
    GetEmailTemplateResponse (..),
    mkGetEmailTemplateResponse,

    -- ** Response lenses
    getrsEmailTemplateResponse,
    getrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetEmailTemplate' smart constructor.
data GetEmailTemplate = GetEmailTemplate'
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

-- | Creates a value of 'GetEmailTemplate' with the minimum fields required to make a request.
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
mkGetEmailTemplate ::
  -- | 'templateName'
  Lude.Text ->
  GetEmailTemplate
mkGetEmailTemplate pTemplateName_ =
  GetEmailTemplate'
    { templateName = pTemplateName_,
      version = Lude.Nothing
    }

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getTemplateName :: Lens.Lens' GetEmailTemplate Lude.Text
getTemplateName = Lens.lens (templateName :: GetEmailTemplate -> Lude.Text) (\s a -> s {templateName = a} :: GetEmailTemplate)
{-# DEPRECATED getTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

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
getVersion :: Lens.Lens' GetEmailTemplate (Lude.Maybe Lude.Text)
getVersion = Lens.lens (version :: GetEmailTemplate -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: GetEmailTemplate)
{-# DEPRECATED getVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.AWSRequest GetEmailTemplate where
  type Rs GetEmailTemplate = GetEmailTemplateResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetEmailTemplateResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetEmailTemplate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetEmailTemplate where
  toPath GetEmailTemplate' {..} =
    Lude.mconcat ["/v1/templates/", Lude.toBS templateName, "/email"]

instance Lude.ToQuery GetEmailTemplate where
  toQuery GetEmailTemplate' {..} =
    Lude.mconcat ["version" Lude.=: version]

-- | /See:/ 'mkGetEmailTemplateResponse' smart constructor.
data GetEmailTemplateResponse = GetEmailTemplateResponse'
  { emailTemplateResponse :: EmailTemplateResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetEmailTemplateResponse' with the minimum fields required to make a request.
--
-- * 'emailTemplateResponse' -
-- * 'responseStatus' - The response status code.
mkGetEmailTemplateResponse ::
  -- | 'emailTemplateResponse'
  EmailTemplateResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  GetEmailTemplateResponse
mkGetEmailTemplateResponse pEmailTemplateResponse_ pResponseStatus_ =
  GetEmailTemplateResponse'
    { emailTemplateResponse =
        pEmailTemplateResponse_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'emailTemplateResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getrsEmailTemplateResponse :: Lens.Lens' GetEmailTemplateResponse EmailTemplateResponse
getrsEmailTemplateResponse = Lens.lens (emailTemplateResponse :: GetEmailTemplateResponse -> EmailTemplateResponse) (\s a -> s {emailTemplateResponse = a} :: GetEmailTemplateResponse)
{-# DEPRECATED getrsEmailTemplateResponse "Use generic-lens or generic-optics with 'emailTemplateResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getrsResponseStatus :: Lens.Lens' GetEmailTemplateResponse Lude.Int
getrsResponseStatus = Lens.lens (responseStatus :: GetEmailTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetEmailTemplateResponse)
{-# DEPRECATED getrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
