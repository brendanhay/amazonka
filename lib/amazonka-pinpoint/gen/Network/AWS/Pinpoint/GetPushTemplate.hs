{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetPushTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the content and settings of a message template for messages that are sent through a push notification channel.
module Network.AWS.Pinpoint.GetPushTemplate
  ( -- * Creating a request
    GetPushTemplate (..),
    mkGetPushTemplate,

    -- ** Request lenses
    gptVersion,
    gptTemplateName,

    -- * Destructuring the response
    GetPushTemplateResponse (..),
    mkGetPushTemplateResponse,

    -- ** Response lenses
    gptrsResponseStatus,
    gptrsPushNotificationTemplateResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetPushTemplate' smart constructor.
data GetPushTemplate = GetPushTemplate'
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

-- | Creates a value of 'GetPushTemplate' with the minimum fields required to make a request.
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
mkGetPushTemplate ::
  -- | 'templateName'
  Lude.Text ->
  GetPushTemplate
mkGetPushTemplate pTemplateName_ =
  GetPushTemplate'
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
gptVersion :: Lens.Lens' GetPushTemplate (Lude.Maybe Lude.Text)
gptVersion = Lens.lens (version :: GetPushTemplate -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: GetPushTemplate)
{-# DEPRECATED gptVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gptTemplateName :: Lens.Lens' GetPushTemplate Lude.Text
gptTemplateName = Lens.lens (templateName :: GetPushTemplate -> Lude.Text) (\s a -> s {templateName = a} :: GetPushTemplate)
{-# DEPRECATED gptTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

instance Lude.AWSRequest GetPushTemplate where
  type Rs GetPushTemplate = GetPushTemplateResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetPushTemplateResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders GetPushTemplate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetPushTemplate where
  toPath GetPushTemplate' {..} =
    Lude.mconcat ["/v1/templates/", Lude.toBS templateName, "/push"]

instance Lude.ToQuery GetPushTemplate where
  toQuery GetPushTemplate' {..} =
    Lude.mconcat ["version" Lude.=: version]

-- | /See:/ 'mkGetPushTemplateResponse' smart constructor.
data GetPushTemplateResponse = GetPushTemplateResponse'
  { responseStatus ::
      Lude.Int,
    pushNotificationTemplateResponse ::
      PushNotificationTemplateResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPushTemplateResponse' with the minimum fields required to make a request.
--
-- * 'pushNotificationTemplateResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkGetPushTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'pushNotificationTemplateResponse'
  PushNotificationTemplateResponse ->
  GetPushTemplateResponse
mkGetPushTemplateResponse
  pResponseStatus_
  pPushNotificationTemplateResponse_ =
    GetPushTemplateResponse'
      { responseStatus = pResponseStatus_,
        pushNotificationTemplateResponse =
          pPushNotificationTemplateResponse_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gptrsResponseStatus :: Lens.Lens' GetPushTemplateResponse Lude.Int
gptrsResponseStatus = Lens.lens (responseStatus :: GetPushTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetPushTemplateResponse)
{-# DEPRECATED gptrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'pushNotificationTemplateResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gptrsPushNotificationTemplateResponse :: Lens.Lens' GetPushTemplateResponse PushNotificationTemplateResponse
gptrsPushNotificationTemplateResponse = Lens.lens (pushNotificationTemplateResponse :: GetPushTemplateResponse -> PushNotificationTemplateResponse) (\s a -> s {pushNotificationTemplateResponse = a} :: GetPushTemplateResponse)
{-# DEPRECATED gptrsPushNotificationTemplateResponse "Use generic-lens or generic-optics with 'pushNotificationTemplateResponse' instead." #-}
