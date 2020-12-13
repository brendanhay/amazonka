{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.UpdateTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an email template. Email templates enable you to send personalized email to one or more destinations in a single API operation. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Amazon SES Developer Guide> .
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.UpdateTemplate
  ( -- * Creating a request
    UpdateTemplate (..),
    mkUpdateTemplate,

    -- ** Request lenses
    utTemplate,

    -- * Destructuring the response
    UpdateTemplateResponse (..),
    mkUpdateTemplateResponse,

    -- ** Response lenses
    utrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | /See:/ 'mkUpdateTemplate' smart constructor.
newtype UpdateTemplate = UpdateTemplate'
  { template :: Template
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTemplate' with the minimum fields required to make a request.
--
-- * 'template' -
mkUpdateTemplate ::
  -- | 'template'
  Template ->
  UpdateTemplate
mkUpdateTemplate pTemplate_ =
  UpdateTemplate' {template = pTemplate_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'template' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utTemplate :: Lens.Lens' UpdateTemplate Template
utTemplate = Lens.lens (template :: UpdateTemplate -> Template) (\s a -> s {template = a} :: UpdateTemplate)
{-# DEPRECATED utTemplate "Use generic-lens or generic-optics with 'template' instead." #-}

instance Lude.AWSRequest UpdateTemplate where
  type Rs UpdateTemplate = UpdateTemplateResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "UpdateTemplateResult"
      ( \s h x ->
          UpdateTemplateResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateTemplate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateTemplate where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateTemplate where
  toQuery UpdateTemplate' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("UpdateTemplate" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "Template" Lude.=: template
      ]

-- | /See:/ 'mkUpdateTemplateResponse' smart constructor.
newtype UpdateTemplateResponse = UpdateTemplateResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTemplateResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateTemplateResponse
mkUpdateTemplateResponse pResponseStatus_ =
  UpdateTemplateResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrsResponseStatus :: Lens.Lens' UpdateTemplateResponse Lude.Int
utrsResponseStatus = Lens.lens (responseStatus :: UpdateTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateTemplateResponse)
{-# DEPRECATED utrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
