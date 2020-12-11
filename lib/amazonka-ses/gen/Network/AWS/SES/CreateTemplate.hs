{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.CreateTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an email template. Email templates enable you to send personalized email to one or more destinations in a single API operation. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Amazon SES Developer Guide> .
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.CreateTemplate
  ( -- * Creating a request
    CreateTemplate (..),
    mkCreateTemplate,

    -- ** Request lenses
    ctTemplate,

    -- * Destructuring the response
    CreateTemplateResponse (..),
    mkCreateTemplateResponse,

    -- ** Response lenses
    ctrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to create an email template. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkCreateTemplate' smart constructor.
newtype CreateTemplate = CreateTemplate' {template :: Template}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTemplate' with the minimum fields required to make a request.
--
-- * 'template' - The content of the email, composed of a subject line, an HTML part, and a text-only part.
mkCreateTemplate ::
  -- | 'template'
  Template ->
  CreateTemplate
mkCreateTemplate pTemplate_ =
  CreateTemplate' {template = pTemplate_}

-- | The content of the email, composed of a subject line, an HTML part, and a text-only part.
--
-- /Note:/ Consider using 'template' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTemplate :: Lens.Lens' CreateTemplate Template
ctTemplate = Lens.lens (template :: CreateTemplate -> Template) (\s a -> s {template = a} :: CreateTemplate)
{-# DEPRECATED ctTemplate "Use generic-lens or generic-optics with 'template' instead." #-}

instance Lude.AWSRequest CreateTemplate where
  type Rs CreateTemplate = CreateTemplateResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "CreateTemplateResult"
      ( \s h x ->
          CreateTemplateResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateTemplate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateTemplate where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTemplate where
  toQuery CreateTemplate' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateTemplate" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "Template" Lude.=: template
      ]

-- | /See:/ 'mkCreateTemplateResponse' smart constructor.
newtype CreateTemplateResponse = CreateTemplateResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTemplateResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTemplateResponse
mkCreateTemplateResponse pResponseStatus_ =
  CreateTemplateResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsResponseStatus :: Lens.Lens' CreateTemplateResponse Lude.Int
ctrsResponseStatus = Lens.lens (responseStatus :: CreateTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTemplateResponse)
{-# DEPRECATED ctrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
