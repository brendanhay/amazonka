{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    ctrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to create an email template. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkCreateTemplate' smart constructor.
newtype CreateTemplate = CreateTemplate'
  { -- | The content of the email, composed of a subject line, an HTML part, and a text-only part.
    template :: Types.Template
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTemplate' value with any optional fields omitted.
mkCreateTemplate ::
  -- | 'template'
  Types.Template ->
  CreateTemplate
mkCreateTemplate template = CreateTemplate' {template}

-- | The content of the email, composed of a subject line, an HTML part, and a text-only part.
--
-- /Note:/ Consider using 'template' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTemplate :: Lens.Lens' CreateTemplate Types.Template
ctTemplate = Lens.field @"template"
{-# DEPRECATED ctTemplate "Use generic-lens or generic-optics with 'template' instead." #-}

instance Core.AWSRequest CreateTemplate where
  type Rs CreateTemplate = CreateTemplateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CreateTemplate")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "Template" template)
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateTemplateResult"
      ( \s h x ->
          CreateTemplateResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateTemplateResponse' smart constructor.
newtype CreateTemplateResponse = CreateTemplateResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTemplateResponse' value with any optional fields omitted.
mkCreateTemplateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateTemplateResponse
mkCreateTemplateResponse responseStatus =
  CreateTemplateResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsResponseStatus :: Lens.Lens' CreateTemplateResponse Core.Int
ctrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ctrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
