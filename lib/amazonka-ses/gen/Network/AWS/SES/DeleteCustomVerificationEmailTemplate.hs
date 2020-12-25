{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DeleteCustomVerificationEmailTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing custom verification email template.
--
-- For more information about custom verification email templates, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html Using Custom Verification Email Templates> in the /Amazon SES Developer Guide/ .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.DeleteCustomVerificationEmailTemplate
  ( -- * Creating a request
    DeleteCustomVerificationEmailTemplate (..),
    mkDeleteCustomVerificationEmailTemplate,

    -- ** Request lenses
    dcvetTemplateName,

    -- * Destructuring the response
    DeleteCustomVerificationEmailTemplateResponse (..),
    mkDeleteCustomVerificationEmailTemplateResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to delete an existing custom verification email template.
--
-- /See:/ 'mkDeleteCustomVerificationEmailTemplate' smart constructor.
newtype DeleteCustomVerificationEmailTemplate = DeleteCustomVerificationEmailTemplate'
  { -- | The name of the custom verification email template that you want to delete.
    templateName :: Types.TemplateName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCustomVerificationEmailTemplate' value with any optional fields omitted.
mkDeleteCustomVerificationEmailTemplate ::
  -- | 'templateName'
  Types.TemplateName ->
  DeleteCustomVerificationEmailTemplate
mkDeleteCustomVerificationEmailTemplate templateName =
  DeleteCustomVerificationEmailTemplate' {templateName}

-- | The name of the custom verification email template that you want to delete.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvetTemplateName :: Lens.Lens' DeleteCustomVerificationEmailTemplate Types.TemplateName
dcvetTemplateName = Lens.field @"templateName"
{-# DEPRECATED dcvetTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

instance Core.AWSRequest DeleteCustomVerificationEmailTemplate where
  type
    Rs DeleteCustomVerificationEmailTemplate =
      DeleteCustomVerificationEmailTemplateResponse
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
            ( Core.pure ("Action", "DeleteCustomVerificationEmailTemplate")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "TemplateName" templateName)
            )
      }
  response =
    Response.receiveNull
      DeleteCustomVerificationEmailTemplateResponse'

-- | /See:/ 'mkDeleteCustomVerificationEmailTemplateResponse' smart constructor.
data DeleteCustomVerificationEmailTemplateResponse = DeleteCustomVerificationEmailTemplateResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCustomVerificationEmailTemplateResponse' value with any optional fields omitted.
mkDeleteCustomVerificationEmailTemplateResponse ::
  DeleteCustomVerificationEmailTemplateResponse
mkDeleteCustomVerificationEmailTemplateResponse =
  DeleteCustomVerificationEmailTemplateResponse'
