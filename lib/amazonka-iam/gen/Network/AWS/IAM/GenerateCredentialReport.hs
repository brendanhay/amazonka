{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GenerateCredentialReport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a credential report for the AWS account. For more information about the credential report, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/credential-reports.html Getting Credential Reports> in the /IAM User Guide/ .
module Network.AWS.IAM.GenerateCredentialReport
  ( -- * Creating a request
    GenerateCredentialReport (..),
    mkGenerateCredentialReport,

    -- * Destructuring the response
    GenerateCredentialReportResponse (..),
    mkGenerateCredentialReportResponse,

    -- ** Response lenses
    gcrrrsDescription,
    gcrrrsState,
    gcrrrsResponseStatus,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGenerateCredentialReport' smart constructor.
data GenerateCredentialReport = GenerateCredentialReport'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GenerateCredentialReport' value with any optional fields omitted.
mkGenerateCredentialReport ::
  GenerateCredentialReport
mkGenerateCredentialReport = GenerateCredentialReport'

instance Core.AWSRequest GenerateCredentialReport where
  type Rs GenerateCredentialReport = GenerateCredentialReportResponse
  request x@_ =
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
            ( Core.pure ("Action", "GenerateCredentialReport")
                Core.<> (Core.pure ("Version", "2010-05-08"))
            )
      }
  response =
    Response.receiveXMLWrapper
      "GenerateCredentialReportResult"
      ( \s h x ->
          GenerateCredentialReportResponse'
            Core.<$> (x Core..@? "Description")
            Core.<*> (x Core..@? "State")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a successful 'GenerateCredentialReport' request.
--
-- /See:/ 'mkGenerateCredentialReportResponse' smart constructor.
data GenerateCredentialReportResponse = GenerateCredentialReportResponse'
  { -- | Information about the credential report.
    description :: Core.Maybe Types.Description,
    -- | Information about the state of the credential report.
    state :: Core.Maybe Types.ReportStateType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GenerateCredentialReportResponse' value with any optional fields omitted.
mkGenerateCredentialReportResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GenerateCredentialReportResponse
mkGenerateCredentialReportResponse responseStatus =
  GenerateCredentialReportResponse'
    { description = Core.Nothing,
      state = Core.Nothing,
      responseStatus
    }

-- | Information about the credential report.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrrsDescription :: Lens.Lens' GenerateCredentialReportResponse (Core.Maybe Types.Description)
gcrrrsDescription = Lens.field @"description"
{-# DEPRECATED gcrrrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Information about the state of the credential report.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrrsState :: Lens.Lens' GenerateCredentialReportResponse (Core.Maybe Types.ReportStateType)
gcrrrsState = Lens.field @"state"
{-# DEPRECATED gcrrrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrrsResponseStatus :: Lens.Lens' GenerateCredentialReportResponse Core.Int
gcrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
