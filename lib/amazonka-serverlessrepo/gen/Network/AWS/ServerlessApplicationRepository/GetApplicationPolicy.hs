{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.GetApplicationPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the policy for the application.
module Network.AWS.ServerlessApplicationRepository.GetApplicationPolicy
  ( -- * Creating a request
    GetApplicationPolicy (..),
    mkGetApplicationPolicy,

    -- ** Request lenses
    gapApplicationId,

    -- * Destructuring the response
    GetApplicationPolicyResponse (..),
    mkGetApplicationPolicyResponse,

    -- ** Response lenses
    gaprrsStatements,
    gaprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServerlessApplicationRepository.Types as Types

-- | /See:/ 'mkGetApplicationPolicy' smart constructor.
newtype GetApplicationPolicy = GetApplicationPolicy'
  { -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetApplicationPolicy' value with any optional fields omitted.
mkGetApplicationPolicy ::
  -- | 'applicationId'
  Core.Text ->
  GetApplicationPolicy
mkGetApplicationPolicy applicationId =
  GetApplicationPolicy' {applicationId}

-- | The Amazon Resource Name (ARN) of the application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gapApplicationId :: Lens.Lens' GetApplicationPolicy Core.Text
gapApplicationId = Lens.field @"applicationId"
{-# DEPRECATED gapApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Core.AWSRequest GetApplicationPolicy where
  type Rs GetApplicationPolicy = GetApplicationPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/applications/" Core.<> (Core.toText applicationId)
                Core.<> ("/policy")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApplicationPolicyResponse'
            Core.<$> (x Core..:? "statements") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetApplicationPolicyResponse' smart constructor.
data GetApplicationPolicyResponse = GetApplicationPolicyResponse'
  { -- | An array of policy statements applied to the application.
    statements :: Core.Maybe [Types.ApplicationPolicyStatement],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetApplicationPolicyResponse' value with any optional fields omitted.
mkGetApplicationPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetApplicationPolicyResponse
mkGetApplicationPolicyResponse responseStatus =
  GetApplicationPolicyResponse'
    { statements = Core.Nothing,
      responseStatus
    }

-- | An array of policy statements applied to the application.
--
-- /Note:/ Consider using 'statements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaprrsStatements :: Lens.Lens' GetApplicationPolicyResponse (Core.Maybe [Types.ApplicationPolicyStatement])
gaprrsStatements = Lens.field @"statements"
{-# DEPRECATED gaprrsStatements "Use generic-lens or generic-optics with 'statements' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaprrsResponseStatus :: Lens.Lens' GetApplicationPolicyResponse Core.Int
gaprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gaprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
