{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.PutApplicationPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the permission policy for an application. For the list of actions supported for this operation, see
--
--  <https://docs.aws.amazon.com/serverlessrepo/latest/devguide/access-control-resource-based.html#application-permissions Application
--  Permissions>
--  .
module Network.AWS.ServerlessApplicationRepository.PutApplicationPolicy
  ( -- * Creating a request
    PutApplicationPolicy (..),
    mkPutApplicationPolicy,

    -- ** Request lenses
    papApplicationId,
    papStatements,

    -- * Destructuring the response
    PutApplicationPolicyResponse (..),
    mkPutApplicationPolicyResponse,

    -- ** Response lenses
    paprrsStatements,
    paprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServerlessApplicationRepository.Types as Types

-- | /See:/ 'mkPutApplicationPolicy' smart constructor.
data PutApplicationPolicy = PutApplicationPolicy'
  { -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Core.Text,
    -- | An array of policy statements applied to the application.
    statements :: [Types.ApplicationPolicyStatement]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutApplicationPolicy' value with any optional fields omitted.
mkPutApplicationPolicy ::
  -- | 'applicationId'
  Core.Text ->
  PutApplicationPolicy
mkPutApplicationPolicy applicationId =
  PutApplicationPolicy' {applicationId, statements = Core.mempty}

-- | The Amazon Resource Name (ARN) of the application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papApplicationId :: Lens.Lens' PutApplicationPolicy Core.Text
papApplicationId = Lens.field @"applicationId"
{-# DEPRECATED papApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | An array of policy statements applied to the application.
--
-- /Note:/ Consider using 'statements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papStatements :: Lens.Lens' PutApplicationPolicy [Types.ApplicationPolicyStatement]
papStatements = Lens.field @"statements"
{-# DEPRECATED papStatements "Use generic-lens or generic-optics with 'statements' instead." #-}

instance Core.FromJSON PutApplicationPolicy where
  toJSON PutApplicationPolicy {..} =
    Core.object
      (Core.catMaybes [Core.Just ("statements" Core..= statements)])

instance Core.AWSRequest PutApplicationPolicy where
  type Rs PutApplicationPolicy = PutApplicationPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/applications/" Core.<> (Core.toText applicationId)
                Core.<> ("/policy")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutApplicationPolicyResponse'
            Core.<$> (x Core..:? "statements") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutApplicationPolicyResponse' smart constructor.
data PutApplicationPolicyResponse = PutApplicationPolicyResponse'
  { -- | An array of policy statements applied to the application.
    statements :: Core.Maybe [Types.ApplicationPolicyStatement],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutApplicationPolicyResponse' value with any optional fields omitted.
mkPutApplicationPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutApplicationPolicyResponse
mkPutApplicationPolicyResponse responseStatus =
  PutApplicationPolicyResponse'
    { statements = Core.Nothing,
      responseStatus
    }

-- | An array of policy statements applied to the application.
--
-- /Note:/ Consider using 'statements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paprrsStatements :: Lens.Lens' PutApplicationPolicyResponse (Core.Maybe [Types.ApplicationPolicyStatement])
paprrsStatements = Lens.field @"statements"
{-# DEPRECATED paprrsStatements "Use generic-lens or generic-optics with 'statements' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paprrsResponseStatus :: Lens.Lens' PutApplicationPolicyResponse Core.Int
paprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED paprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
