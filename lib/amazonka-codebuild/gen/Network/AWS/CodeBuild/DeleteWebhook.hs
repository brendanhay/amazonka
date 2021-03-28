{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.DeleteWebhook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For an existing AWS CodeBuild build project that has its source code stored in a GitHub or Bitbucket repository, stops AWS CodeBuild from rebuilding the source code every time a code change is pushed to the repository.
module Network.AWS.CodeBuild.DeleteWebhook
    (
    -- * Creating a request
      DeleteWebhook (..)
    , mkDeleteWebhook
    -- ** Request lenses
    , dwProjectName

    -- * Destructuring the response
    , DeleteWebhookResponse (..)
    , mkDeleteWebhookResponse
    -- ** Response lenses
    , dwrrsResponseStatus
    ) where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteWebhook' smart constructor.
newtype DeleteWebhook = DeleteWebhook'
  { projectName :: Types.ProjectName
    -- ^ The name of the AWS CodeBuild project.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteWebhook' value with any optional fields omitted.
mkDeleteWebhook
    :: Types.ProjectName -- ^ 'projectName'
    -> DeleteWebhook
mkDeleteWebhook projectName = DeleteWebhook'{projectName}

-- | The name of the AWS CodeBuild project.
--
-- /Note:/ Consider using 'projectName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwProjectName :: Lens.Lens' DeleteWebhook Types.ProjectName
dwProjectName = Lens.field @"projectName"
{-# INLINEABLE dwProjectName #-}
{-# DEPRECATED projectName "Use generic-lens or generic-optics with 'projectName' instead"  #-}

instance Core.ToQuery DeleteWebhook where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteWebhook where
        toHeaders DeleteWebhook{..}
          = Core.pure ("X-Amz-Target", "CodeBuild_20161006.DeleteWebhook")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteWebhook where
        toJSON DeleteWebhook{..}
          = Core.object
              (Core.catMaybes [Core.Just ("projectName" Core..= projectName)])

instance Core.AWSRequest DeleteWebhook where
        type Rs DeleteWebhook = DeleteWebhookResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteWebhookResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteWebhookResponse' smart constructor.
newtype DeleteWebhookResponse = DeleteWebhookResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteWebhookResponse' value with any optional fields omitted.
mkDeleteWebhookResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteWebhookResponse
mkDeleteWebhookResponse responseStatus
  = DeleteWebhookResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwrrsResponseStatus :: Lens.Lens' DeleteWebhookResponse Core.Int
dwrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dwrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
