{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.UpdatePullRequestDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the contents of the description of a pull request.
module Network.AWS.CodeCommit.UpdatePullRequestDescription
    (
    -- * Creating a request
      UpdatePullRequestDescription (..)
    , mkUpdatePullRequestDescription
    -- ** Request lenses
    , uprdPullRequestId
    , uprdDescription

    -- * Destructuring the response
    , UpdatePullRequestDescriptionResponse (..)
    , mkUpdatePullRequestDescriptionResponse
    -- ** Response lenses
    , uprdrrsPullRequest
    , uprdrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdatePullRequestDescription' smart constructor.
data UpdatePullRequestDescription = UpdatePullRequestDescription'
  { pullRequestId :: Types.PullRequestId
    -- ^ The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
  , description :: Types.Description
    -- ^ The updated content of the description for the pull request. This content replaces the existing description.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdatePullRequestDescription' value with any optional fields omitted.
mkUpdatePullRequestDescription
    :: Types.PullRequestId -- ^ 'pullRequestId'
    -> Types.Description -- ^ 'description'
    -> UpdatePullRequestDescription
mkUpdatePullRequestDescription pullRequestId description
  = UpdatePullRequestDescription'{pullRequestId, description}

-- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprdPullRequestId :: Lens.Lens' UpdatePullRequestDescription Types.PullRequestId
uprdPullRequestId = Lens.field @"pullRequestId"
{-# INLINEABLE uprdPullRequestId #-}
{-# DEPRECATED pullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead"  #-}

-- | The updated content of the description for the pull request. This content replaces the existing description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprdDescription :: Lens.Lens' UpdatePullRequestDescription Types.Description
uprdDescription = Lens.field @"description"
{-# INLINEABLE uprdDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.ToQuery UpdatePullRequestDescription where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdatePullRequestDescription where
        toHeaders UpdatePullRequestDescription{..}
          = Core.pure
              ("X-Amz-Target",
               "CodeCommit_20150413.UpdatePullRequestDescription")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdatePullRequestDescription where
        toJSON UpdatePullRequestDescription{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("pullRequestId" Core..= pullRequestId),
                  Core.Just ("description" Core..= description)])

instance Core.AWSRequest UpdatePullRequestDescription where
        type Rs UpdatePullRequestDescription =
             UpdatePullRequestDescriptionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdatePullRequestDescriptionResponse' Core.<$>
                   (x Core..: "pullRequest") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdatePullRequestDescriptionResponse' smart constructor.
data UpdatePullRequestDescriptionResponse = UpdatePullRequestDescriptionResponse'
  { pullRequest :: Types.PullRequest
    -- ^ Information about the updated pull request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdatePullRequestDescriptionResponse' value with any optional fields omitted.
mkUpdatePullRequestDescriptionResponse
    :: Types.PullRequest -- ^ 'pullRequest'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdatePullRequestDescriptionResponse
mkUpdatePullRequestDescriptionResponse pullRequest responseStatus
  = UpdatePullRequestDescriptionResponse'{pullRequest,
                                          responseStatus}

-- | Information about the updated pull request.
--
-- /Note:/ Consider using 'pullRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprdrrsPullRequest :: Lens.Lens' UpdatePullRequestDescriptionResponse Types.PullRequest
uprdrrsPullRequest = Lens.field @"pullRequest"
{-# INLINEABLE uprdrrsPullRequest #-}
{-# DEPRECATED pullRequest "Use generic-lens or generic-optics with 'pullRequest' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprdrrsResponseStatus :: Lens.Lens' UpdatePullRequestDescriptionResponse Core.Int
uprdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uprdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
