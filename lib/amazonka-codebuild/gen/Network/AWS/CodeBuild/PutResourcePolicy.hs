{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.PutResourcePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stores a resource policy for the ARN of a @Project@ or @ReportGroup@ object. 
module Network.AWS.CodeBuild.PutResourcePolicy
    (
    -- * Creating a request
      PutResourcePolicy (..)
    , mkPutResourcePolicy
    -- ** Request lenses
    , prpPolicy
    , prpResourceArn

    -- * Destructuring the response
    , PutResourcePolicyResponse (..)
    , mkPutResourcePolicyResponse
    -- ** Response lenses
    , prprrsResourceArn
    , prprrsResponseStatus
    ) where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutResourcePolicy' smart constructor.
data PutResourcePolicy = PutResourcePolicy'
  { policy :: Types.NonEmptyString
    -- ^ A JSON-formatted resource policy. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/project-sharing.html#project-sharing-share Sharing a Project> and <https://docs.aws.amazon.com/codebuild/latest/userguide/report-groups-sharing.html#report-groups-sharing-share Sharing a Report Group> in the /AWS CodeBuild User Guide/ . 
  , resourceArn :: Types.NonEmptyString
    -- ^ The ARN of the @Project@ or @ReportGroup@ resource you want to associate with a resource policy. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutResourcePolicy' value with any optional fields omitted.
mkPutResourcePolicy
    :: Types.NonEmptyString -- ^ 'policy'
    -> Types.NonEmptyString -- ^ 'resourceArn'
    -> PutResourcePolicy
mkPutResourcePolicy policy resourceArn
  = PutResourcePolicy'{policy, resourceArn}

-- | A JSON-formatted resource policy. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/project-sharing.html#project-sharing-share Sharing a Project> and <https://docs.aws.amazon.com/codebuild/latest/userguide/report-groups-sharing.html#report-groups-sharing-share Sharing a Report Group> in the /AWS CodeBuild User Guide/ . 
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpPolicy :: Lens.Lens' PutResourcePolicy Types.NonEmptyString
prpPolicy = Lens.field @"policy"
{-# INLINEABLE prpPolicy #-}
{-# DEPRECATED policy "Use generic-lens or generic-optics with 'policy' instead"  #-}

-- | The ARN of the @Project@ or @ReportGroup@ resource you want to associate with a resource policy. 
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpResourceArn :: Lens.Lens' PutResourcePolicy Types.NonEmptyString
prpResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE prpResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

instance Core.ToQuery PutResourcePolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutResourcePolicy where
        toHeaders PutResourcePolicy{..}
          = Core.pure
              ("X-Amz-Target", "CodeBuild_20161006.PutResourcePolicy")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutResourcePolicy where
        toJSON PutResourcePolicy{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("policy" Core..= policy),
                  Core.Just ("resourceArn" Core..= resourceArn)])

instance Core.AWSRequest PutResourcePolicy where
        type Rs PutResourcePolicy = PutResourcePolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutResourcePolicyResponse' Core.<$>
                   (x Core..:? "resourceArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutResourcePolicyResponse' smart constructor.
data PutResourcePolicyResponse = PutResourcePolicyResponse'
  { resourceArn :: Core.Maybe Types.ResourceArn
    -- ^ The ARN of the @Project@ or @ReportGroup@ resource that is associated with a resource policy. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutResourcePolicyResponse' value with any optional fields omitted.
mkPutResourcePolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutResourcePolicyResponse
mkPutResourcePolicyResponse responseStatus
  = PutResourcePolicyResponse'{resourceArn = Core.Nothing,
                               responseStatus}

-- | The ARN of the @Project@ or @ReportGroup@ resource that is associated with a resource policy. 
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prprrsResourceArn :: Lens.Lens' PutResourcePolicyResponse (Core.Maybe Types.ResourceArn)
prprrsResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE prprrsResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prprrsResponseStatus :: Lens.Lens' PutResourcePolicyResponse Core.Int
prprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE prprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
