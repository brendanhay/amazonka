{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.SetVisibleToAllUsers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the 'Cluster$VisibleToAllUsers' value, which determines whether the cluster is visible to all IAM users of the AWS account associated with the cluster. Only the IAM user who created the cluster or the AWS account root user can call this action. The default value, @true@ , indicates that all IAM users in the AWS account can perform cluster actions if they have the proper IAM policy permissions. If set to @false@ , only the IAM user that created the cluster can perform actions. This action works on running clusters. You can override the default @true@ setting when you create a cluster by using the @VisibleToAllUsers@ parameter with @RunJobFlow@ .
module Network.AWS.EMR.SetVisibleToAllUsers
    (
    -- * Creating a request
      SetVisibleToAllUsers (..)
    , mkSetVisibleToAllUsers
    -- ** Request lenses
    , svtauJobFlowIds
    , svtauVisibleToAllUsers

    -- * Destructuring the response
    , SetVisibleToAllUsersResponse (..)
    , mkSetVisibleToAllUsersResponse
    ) where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input to the SetVisibleToAllUsers action.
--
-- /See:/ 'mkSetVisibleToAllUsers' smart constructor.
data SetVisibleToAllUsers = SetVisibleToAllUsers'
  { jobFlowIds :: [Types.XmlString]
    -- ^ The unique identifier of the job flow (cluster).
  , visibleToAllUsers :: Core.Bool
    -- ^ A value of @true@ indicates that all IAM users in the AWS account can perform cluster actions if they have the proper IAM policy permissions. This is the default. A value of @false@ indicates that only the IAM user who created the cluster can perform actions.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetVisibleToAllUsers' value with any optional fields omitted.
mkSetVisibleToAllUsers
    :: Core.Bool -- ^ 'visibleToAllUsers'
    -> SetVisibleToAllUsers
mkSetVisibleToAllUsers visibleToAllUsers
  = SetVisibleToAllUsers'{jobFlowIds = Core.mempty,
                          visibleToAllUsers}

-- | The unique identifier of the job flow (cluster).
--
-- /Note:/ Consider using 'jobFlowIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svtauJobFlowIds :: Lens.Lens' SetVisibleToAllUsers [Types.XmlString]
svtauJobFlowIds = Lens.field @"jobFlowIds"
{-# INLINEABLE svtauJobFlowIds #-}
{-# DEPRECATED jobFlowIds "Use generic-lens or generic-optics with 'jobFlowIds' instead"  #-}

-- | A value of @true@ indicates that all IAM users in the AWS account can perform cluster actions if they have the proper IAM policy permissions. This is the default. A value of @false@ indicates that only the IAM user who created the cluster can perform actions.
--
-- /Note:/ Consider using 'visibleToAllUsers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svtauVisibleToAllUsers :: Lens.Lens' SetVisibleToAllUsers Core.Bool
svtauVisibleToAllUsers = Lens.field @"visibleToAllUsers"
{-# INLINEABLE svtauVisibleToAllUsers #-}
{-# DEPRECATED visibleToAllUsers "Use generic-lens or generic-optics with 'visibleToAllUsers' instead"  #-}

instance Core.ToQuery SetVisibleToAllUsers where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SetVisibleToAllUsers where
        toHeaders SetVisibleToAllUsers{..}
          = Core.pure
              ("X-Amz-Target", "ElasticMapReduce.SetVisibleToAllUsers")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SetVisibleToAllUsers where
        toJSON SetVisibleToAllUsers{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("JobFlowIds" Core..= jobFlowIds),
                  Core.Just ("VisibleToAllUsers" Core..= visibleToAllUsers)])

instance Core.AWSRequest SetVisibleToAllUsers where
        type Rs SetVisibleToAllUsers = SetVisibleToAllUsersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull SetVisibleToAllUsersResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSetVisibleToAllUsersResponse' smart constructor.
data SetVisibleToAllUsersResponse = SetVisibleToAllUsersResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetVisibleToAllUsersResponse' value with any optional fields omitted.
mkSetVisibleToAllUsersResponse
    :: SetVisibleToAllUsersResponse
mkSetVisibleToAllUsersResponse = SetVisibleToAllUsersResponse'
