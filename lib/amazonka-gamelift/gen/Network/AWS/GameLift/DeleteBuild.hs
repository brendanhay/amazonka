{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DeleteBuild
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a build. This operation permanently deletes the build resource and any uploaded build files. Deleting a build does not affect the status of any active fleets using the build, but you can no longer create new fleets with the deleted build.
--
-- To delete a build, specify the build ID. 
-- __Learn more__ 
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-intro.html Upload a Custom Server Build> 
-- __Related operations__ 
--
--     * 'CreateBuild' 
--
--
--     * 'ListBuilds' 
--
--
--     * 'DescribeBuild' 
--
--
--     * 'UpdateBuild' 
--
--
--     * 'DeleteBuild' 
--
--
module Network.AWS.GameLift.DeleteBuild
    (
    -- * Creating a request
      DeleteBuild (..)
    , mkDeleteBuild
    -- ** Request lenses
    , dbBuildId

    -- * Destructuring the response
    , DeleteBuildResponse (..)
    , mkDeleteBuildResponse
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDeleteBuild' smart constructor.
newtype DeleteBuild = DeleteBuild'
  { buildId :: Types.BuildIdOrArn
    -- ^ A unique identifier for a build to delete. You can use either the build ID or ARN value. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBuild' value with any optional fields omitted.
mkDeleteBuild
    :: Types.BuildIdOrArn -- ^ 'buildId'
    -> DeleteBuild
mkDeleteBuild buildId = DeleteBuild'{buildId}

-- | A unique identifier for a build to delete. You can use either the build ID or ARN value. 
--
-- /Note:/ Consider using 'buildId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbBuildId :: Lens.Lens' DeleteBuild Types.BuildIdOrArn
dbBuildId = Lens.field @"buildId"
{-# INLINEABLE dbBuildId #-}
{-# DEPRECATED buildId "Use generic-lens or generic-optics with 'buildId' instead"  #-}

instance Core.ToQuery DeleteBuild where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteBuild where
        toHeaders DeleteBuild{..}
          = Core.pure ("X-Amz-Target", "GameLift.DeleteBuild") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteBuild where
        toJSON DeleteBuild{..}
          = Core.object
              (Core.catMaybes [Core.Just ("BuildId" Core..= buildId)])

instance Core.AWSRequest DeleteBuild where
        type Rs DeleteBuild = DeleteBuildResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteBuildResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteBuildResponse' smart constructor.
data DeleteBuildResponse = DeleteBuildResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBuildResponse' value with any optional fields omitted.
mkDeleteBuildResponse
    :: DeleteBuildResponse
mkDeleteBuildResponse = DeleteBuildResponse'
