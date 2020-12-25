{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DeleteReplicationTaskAssessmentRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the record of a single premigration assessment run.
--
-- This operation removes all metadata that AWS DMS maintains about this assessment run. However, the operation leaves untouched all information about this assessment run that is stored in your Amazon S3 bucket.
module Network.AWS.DMS.DeleteReplicationTaskAssessmentRun
  ( -- * Creating a request
    DeleteReplicationTaskAssessmentRun (..),
    mkDeleteReplicationTaskAssessmentRun,

    -- ** Request lenses
    drtarReplicationTaskAssessmentRunArn,

    -- * Destructuring the response
    DeleteReplicationTaskAssessmentRunResponse (..),
    mkDeleteReplicationTaskAssessmentRunResponse,

    -- ** Response lenses
    drtarrrsReplicationTaskAssessmentRun,
    drtarrrsResponseStatus,
  )
where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDeleteReplicationTaskAssessmentRun' smart constructor.
newtype DeleteReplicationTaskAssessmentRun = DeleteReplicationTaskAssessmentRun'
  { -- | Amazon Resource Name (ARN) of the premigration assessment run to be deleted.
    replicationTaskAssessmentRunArn :: Types.ReplicationTaskAssessmentRunArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteReplicationTaskAssessmentRun' value with any optional fields omitted.
mkDeleteReplicationTaskAssessmentRun ::
  -- | 'replicationTaskAssessmentRunArn'
  Types.ReplicationTaskAssessmentRunArn ->
  DeleteReplicationTaskAssessmentRun
mkDeleteReplicationTaskAssessmentRun
  replicationTaskAssessmentRunArn =
    DeleteReplicationTaskAssessmentRun' {replicationTaskAssessmentRunArn}

-- | Amazon Resource Name (ARN) of the premigration assessment run to be deleted.
--
-- /Note:/ Consider using 'replicationTaskAssessmentRunArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtarReplicationTaskAssessmentRunArn :: Lens.Lens' DeleteReplicationTaskAssessmentRun Types.ReplicationTaskAssessmentRunArn
drtarReplicationTaskAssessmentRunArn = Lens.field @"replicationTaskAssessmentRunArn"
{-# DEPRECATED drtarReplicationTaskAssessmentRunArn "Use generic-lens or generic-optics with 'replicationTaskAssessmentRunArn' instead." #-}

instance Core.FromJSON DeleteReplicationTaskAssessmentRun where
  toJSON DeleteReplicationTaskAssessmentRun {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "ReplicationTaskAssessmentRunArn"
                  Core..= replicationTaskAssessmentRunArn
              )
          ]
      )

instance Core.AWSRequest DeleteReplicationTaskAssessmentRun where
  type
    Rs DeleteReplicationTaskAssessmentRun =
      DeleteReplicationTaskAssessmentRunResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonDMSv20160101.DeleteReplicationTaskAssessmentRun"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteReplicationTaskAssessmentRunResponse'
            Core.<$> (x Core..:? "ReplicationTaskAssessmentRun")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- |
--
-- /See:/ 'mkDeleteReplicationTaskAssessmentRunResponse' smart constructor.
data DeleteReplicationTaskAssessmentRunResponse = DeleteReplicationTaskAssessmentRunResponse'
  { -- | The @ReplicationTaskAssessmentRun@ object for the deleted assessment run.
    replicationTaskAssessmentRun :: Core.Maybe Types.ReplicationTaskAssessmentRun,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteReplicationTaskAssessmentRunResponse' value with any optional fields omitted.
mkDeleteReplicationTaskAssessmentRunResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteReplicationTaskAssessmentRunResponse
mkDeleteReplicationTaskAssessmentRunResponse responseStatus =
  DeleteReplicationTaskAssessmentRunResponse'
    { replicationTaskAssessmentRun =
        Core.Nothing,
      responseStatus
    }

-- | The @ReplicationTaskAssessmentRun@ object for the deleted assessment run.
--
-- /Note:/ Consider using 'replicationTaskAssessmentRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtarrrsReplicationTaskAssessmentRun :: Lens.Lens' DeleteReplicationTaskAssessmentRunResponse (Core.Maybe Types.ReplicationTaskAssessmentRun)
drtarrrsReplicationTaskAssessmentRun = Lens.field @"replicationTaskAssessmentRun"
{-# DEPRECATED drtarrrsReplicationTaskAssessmentRun "Use generic-lens or generic-optics with 'replicationTaskAssessmentRun' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtarrrsResponseStatus :: Lens.Lens' DeleteReplicationTaskAssessmentRunResponse Core.Int
drtarrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drtarrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
