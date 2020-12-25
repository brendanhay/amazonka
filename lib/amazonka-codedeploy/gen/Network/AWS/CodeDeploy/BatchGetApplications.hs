{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.BatchGetApplications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more applications. The maximum number of applications that can be returned is 100.
module Network.AWS.CodeDeploy.BatchGetApplications
  ( -- * Creating a request
    BatchGetApplications (..),
    mkBatchGetApplications,

    -- ** Request lenses
    bgaApplicationNames,

    -- * Destructuring the response
    BatchGetApplicationsResponse (..),
    mkBatchGetApplicationsResponse,

    -- ** Response lenses
    bgarrsApplicationsInfo,
    bgarrsResponseStatus,
  )
where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @BatchGetApplications@ operation.
--
-- /See:/ 'mkBatchGetApplications' smart constructor.
newtype BatchGetApplications = BatchGetApplications'
  { -- | A list of application names separated by spaces. The maximum number of application names you can specify is 100.
    applicationNames :: [Types.ApplicationName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetApplications' value with any optional fields omitted.
mkBatchGetApplications ::
  BatchGetApplications
mkBatchGetApplications =
  BatchGetApplications' {applicationNames = Core.mempty}

-- | A list of application names separated by spaces. The maximum number of application names you can specify is 100.
--
-- /Note:/ Consider using 'applicationNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgaApplicationNames :: Lens.Lens' BatchGetApplications [Types.ApplicationName]
bgaApplicationNames = Lens.field @"applicationNames"
{-# DEPRECATED bgaApplicationNames "Use generic-lens or generic-optics with 'applicationNames' instead." #-}

instance Core.FromJSON BatchGetApplications where
  toJSON BatchGetApplications {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("applicationNames" Core..= applicationNames)]
      )

instance Core.AWSRequest BatchGetApplications where
  type Rs BatchGetApplications = BatchGetApplicationsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeDeploy_20141006.BatchGetApplications")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetApplicationsResponse'
            Core.<$> (x Core..:? "applicationsInfo")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @BatchGetApplications@ operation.
--
-- /See:/ 'mkBatchGetApplicationsResponse' smart constructor.
data BatchGetApplicationsResponse = BatchGetApplicationsResponse'
  { -- | Information about the applications.
    applicationsInfo :: Core.Maybe [Types.ApplicationInfo],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BatchGetApplicationsResponse' value with any optional fields omitted.
mkBatchGetApplicationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BatchGetApplicationsResponse
mkBatchGetApplicationsResponse responseStatus =
  BatchGetApplicationsResponse'
    { applicationsInfo = Core.Nothing,
      responseStatus
    }

-- | Information about the applications.
--
-- /Note:/ Consider using 'applicationsInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarrsApplicationsInfo :: Lens.Lens' BatchGetApplicationsResponse (Core.Maybe [Types.ApplicationInfo])
bgarrsApplicationsInfo = Lens.field @"applicationsInfo"
{-# DEPRECATED bgarrsApplicationsInfo "Use generic-lens or generic-optics with 'applicationsInfo' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarrsResponseStatus :: Lens.Lens' BatchGetApplicationsResponse Core.Int
bgarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED bgarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
