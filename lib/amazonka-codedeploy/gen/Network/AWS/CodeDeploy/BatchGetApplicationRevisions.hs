{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.BatchGetApplicationRevisions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more application revisions. The maximum number of application revisions that can be returned is 25.
module Network.AWS.CodeDeploy.BatchGetApplicationRevisions
  ( -- * Creating a request
    BatchGetApplicationRevisions (..),
    mkBatchGetApplicationRevisions,

    -- ** Request lenses
    bgarApplicationName,
    bgarRevisions,

    -- * Destructuring the response
    BatchGetApplicationRevisionsResponse (..),
    mkBatchGetApplicationRevisionsResponse,

    -- ** Response lenses
    bgarrrsApplicationName,
    bgarrrsErrorMessage,
    bgarrrsRevisions,
    bgarrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @BatchGetApplicationRevisions@ operation.
--
-- /See:/ 'mkBatchGetApplicationRevisions' smart constructor.
data BatchGetApplicationRevisions = BatchGetApplicationRevisions'
  { -- | The name of an AWS CodeDeploy application about which to get revision information.
    applicationName :: Types.ApplicationName,
    -- | An array of @RevisionLocation@ objects that specify information to get about the application revisions, including type and location. The maximum number of @RevisionLocation@ objects you can specify is 25.
    revisions :: [Types.RevisionLocation]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetApplicationRevisions' value with any optional fields omitted.
mkBatchGetApplicationRevisions ::
  -- | 'applicationName'
  Types.ApplicationName ->
  BatchGetApplicationRevisions
mkBatchGetApplicationRevisions applicationName =
  BatchGetApplicationRevisions'
    { applicationName,
      revisions = Core.mempty
    }

-- | The name of an AWS CodeDeploy application about which to get revision information.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarApplicationName :: Lens.Lens' BatchGetApplicationRevisions Types.ApplicationName
bgarApplicationName = Lens.field @"applicationName"
{-# DEPRECATED bgarApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | An array of @RevisionLocation@ objects that specify information to get about the application revisions, including type and location. The maximum number of @RevisionLocation@ objects you can specify is 25.
--
-- /Note:/ Consider using 'revisions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarRevisions :: Lens.Lens' BatchGetApplicationRevisions [Types.RevisionLocation]
bgarRevisions = Lens.field @"revisions"
{-# DEPRECATED bgarRevisions "Use generic-lens or generic-optics with 'revisions' instead." #-}

instance Core.FromJSON BatchGetApplicationRevisions where
  toJSON BatchGetApplicationRevisions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("applicationName" Core..= applicationName),
            Core.Just ("revisions" Core..= revisions)
          ]
      )

instance Core.AWSRequest BatchGetApplicationRevisions where
  type
    Rs BatchGetApplicationRevisions =
      BatchGetApplicationRevisionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "CodeDeploy_20141006.BatchGetApplicationRevisions"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetApplicationRevisionsResponse'
            Core.<$> (x Core..:? "applicationName")
            Core.<*> (x Core..:? "errorMessage")
            Core.<*> (x Core..:? "revisions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @BatchGetApplicationRevisions@ operation.
--
-- /See:/ 'mkBatchGetApplicationRevisionsResponse' smart constructor.
data BatchGetApplicationRevisionsResponse = BatchGetApplicationRevisionsResponse'
  { -- | The name of the application that corresponds to the revisions.
    applicationName :: Core.Maybe Types.ApplicationName,
    -- | Information about errors that might have occurred during the API call.
    errorMessage :: Core.Maybe Types.ErrorMessage,
    -- | Additional information about the revisions, including the type and location.
    revisions :: Core.Maybe [Types.RevisionInfo],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BatchGetApplicationRevisionsResponse' value with any optional fields omitted.
mkBatchGetApplicationRevisionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BatchGetApplicationRevisionsResponse
mkBatchGetApplicationRevisionsResponse responseStatus =
  BatchGetApplicationRevisionsResponse'
    { applicationName =
        Core.Nothing,
      errorMessage = Core.Nothing,
      revisions = Core.Nothing,
      responseStatus
    }

-- | The name of the application that corresponds to the revisions.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarrrsApplicationName :: Lens.Lens' BatchGetApplicationRevisionsResponse (Core.Maybe Types.ApplicationName)
bgarrrsApplicationName = Lens.field @"applicationName"
{-# DEPRECATED bgarrrsApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | Information about errors that might have occurred during the API call.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarrrsErrorMessage :: Lens.Lens' BatchGetApplicationRevisionsResponse (Core.Maybe Types.ErrorMessage)
bgarrrsErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED bgarrrsErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | Additional information about the revisions, including the type and location.
--
-- /Note:/ Consider using 'revisions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarrrsRevisions :: Lens.Lens' BatchGetApplicationRevisionsResponse (Core.Maybe [Types.RevisionInfo])
bgarrrsRevisions = Lens.field @"revisions"
{-# DEPRECATED bgarrrsRevisions "Use generic-lens or generic-optics with 'revisions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarrrsResponseStatus :: Lens.Lens' BatchGetApplicationRevisionsResponse Core.Int
bgarrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED bgarrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
