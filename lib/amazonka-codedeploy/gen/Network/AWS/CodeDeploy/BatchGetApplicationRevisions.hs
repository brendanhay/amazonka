{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      BatchGetApplicationRevisions (..)
    , mkBatchGetApplicationRevisions
    -- ** Request lenses
    , bgarApplicationName
    , bgarRevisions

    -- * Destructuring the response
    , BatchGetApplicationRevisionsResponse (..)
    , mkBatchGetApplicationRevisionsResponse
    -- ** Response lenses
    , bgarrrsApplicationName
    , bgarrrsErrorMessage
    , bgarrrsRevisions
    , bgarrrsResponseStatus
    ) where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @BatchGetApplicationRevisions@ operation.
--
-- /See:/ 'mkBatchGetApplicationRevisions' smart constructor.
data BatchGetApplicationRevisions = BatchGetApplicationRevisions'
  { applicationName :: Types.ApplicationName
    -- ^ The name of an AWS CodeDeploy application about which to get revision information.
  , revisions :: [Types.RevisionLocation]
    -- ^ An array of @RevisionLocation@ objects that specify information to get about the application revisions, including type and location. The maximum number of @RevisionLocation@ objects you can specify is 25.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetApplicationRevisions' value with any optional fields omitted.
mkBatchGetApplicationRevisions
    :: Types.ApplicationName -- ^ 'applicationName'
    -> BatchGetApplicationRevisions
mkBatchGetApplicationRevisions applicationName
  = BatchGetApplicationRevisions'{applicationName,
                                  revisions = Core.mempty}

-- | The name of an AWS CodeDeploy application about which to get revision information.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarApplicationName :: Lens.Lens' BatchGetApplicationRevisions Types.ApplicationName
bgarApplicationName = Lens.field @"applicationName"
{-# INLINEABLE bgarApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | An array of @RevisionLocation@ objects that specify information to get about the application revisions, including type and location. The maximum number of @RevisionLocation@ objects you can specify is 25.
--
-- /Note:/ Consider using 'revisions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarRevisions :: Lens.Lens' BatchGetApplicationRevisions [Types.RevisionLocation]
bgarRevisions = Lens.field @"revisions"
{-# INLINEABLE bgarRevisions #-}
{-# DEPRECATED revisions "Use generic-lens or generic-optics with 'revisions' instead"  #-}

instance Core.ToQuery BatchGetApplicationRevisions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BatchGetApplicationRevisions where
        toHeaders BatchGetApplicationRevisions{..}
          = Core.pure
              ("X-Amz-Target",
               "CodeDeploy_20141006.BatchGetApplicationRevisions")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON BatchGetApplicationRevisions where
        toJSON BatchGetApplicationRevisions{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("applicationName" Core..= applicationName),
                  Core.Just ("revisions" Core..= revisions)])

instance Core.AWSRequest BatchGetApplicationRevisions where
        type Rs BatchGetApplicationRevisions =
             BatchGetApplicationRevisionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchGetApplicationRevisionsResponse' Core.<$>
                   (x Core..:? "applicationName") Core.<*> x Core..:? "errorMessage"
                     Core.<*> x Core..:? "revisions"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @BatchGetApplicationRevisions@ operation.
--
-- /See:/ 'mkBatchGetApplicationRevisionsResponse' smart constructor.
data BatchGetApplicationRevisionsResponse = BatchGetApplicationRevisionsResponse'
  { applicationName :: Core.Maybe Types.ApplicationName
    -- ^ The name of the application that corresponds to the revisions.
  , errorMessage :: Core.Maybe Types.ErrorMessage
    -- ^ Information about errors that might have occurred during the API call.
  , revisions :: Core.Maybe [Types.RevisionInfo]
    -- ^ Additional information about the revisions, including the type and location.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BatchGetApplicationRevisionsResponse' value with any optional fields omitted.
mkBatchGetApplicationRevisionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchGetApplicationRevisionsResponse
mkBatchGetApplicationRevisionsResponse responseStatus
  = BatchGetApplicationRevisionsResponse'{applicationName =
                                            Core.Nothing,
                                          errorMessage = Core.Nothing, revisions = Core.Nothing,
                                          responseStatus}

-- | The name of the application that corresponds to the revisions.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarrrsApplicationName :: Lens.Lens' BatchGetApplicationRevisionsResponse (Core.Maybe Types.ApplicationName)
bgarrrsApplicationName = Lens.field @"applicationName"
{-# INLINEABLE bgarrrsApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | Information about errors that might have occurred during the API call.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarrrsErrorMessage :: Lens.Lens' BatchGetApplicationRevisionsResponse (Core.Maybe Types.ErrorMessage)
bgarrrsErrorMessage = Lens.field @"errorMessage"
{-# INLINEABLE bgarrrsErrorMessage #-}
{-# DEPRECATED errorMessage "Use generic-lens or generic-optics with 'errorMessage' instead"  #-}

-- | Additional information about the revisions, including the type and location.
--
-- /Note:/ Consider using 'revisions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarrrsRevisions :: Lens.Lens' BatchGetApplicationRevisionsResponse (Core.Maybe [Types.RevisionInfo])
bgarrrsRevisions = Lens.field @"revisions"
{-# INLINEABLE bgarrrsRevisions #-}
{-# DEPRECATED revisions "Use generic-lens or generic-optics with 'revisions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarrrsResponseStatus :: Lens.Lens' BatchGetApplicationRevisionsResponse Core.Int
bgarrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bgarrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
