{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.TestRepositoryTriggers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests the functionality of repository triggers by sending information to the trigger target. If real data is available in the repository, the test sends data from the last commit. If no data is available, sample data is generated.
module Network.AWS.CodeCommit.TestRepositoryTriggers
    (
    -- * Creating a request
      TestRepositoryTriggers (..)
    , mkTestRepositoryTriggers
    -- ** Request lenses
    , trtRepositoryName
    , trtTriggers

    -- * Destructuring the response
    , TestRepositoryTriggersResponse (..)
    , mkTestRepositoryTriggersResponse
    -- ** Response lenses
    , trtrrsFailedExecutions
    , trtrrsSuccessfulExecutions
    , trtrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a test repository triggers operation.
--
-- /See:/ 'mkTestRepositoryTriggers' smart constructor.
data TestRepositoryTriggers = TestRepositoryTriggers'
  { repositoryName :: Types.RepositoryName
    -- ^ The name of the repository in which to test the triggers.
  , triggers :: [Types.RepositoryTrigger]
    -- ^ The list of triggers to test.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TestRepositoryTriggers' value with any optional fields omitted.
mkTestRepositoryTriggers
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> TestRepositoryTriggers
mkTestRepositoryTriggers repositoryName
  = TestRepositoryTriggers'{repositoryName, triggers = Core.mempty}

-- | The name of the repository in which to test the triggers.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trtRepositoryName :: Lens.Lens' TestRepositoryTriggers Types.RepositoryName
trtRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE trtRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The list of triggers to test.
--
-- /Note:/ Consider using 'triggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trtTriggers :: Lens.Lens' TestRepositoryTriggers [Types.RepositoryTrigger]
trtTriggers = Lens.field @"triggers"
{-# INLINEABLE trtTriggers #-}
{-# DEPRECATED triggers "Use generic-lens or generic-optics with 'triggers' instead"  #-}

instance Core.ToQuery TestRepositoryTriggers where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders TestRepositoryTriggers where
        toHeaders TestRepositoryTriggers{..}
          = Core.pure
              ("X-Amz-Target", "CodeCommit_20150413.TestRepositoryTriggers")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON TestRepositoryTriggers where
        toJSON TestRepositoryTriggers{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("repositoryName" Core..= repositoryName),
                  Core.Just ("triggers" Core..= triggers)])

instance Core.AWSRequest TestRepositoryTriggers where
        type Rs TestRepositoryTriggers = TestRepositoryTriggersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 TestRepositoryTriggersResponse' Core.<$>
                   (x Core..:? "failedExecutions") Core.<*>
                     x Core..:? "successfulExecutions"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a test repository triggers operation.
--
-- /See:/ 'mkTestRepositoryTriggersResponse' smart constructor.
data TestRepositoryTriggersResponse = TestRepositoryTriggersResponse'
  { failedExecutions :: Core.Maybe [Types.RepositoryTriggerExecutionFailure]
    -- ^ The list of triggers that were not tested. This list provides the names of the triggers that could not be tested, separated by commas.
  , successfulExecutions :: Core.Maybe [Types.RepositoryTriggerName]
    -- ^ The list of triggers that were successfully tested. This list provides the names of the triggers that were successfully tested, separated by commas.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TestRepositoryTriggersResponse' value with any optional fields omitted.
mkTestRepositoryTriggersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> TestRepositoryTriggersResponse
mkTestRepositoryTriggersResponse responseStatus
  = TestRepositoryTriggersResponse'{failedExecutions = Core.Nothing,
                                    successfulExecutions = Core.Nothing, responseStatus}

-- | The list of triggers that were not tested. This list provides the names of the triggers that could not be tested, separated by commas.
--
-- /Note:/ Consider using 'failedExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trtrrsFailedExecutions :: Lens.Lens' TestRepositoryTriggersResponse (Core.Maybe [Types.RepositoryTriggerExecutionFailure])
trtrrsFailedExecutions = Lens.field @"failedExecutions"
{-# INLINEABLE trtrrsFailedExecutions #-}
{-# DEPRECATED failedExecutions "Use generic-lens or generic-optics with 'failedExecutions' instead"  #-}

-- | The list of triggers that were successfully tested. This list provides the names of the triggers that were successfully tested, separated by commas.
--
-- /Note:/ Consider using 'successfulExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trtrrsSuccessfulExecutions :: Lens.Lens' TestRepositoryTriggersResponse (Core.Maybe [Types.RepositoryTriggerName])
trtrrsSuccessfulExecutions = Lens.field @"successfulExecutions"
{-# INLINEABLE trtrrsSuccessfulExecutions #-}
{-# DEPRECATED successfulExecutions "Use generic-lens or generic-optics with 'successfulExecutions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trtrrsResponseStatus :: Lens.Lens' TestRepositoryTriggersResponse Core.Int
trtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE trtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
