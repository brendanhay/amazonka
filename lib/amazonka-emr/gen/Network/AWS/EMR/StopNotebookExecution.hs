{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.StopNotebookExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a notebook execution.
module Network.AWS.EMR.StopNotebookExecution
    (
    -- * Creating a request
      StopNotebookExecution (..)
    , mkStopNotebookExecution
    -- ** Request lenses
    , sneNotebookExecutionId

    -- * Destructuring the response
    , StopNotebookExecutionResponse (..)
    , mkStopNotebookExecutionResponse
    ) where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopNotebookExecution' smart constructor.
newtype StopNotebookExecution = StopNotebookExecution'
  { notebookExecutionId :: Types.XmlStringMaxLen256
    -- ^ The unique identifier of the notebook execution.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopNotebookExecution' value with any optional fields omitted.
mkStopNotebookExecution
    :: Types.XmlStringMaxLen256 -- ^ 'notebookExecutionId'
    -> StopNotebookExecution
mkStopNotebookExecution notebookExecutionId
  = StopNotebookExecution'{notebookExecutionId}

-- | The unique identifier of the notebook execution.
--
-- /Note:/ Consider using 'notebookExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sneNotebookExecutionId :: Lens.Lens' StopNotebookExecution Types.XmlStringMaxLen256
sneNotebookExecutionId = Lens.field @"notebookExecutionId"
{-# INLINEABLE sneNotebookExecutionId #-}
{-# DEPRECATED notebookExecutionId "Use generic-lens or generic-optics with 'notebookExecutionId' instead"  #-}

instance Core.ToQuery StopNotebookExecution where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopNotebookExecution where
        toHeaders StopNotebookExecution{..}
          = Core.pure
              ("X-Amz-Target", "ElasticMapReduce.StopNotebookExecution")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopNotebookExecution where
        toJSON StopNotebookExecution{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("NotebookExecutionId" Core..= notebookExecutionId)])

instance Core.AWSRequest StopNotebookExecution where
        type Rs StopNotebookExecution = StopNotebookExecutionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull StopNotebookExecutionResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopNotebookExecutionResponse' smart constructor.
data StopNotebookExecutionResponse = StopNotebookExecutionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopNotebookExecutionResponse' value with any optional fields omitted.
mkStopNotebookExecutionResponse
    :: StopNotebookExecutionResponse
mkStopNotebookExecutionResponse = StopNotebookExecutionResponse'
