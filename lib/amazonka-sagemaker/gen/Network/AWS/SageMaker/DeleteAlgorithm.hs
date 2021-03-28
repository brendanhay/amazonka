{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteAlgorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified algorithm from your account.
module Network.AWS.SageMaker.DeleteAlgorithm
    (
    -- * Creating a request
      DeleteAlgorithm (..)
    , mkDeleteAlgorithm
    -- ** Request lenses
    , daAlgorithmName

    -- * Destructuring the response
    , DeleteAlgorithmResponse (..)
    , mkDeleteAlgorithmResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDeleteAlgorithm' smart constructor.
newtype DeleteAlgorithm = DeleteAlgorithm'
  { algorithmName :: Types.EntityName
    -- ^ The name of the algorithm to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAlgorithm' value with any optional fields omitted.
mkDeleteAlgorithm
    :: Types.EntityName -- ^ 'algorithmName'
    -> DeleteAlgorithm
mkDeleteAlgorithm algorithmName = DeleteAlgorithm'{algorithmName}

-- | The name of the algorithm to delete.
--
-- /Note:/ Consider using 'algorithmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAlgorithmName :: Lens.Lens' DeleteAlgorithm Types.EntityName
daAlgorithmName = Lens.field @"algorithmName"
{-# INLINEABLE daAlgorithmName #-}
{-# DEPRECATED algorithmName "Use generic-lens or generic-optics with 'algorithmName' instead"  #-}

instance Core.ToQuery DeleteAlgorithm where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteAlgorithm where
        toHeaders DeleteAlgorithm{..}
          = Core.pure ("X-Amz-Target", "SageMaker.DeleteAlgorithm") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteAlgorithm where
        toJSON DeleteAlgorithm{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AlgorithmName" Core..= algorithmName)])

instance Core.AWSRequest DeleteAlgorithm where
        type Rs DeleteAlgorithm = DeleteAlgorithmResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteAlgorithmResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteAlgorithmResponse' smart constructor.
data DeleteAlgorithmResponse = DeleteAlgorithmResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAlgorithmResponse' value with any optional fields omitted.
mkDeleteAlgorithmResponse
    :: DeleteAlgorithmResponse
mkDeleteAlgorithmResponse = DeleteAlgorithmResponse'
