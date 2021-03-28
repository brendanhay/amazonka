{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteCodeRepository
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Git repository from your account.
module Network.AWS.SageMaker.DeleteCodeRepository
    (
    -- * Creating a request
      DeleteCodeRepository (..)
    , mkDeleteCodeRepository
    -- ** Request lenses
    , dCodeRepositoryName

    -- * Destructuring the response
    , DeleteCodeRepositoryResponse (..)
    , mkDeleteCodeRepositoryResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDeleteCodeRepository' smart constructor.
newtype DeleteCodeRepository = DeleteCodeRepository'
  { codeRepositoryName :: Types.EntityName
    -- ^ The name of the Git repository to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCodeRepository' value with any optional fields omitted.
mkDeleteCodeRepository
    :: Types.EntityName -- ^ 'codeRepositoryName'
    -> DeleteCodeRepository
mkDeleteCodeRepository codeRepositoryName
  = DeleteCodeRepository'{codeRepositoryName}

-- | The name of the Git repository to delete.
--
-- /Note:/ Consider using 'codeRepositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCodeRepositoryName :: Lens.Lens' DeleteCodeRepository Types.EntityName
dCodeRepositoryName = Lens.field @"codeRepositoryName"
{-# INLINEABLE dCodeRepositoryName #-}
{-# DEPRECATED codeRepositoryName "Use generic-lens or generic-optics with 'codeRepositoryName' instead"  #-}

instance Core.ToQuery DeleteCodeRepository where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteCodeRepository where
        toHeaders DeleteCodeRepository{..}
          = Core.pure ("X-Amz-Target", "SageMaker.DeleteCodeRepository")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteCodeRepository where
        toJSON DeleteCodeRepository{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("CodeRepositoryName" Core..= codeRepositoryName)])

instance Core.AWSRequest DeleteCodeRepository where
        type Rs DeleteCodeRepository = DeleteCodeRepositoryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteCodeRepositoryResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteCodeRepositoryResponse' smart constructor.
data DeleteCodeRepositoryResponse = DeleteCodeRepositoryResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCodeRepositoryResponse' value with any optional fields omitted.
mkDeleteCodeRepositoryResponse
    :: DeleteCodeRepositoryResponse
mkDeleteCodeRepositoryResponse = DeleteCodeRepositoryResponse'
