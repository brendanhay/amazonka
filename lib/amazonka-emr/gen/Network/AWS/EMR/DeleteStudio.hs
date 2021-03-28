{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.DeleteStudio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an Amazon EMR Studio from the Studio metadata store.
module Network.AWS.EMR.DeleteStudio
    (
    -- * Creating a request
      DeleteStudio (..)
    , mkDeleteStudio
    -- ** Request lenses
    , dStudioId

    -- * Destructuring the response
    , DeleteStudioResponse (..)
    , mkDeleteStudioResponse
    ) where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteStudio' smart constructor.
newtype DeleteStudio = DeleteStudio'
  { studioId :: Types.StudioId
    -- ^ The ID of the Amazon EMR Studio.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteStudio' value with any optional fields omitted.
mkDeleteStudio
    :: Types.StudioId -- ^ 'studioId'
    -> DeleteStudio
mkDeleteStudio studioId = DeleteStudio'{studioId}

-- | The ID of the Amazon EMR Studio.
--
-- /Note:/ Consider using 'studioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStudioId :: Lens.Lens' DeleteStudio Types.StudioId
dStudioId = Lens.field @"studioId"
{-# INLINEABLE dStudioId #-}
{-# DEPRECATED studioId "Use generic-lens or generic-optics with 'studioId' instead"  #-}

instance Core.ToQuery DeleteStudio where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteStudio where
        toHeaders DeleteStudio{..}
          = Core.pure ("X-Amz-Target", "ElasticMapReduce.DeleteStudio")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteStudio where
        toJSON DeleteStudio{..}
          = Core.object
              (Core.catMaybes [Core.Just ("StudioId" Core..= studioId)])

instance Core.AWSRequest DeleteStudio where
        type Rs DeleteStudio = DeleteStudioResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteStudioResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteStudioResponse' smart constructor.
data DeleteStudioResponse = DeleteStudioResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteStudioResponse' value with any optional fields omitted.
mkDeleteStudioResponse
    :: DeleteStudioResponse
mkDeleteStudioResponse = DeleteStudioResponse'
