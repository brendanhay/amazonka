{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.DeletePreset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The DeletePreset operation removes a preset that you've added in an AWS region.
module Network.AWS.ElasticTranscoder.DeletePreset
    (
    -- * Creating a request
      DeletePreset (..)
    , mkDeletePreset
    -- ** Request lenses
    , dpId

    -- * Destructuring the response
    , DeletePresetResponse (..)
    , mkDeletePresetResponse
    -- ** Response lenses
    , dprrsResponseStatus
    ) where

import qualified Network.AWS.ElasticTranscoder.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The @DeletePresetRequest@ structure.
--
-- /See:/ 'mkDeletePreset' smart constructor.
newtype DeletePreset = DeletePreset'
  { id :: Types.Id
    -- ^ The identifier of the preset for which you want to get detailed information.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePreset' value with any optional fields omitted.
mkDeletePreset
    :: Types.Id -- ^ 'id'
    -> DeletePreset
mkDeletePreset id = DeletePreset'{id}

-- | The identifier of the preset for which you want to get detailed information.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpId :: Lens.Lens' DeletePreset Types.Id
dpId = Lens.field @"id"
{-# INLINEABLE dpId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery DeletePreset where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeletePreset where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeletePreset where
        type Rs DeletePreset = DeletePresetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/2012-09-25/presets/" Core.<> Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeletePresetResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | The @DeletePresetResponse@ structure.
--
-- /See:/ 'mkDeletePresetResponse' smart constructor.
newtype DeletePresetResponse = DeletePresetResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePresetResponse' value with any optional fields omitted.
mkDeletePresetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeletePresetResponse
mkDeletePresetResponse responseStatus
  = DeletePresetResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsResponseStatus :: Lens.Lens' DeletePresetResponse Core.Int
dprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
