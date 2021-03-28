{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.ReadPreset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The ReadPreset operation gets detailed information about a preset.
module Network.AWS.ElasticTranscoder.ReadPreset
    (
    -- * Creating a request
      ReadPreset (..)
    , mkReadPreset
    -- ** Request lenses
    , rpId

    -- * Destructuring the response
    , ReadPresetResponse (..)
    , mkReadPresetResponse
    -- ** Response lenses
    , rprrsPreset
    , rprrsResponseStatus
    ) where

import qualified Network.AWS.ElasticTranscoder.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The @ReadPresetRequest@ structure.
--
-- /See:/ 'mkReadPreset' smart constructor.
newtype ReadPreset = ReadPreset'
  { id :: Types.Id
    -- ^ The identifier of the preset for which you want to get detailed information.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ReadPreset' value with any optional fields omitted.
mkReadPreset
    :: Types.Id -- ^ 'id'
    -> ReadPreset
mkReadPreset id = ReadPreset'{id}

-- | The identifier of the preset for which you want to get detailed information.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpId :: Lens.Lens' ReadPreset Types.Id
rpId = Lens.field @"id"
{-# INLINEABLE rpId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery ReadPreset where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ReadPreset where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ReadPreset where
        type Rs ReadPreset = ReadPresetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/2012-09-25/presets/" Core.<> Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ReadPresetResponse' Core.<$>
                   (x Core..:? "Preset") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The @ReadPresetResponse@ structure.
--
-- /See:/ 'mkReadPresetResponse' smart constructor.
data ReadPresetResponse = ReadPresetResponse'
  { preset :: Core.Maybe Types.Preset
    -- ^ A section of the response body that provides information about the preset.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReadPresetResponse' value with any optional fields omitted.
mkReadPresetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ReadPresetResponse
mkReadPresetResponse responseStatus
  = ReadPresetResponse'{preset = Core.Nothing, responseStatus}

-- | A section of the response body that provides information about the preset.
--
-- /Note:/ Consider using 'preset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprrsPreset :: Lens.Lens' ReadPresetResponse (Core.Maybe Types.Preset)
rprrsPreset = Lens.field @"preset"
{-# INLINEABLE rprrsPreset #-}
{-# DEPRECATED preset "Use generic-lens or generic-optics with 'preset' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprrsResponseStatus :: Lens.Lens' ReadPresetResponse Core.Int
rprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
