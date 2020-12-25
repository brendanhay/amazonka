{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ReadPreset (..),
    mkReadPreset,

    -- ** Request lenses
    rpId,

    -- * Destructuring the response
    ReadPresetResponse (..),
    mkReadPresetResponse,

    -- ** Response lenses
    rprrsPreset,
    rprrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticTranscoder.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The @ReadPresetRequest@ structure.
--
-- /See:/ 'mkReadPreset' smart constructor.
newtype ReadPreset = ReadPreset'
  { -- | The identifier of the preset for which you want to get detailed information.
    id :: Types.Id
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ReadPreset' value with any optional fields omitted.
mkReadPreset ::
  -- | 'id'
  Types.Id ->
  ReadPreset
mkReadPreset id = ReadPreset' {id}

-- | The identifier of the preset for which you want to get detailed information.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpId :: Lens.Lens' ReadPreset Types.Id
rpId = Lens.field @"id"
{-# DEPRECATED rpId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.AWSRequest ReadPreset where
  type Rs ReadPreset = ReadPresetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath ("/2012-09-25/presets/" Core.<> (Core.toText id)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ReadPresetResponse'
            Core.<$> (x Core..:? "Preset") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The @ReadPresetResponse@ structure.
--
-- /See:/ 'mkReadPresetResponse' smart constructor.
data ReadPresetResponse = ReadPresetResponse'
  { -- | A section of the response body that provides information about the preset.
    preset :: Core.Maybe Types.Preset,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReadPresetResponse' value with any optional fields omitted.
mkReadPresetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ReadPresetResponse
mkReadPresetResponse responseStatus =
  ReadPresetResponse' {preset = Core.Nothing, responseStatus}

-- | A section of the response body that provides information about the preset.
--
-- /Note:/ Consider using 'preset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprrsPreset :: Lens.Lens' ReadPresetResponse (Core.Maybe Types.Preset)
rprrsPreset = Lens.field @"preset"
{-# DEPRECATED rprrsPreset "Use generic-lens or generic-optics with 'preset' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprrsResponseStatus :: Lens.Lens' ReadPresetResponse Core.Int
rprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
