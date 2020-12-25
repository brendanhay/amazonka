{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.GetPreset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve the JSON for a specific preset.
module Network.AWS.MediaConvert.GetPreset
  ( -- * Creating a request
    GetPreset (..),
    mkGetPreset,

    -- ** Request lenses
    gpName,

    -- * Destructuring the response
    GetPresetResponse (..),
    mkGetPresetResponse,

    -- ** Response lenses
    gprrsPreset,
    gprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetPreset' smart constructor.
newtype GetPreset = GetPreset'
  { -- | The name of the preset.
    name :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetPreset' value with any optional fields omitted.
mkGetPreset ::
  -- | 'name'
  Core.Text ->
  GetPreset
mkGetPreset name = GetPreset' {name}

-- | The name of the preset.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpName :: Lens.Lens' GetPreset Core.Text
gpName = Lens.field @"name"
{-# DEPRECATED gpName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.AWSRequest GetPreset where
  type Rs GetPreset = GetPresetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath ("/2017-08-29/presets/" Core.<> (Core.toText name)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPresetResponse'
            Core.<$> (x Core..:? "preset") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetPresetResponse' smart constructor.
data GetPresetResponse = GetPresetResponse'
  { -- | A preset is a collection of preconfigured media conversion settings that you want MediaConvert to apply to the output during the conversion process.
    preset :: Core.Maybe Types.Preset,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetPresetResponse' value with any optional fields omitted.
mkGetPresetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetPresetResponse
mkGetPresetResponse responseStatus =
  GetPresetResponse' {preset = Core.Nothing, responseStatus}

-- | A preset is a collection of preconfigured media conversion settings that you want MediaConvert to apply to the output during the conversion process.
--
-- /Note:/ Consider using 'preset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsPreset :: Lens.Lens' GetPresetResponse (Core.Maybe Types.Preset)
gprrsPreset = Lens.field @"preset"
{-# DEPRECATED gprrsPreset "Use generic-lens or generic-optics with 'preset' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsResponseStatus :: Lens.Lens' GetPresetResponse Core.Int
gprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
