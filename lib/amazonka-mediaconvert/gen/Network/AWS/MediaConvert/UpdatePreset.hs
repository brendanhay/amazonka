{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.UpdatePreset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify one of your existing presets.
module Network.AWS.MediaConvert.UpdatePreset
    (
    -- * Creating a request
      UpdatePreset (..)
    , mkUpdatePreset
    -- ** Request lenses
    , upName
    , upCategory
    , upDescription
    , upSettings

    -- * Destructuring the response
    , UpdatePresetResponse (..)
    , mkUpdatePresetResponse
    -- ** Response lenses
    , uprrsPreset
    , uprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdatePreset' smart constructor.
data UpdatePreset = UpdatePreset'
  { name :: Core.Text
    -- ^ The name of the preset you are modifying.
  , category :: Core.Maybe Core.Text
    -- ^ The new category for the preset, if you are changing it.
  , description :: Core.Maybe Core.Text
    -- ^ The new description for the preset, if you are changing it.
  , settings :: Core.Maybe Types.PresetSettings
    -- ^ Settings for preset
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdatePreset' value with any optional fields omitted.
mkUpdatePreset
    :: Core.Text -- ^ 'name'
    -> UpdatePreset
mkUpdatePreset name
  = UpdatePreset'{name, category = Core.Nothing,
                  description = Core.Nothing, settings = Core.Nothing}

-- | The name of the preset you are modifying.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upName :: Lens.Lens' UpdatePreset Core.Text
upName = Lens.field @"name"
{-# INLINEABLE upName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The new category for the preset, if you are changing it.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upCategory :: Lens.Lens' UpdatePreset (Core.Maybe Core.Text)
upCategory = Lens.field @"category"
{-# INLINEABLE upCategory #-}
{-# DEPRECATED category "Use generic-lens or generic-optics with 'category' instead"  #-}

-- | The new description for the preset, if you are changing it.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upDescription :: Lens.Lens' UpdatePreset (Core.Maybe Core.Text)
upDescription = Lens.field @"description"
{-# INLINEABLE upDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Settings for preset
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSettings :: Lens.Lens' UpdatePreset (Core.Maybe Types.PresetSettings)
upSettings = Lens.field @"settings"
{-# INLINEABLE upSettings #-}
{-# DEPRECATED settings "Use generic-lens or generic-optics with 'settings' instead"  #-}

instance Core.ToQuery UpdatePreset where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdatePreset where
        toHeaders UpdatePreset{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdatePreset where
        toJSON UpdatePreset{..}
          = Core.object
              (Core.catMaybes
                 [("category" Core..=) Core.<$> category,
                  ("description" Core..=) Core.<$> description,
                  ("settings" Core..=) Core.<$> settings])

instance Core.AWSRequest UpdatePreset where
        type Rs UpdatePreset = UpdatePresetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/2017-08-29/presets/" Core.<> Core.toText name,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdatePresetResponse' Core.<$>
                   (x Core..:? "preset") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdatePresetResponse' smart constructor.
data UpdatePresetResponse = UpdatePresetResponse'
  { preset :: Core.Maybe Types.Preset
    -- ^ A preset is a collection of preconfigured media conversion settings that you want MediaConvert to apply to the output during the conversion process.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdatePresetResponse' value with any optional fields omitted.
mkUpdatePresetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdatePresetResponse
mkUpdatePresetResponse responseStatus
  = UpdatePresetResponse'{preset = Core.Nothing, responseStatus}

-- | A preset is a collection of preconfigured media conversion settings that you want MediaConvert to apply to the output during the conversion process.
--
-- /Note:/ Consider using 'preset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsPreset :: Lens.Lens' UpdatePresetResponse (Core.Maybe Types.Preset)
uprrsPreset = Lens.field @"preset"
{-# INLINEABLE uprrsPreset #-}
{-# DEPRECATED preset "Use generic-lens or generic-optics with 'preset' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsResponseStatus :: Lens.Lens' UpdatePresetResponse Core.Int
uprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
