{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.CreatePreset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new preset. For information about job templates see the User Guide at http://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
module Network.AWS.MediaConvert.CreatePreset
    (
    -- * Creating a request
      CreatePreset (..)
    , mkCreatePreset
    -- ** Request lenses
    , cpSettings
    , cpName
    , cpCategory
    , cpDescription
    , cpTags

    -- * Destructuring the response
    , CreatePresetResponse (..)
    , mkCreatePresetResponse
    -- ** Response lenses
    , cprrsPreset
    , cprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreatePreset' smart constructor.
data CreatePreset = CreatePreset'
  { settings :: Types.PresetSettings
    -- ^ Settings for preset
  , name :: Core.Text
    -- ^ The name of the preset you are creating.
  , category :: Core.Maybe Core.Text
    -- ^ Optional. A category for the preset you are creating.
  , description :: Core.Maybe Core.Text
    -- ^ Optional. A description of the preset you are creating.
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The tags that you want to add to the resource. You can tag resources with a key-value pair or with only a key.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePreset' value with any optional fields omitted.
mkCreatePreset
    :: Types.PresetSettings -- ^ 'settings'
    -> Core.Text -- ^ 'name'
    -> CreatePreset
mkCreatePreset settings name
  = CreatePreset'{settings, name, category = Core.Nothing,
                  description = Core.Nothing, tags = Core.Nothing}

-- | Settings for preset
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpSettings :: Lens.Lens' CreatePreset Types.PresetSettings
cpSettings = Lens.field @"settings"
{-# INLINEABLE cpSettings #-}
{-# DEPRECATED settings "Use generic-lens or generic-optics with 'settings' instead"  #-}

-- | The name of the preset you are creating.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpName :: Lens.Lens' CreatePreset Core.Text
cpName = Lens.field @"name"
{-# INLINEABLE cpName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Optional. A category for the preset you are creating.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpCategory :: Lens.Lens' CreatePreset (Core.Maybe Core.Text)
cpCategory = Lens.field @"category"
{-# INLINEABLE cpCategory #-}
{-# DEPRECATED category "Use generic-lens or generic-optics with 'category' instead"  #-}

-- | Optional. A description of the preset you are creating.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpDescription :: Lens.Lens' CreatePreset (Core.Maybe Core.Text)
cpDescription = Lens.field @"description"
{-# INLINEABLE cpDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The tags that you want to add to the resource. You can tag resources with a key-value pair or with only a key.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTags :: Lens.Lens' CreatePreset (Core.Maybe (Core.HashMap Core.Text Core.Text))
cpTags = Lens.field @"tags"
{-# INLINEABLE cpTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreatePreset where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreatePreset where
        toHeaders CreatePreset{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreatePreset where
        toJSON CreatePreset{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("settings" Core..= settings),
                  Core.Just ("name" Core..= name),
                  ("category" Core..=) Core.<$> category,
                  ("description" Core..=) Core.<$> description,
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreatePreset where
        type Rs CreatePreset = CreatePresetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/2017-08-29/presets",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreatePresetResponse' Core.<$>
                   (x Core..:? "preset") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreatePresetResponse' smart constructor.
data CreatePresetResponse = CreatePresetResponse'
  { preset :: Core.Maybe Types.Preset
    -- ^ A preset is a collection of preconfigured media conversion settings that you want MediaConvert to apply to the output during the conversion process.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreatePresetResponse' value with any optional fields omitted.
mkCreatePresetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreatePresetResponse
mkCreatePresetResponse responseStatus
  = CreatePresetResponse'{preset = Core.Nothing, responseStatus}

-- | A preset is a collection of preconfigured media conversion settings that you want MediaConvert to apply to the output during the conversion process.
--
-- /Note:/ Consider using 'preset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsPreset :: Lens.Lens' CreatePresetResponse (Core.Maybe Types.Preset)
cprrsPreset = Lens.field @"preset"
{-# INLINEABLE cprrsPreset #-}
{-# DEPRECATED preset "Use generic-lens or generic-optics with 'preset' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsResponseStatus :: Lens.Lens' CreatePresetResponse Core.Int
cprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
