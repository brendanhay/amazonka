{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreatePreset (..),
    mkCreatePreset,

    -- ** Request lenses
    cpSettings,
    cpName,
    cpCategory,
    cpDescription,
    cpTags,

    -- * Destructuring the response
    CreatePresetResponse (..),
    mkCreatePresetResponse,

    -- ** Response lenses
    cprrsPreset,
    cprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreatePreset' smart constructor.
data CreatePreset = CreatePreset'
  { -- | Settings for preset
    settings :: Types.PresetSettings,
    -- | The name of the preset you are creating.
    name :: Core.Text,
    -- | Optional. A category for the preset you are creating.
    category :: Core.Maybe Core.Text,
    -- | Optional. A description of the preset you are creating.
    description :: Core.Maybe Core.Text,
    -- | The tags that you want to add to the resource. You can tag resources with a key-value pair or with only a key.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePreset' value with any optional fields omitted.
mkCreatePreset ::
  -- | 'settings'
  Types.PresetSettings ->
  -- | 'name'
  Core.Text ->
  CreatePreset
mkCreatePreset settings name =
  CreatePreset'
    { settings,
      name,
      category = Core.Nothing,
      description = Core.Nothing,
      tags = Core.Nothing
    }

-- | Settings for preset
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpSettings :: Lens.Lens' CreatePreset Types.PresetSettings
cpSettings = Lens.field @"settings"
{-# DEPRECATED cpSettings "Use generic-lens or generic-optics with 'settings' instead." #-}

-- | The name of the preset you are creating.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpName :: Lens.Lens' CreatePreset Core.Text
cpName = Lens.field @"name"
{-# DEPRECATED cpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Optional. A category for the preset you are creating.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpCategory :: Lens.Lens' CreatePreset (Core.Maybe Core.Text)
cpCategory = Lens.field @"category"
{-# DEPRECATED cpCategory "Use generic-lens or generic-optics with 'category' instead." #-}

-- | Optional. A description of the preset you are creating.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpDescription :: Lens.Lens' CreatePreset (Core.Maybe Core.Text)
cpDescription = Lens.field @"description"
{-# DEPRECATED cpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tags that you want to add to the resource. You can tag resources with a key-value pair or with only a key.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTags :: Lens.Lens' CreatePreset (Core.Maybe (Core.HashMap Core.Text Core.Text))
cpTags = Lens.field @"tags"
{-# DEPRECATED cpTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreatePreset where
  toJSON CreatePreset {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("settings" Core..= settings),
            Core.Just ("name" Core..= name),
            ("category" Core..=) Core.<$> category,
            ("description" Core..=) Core.<$> description,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreatePreset where
  type Rs CreatePreset = CreatePresetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/2017-08-29/presets",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePresetResponse'
            Core.<$> (x Core..:? "preset") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreatePresetResponse' smart constructor.
data CreatePresetResponse = CreatePresetResponse'
  { -- | A preset is a collection of preconfigured media conversion settings that you want MediaConvert to apply to the output during the conversion process.
    preset :: Core.Maybe Types.Preset,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreatePresetResponse' value with any optional fields omitted.
mkCreatePresetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreatePresetResponse
mkCreatePresetResponse responseStatus =
  CreatePresetResponse' {preset = Core.Nothing, responseStatus}

-- | A preset is a collection of preconfigured media conversion settings that you want MediaConvert to apply to the output during the conversion process.
--
-- /Note:/ Consider using 'preset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsPreset :: Lens.Lens' CreatePresetResponse (Core.Maybe Types.Preset)
cprrsPreset = Lens.field @"preset"
{-# DEPRECATED cprrsPreset "Use generic-lens or generic-optics with 'preset' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsResponseStatus :: Lens.Lens' CreatePresetResponse Core.Int
cprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
