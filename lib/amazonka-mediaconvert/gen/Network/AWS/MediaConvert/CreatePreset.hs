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
    cpCategory,
    cpName,
    cpDescription,
    cpTags,

    -- * Destructuring the response
    CreatePresetResponse (..),
    mkCreatePresetResponse,

    -- ** Response lenses
    cprsPreset,
    cprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreatePreset' smart constructor.
data CreatePreset = CreatePreset'
  { -- | Settings for preset
    settings :: PresetSettings,
    -- | Optional. A category for the preset you are creating.
    category :: Lude.Maybe Lude.Text,
    -- | The name of the preset you are creating.
    name :: Lude.Text,
    -- | Optional. A description of the preset you are creating.
    description :: Lude.Maybe Lude.Text,
    -- | The tags that you want to add to the resource. You can tag resources with a key-value pair or with only a key.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePreset' with the minimum fields required to make a request.
--
-- * 'settings' - Settings for preset
-- * 'category' - Optional. A category for the preset you are creating.
-- * 'name' - The name of the preset you are creating.
-- * 'description' - Optional. A description of the preset you are creating.
-- * 'tags' - The tags that you want to add to the resource. You can tag resources with a key-value pair or with only a key.
mkCreatePreset ::
  -- | 'settings'
  PresetSettings ->
  -- | 'name'
  Lude.Text ->
  CreatePreset
mkCreatePreset pSettings_ pName_ =
  CreatePreset'
    { settings = pSettings_,
      category = Lude.Nothing,
      name = pName_,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | Settings for preset
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpSettings :: Lens.Lens' CreatePreset PresetSettings
cpSettings = Lens.lens (settings :: CreatePreset -> PresetSettings) (\s a -> s {settings = a} :: CreatePreset)
{-# DEPRECATED cpSettings "Use generic-lens or generic-optics with 'settings' instead." #-}

-- | Optional. A category for the preset you are creating.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpCategory :: Lens.Lens' CreatePreset (Lude.Maybe Lude.Text)
cpCategory = Lens.lens (category :: CreatePreset -> Lude.Maybe Lude.Text) (\s a -> s {category = a} :: CreatePreset)
{-# DEPRECATED cpCategory "Use generic-lens or generic-optics with 'category' instead." #-}

-- | The name of the preset you are creating.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpName :: Lens.Lens' CreatePreset Lude.Text
cpName = Lens.lens (name :: CreatePreset -> Lude.Text) (\s a -> s {name = a} :: CreatePreset)
{-# DEPRECATED cpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Optional. A description of the preset you are creating.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpDescription :: Lens.Lens' CreatePreset (Lude.Maybe Lude.Text)
cpDescription = Lens.lens (description :: CreatePreset -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreatePreset)
{-# DEPRECATED cpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tags that you want to add to the resource. You can tag resources with a key-value pair or with only a key.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTags :: Lens.Lens' CreatePreset (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cpTags = Lens.lens (tags :: CreatePreset -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreatePreset)
{-# DEPRECATED cpTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreatePreset where
  type Rs CreatePreset = CreatePresetResponse
  request = Req.postJSON mediaConvertService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreatePresetResponse'
            Lude.<$> (x Lude..?> "preset") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreatePreset where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreatePreset where
  toJSON CreatePreset' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("settings" Lude..= settings),
            ("category" Lude..=) Lude.<$> category,
            Lude.Just ("name" Lude..= name),
            ("description" Lude..=) Lude.<$> description,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreatePreset where
  toPath = Lude.const "/2017-08-29/presets"

instance Lude.ToQuery CreatePreset where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreatePresetResponse' smart constructor.
data CreatePresetResponse = CreatePresetResponse'
  { -- | A preset is a collection of preconfigured media conversion settings that you want MediaConvert to apply to the output during the conversion process.
    preset :: Lude.Maybe Preset,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePresetResponse' with the minimum fields required to make a request.
--
-- * 'preset' - A preset is a collection of preconfigured media conversion settings that you want MediaConvert to apply to the output during the conversion process.
-- * 'responseStatus' - The response status code.
mkCreatePresetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreatePresetResponse
mkCreatePresetResponse pResponseStatus_ =
  CreatePresetResponse'
    { preset = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A preset is a collection of preconfigured media conversion settings that you want MediaConvert to apply to the output during the conversion process.
--
-- /Note:/ Consider using 'preset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsPreset :: Lens.Lens' CreatePresetResponse (Lude.Maybe Preset)
cprsPreset = Lens.lens (preset :: CreatePresetResponse -> Lude.Maybe Preset) (\s a -> s {preset = a} :: CreatePresetResponse)
{-# DEPRECATED cprsPreset "Use generic-lens or generic-optics with 'preset' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsResponseStatus :: Lens.Lens' CreatePresetResponse Lude.Int
cprsResponseStatus = Lens.lens (responseStatus :: CreatePresetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePresetResponse)
{-# DEPRECATED cprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
