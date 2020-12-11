{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    UpdatePreset (..),
    mkUpdatePreset,

    -- ** Request lenses
    upSettings,
    upCategory,
    upDescription,
    upName,

    -- * Destructuring the response
    UpdatePresetResponse (..),
    mkUpdatePresetResponse,

    -- ** Response lenses
    uprsPreset,
    uprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdatePreset' smart constructor.
data UpdatePreset = UpdatePreset'
  { settings ::
      Lude.Maybe PresetSettings,
    category :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePreset' with the minimum fields required to make a request.
--
-- * 'category' - The new category for the preset, if you are changing it.
-- * 'description' - The new description for the preset, if you are changing it.
-- * 'name' - The name of the preset you are modifying.
-- * 'settings' - Settings for preset
mkUpdatePreset ::
  -- | 'name'
  Lude.Text ->
  UpdatePreset
mkUpdatePreset pName_ =
  UpdatePreset'
    { settings = Lude.Nothing,
      category = Lude.Nothing,
      description = Lude.Nothing,
      name = pName_
    }

-- | Settings for preset
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSettings :: Lens.Lens' UpdatePreset (Lude.Maybe PresetSettings)
upSettings = Lens.lens (settings :: UpdatePreset -> Lude.Maybe PresetSettings) (\s a -> s {settings = a} :: UpdatePreset)
{-# DEPRECATED upSettings "Use generic-lens or generic-optics with 'settings' instead." #-}

-- | The new category for the preset, if you are changing it.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upCategory :: Lens.Lens' UpdatePreset (Lude.Maybe Lude.Text)
upCategory = Lens.lens (category :: UpdatePreset -> Lude.Maybe Lude.Text) (\s a -> s {category = a} :: UpdatePreset)
{-# DEPRECATED upCategory "Use generic-lens or generic-optics with 'category' instead." #-}

-- | The new description for the preset, if you are changing it.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upDescription :: Lens.Lens' UpdatePreset (Lude.Maybe Lude.Text)
upDescription = Lens.lens (description :: UpdatePreset -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdatePreset)
{-# DEPRECATED upDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the preset you are modifying.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upName :: Lens.Lens' UpdatePreset Lude.Text
upName = Lens.lens (name :: UpdatePreset -> Lude.Text) (\s a -> s {name = a} :: UpdatePreset)
{-# DEPRECATED upName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest UpdatePreset where
  type Rs UpdatePreset = UpdatePresetResponse
  request = Req.putJSON mediaConvertService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdatePresetResponse'
            Lude.<$> (x Lude..?> "preset") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdatePreset where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdatePreset where
  toJSON UpdatePreset' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("settings" Lude..=) Lude.<$> settings,
            ("category" Lude..=) Lude.<$> category,
            ("description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath UpdatePreset where
  toPath UpdatePreset' {..} =
    Lude.mconcat ["/2017-08-29/presets/", Lude.toBS name]

instance Lude.ToQuery UpdatePreset where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdatePresetResponse' smart constructor.
data UpdatePresetResponse = UpdatePresetResponse'
  { preset ::
      Lude.Maybe Preset,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePresetResponse' with the minimum fields required to make a request.
--
-- * 'preset' - A preset is a collection of preconfigured media conversion settings that you want MediaConvert to apply to the output during the conversion process.
-- * 'responseStatus' - The response status code.
mkUpdatePresetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdatePresetResponse
mkUpdatePresetResponse pResponseStatus_ =
  UpdatePresetResponse'
    { preset = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A preset is a collection of preconfigured media conversion settings that you want MediaConvert to apply to the output during the conversion process.
--
-- /Note:/ Consider using 'preset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsPreset :: Lens.Lens' UpdatePresetResponse (Lude.Maybe Preset)
uprsPreset = Lens.lens (preset :: UpdatePresetResponse -> Lude.Maybe Preset) (\s a -> s {preset = a} :: UpdatePresetResponse)
{-# DEPRECATED uprsPreset "Use generic-lens or generic-optics with 'preset' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsResponseStatus :: Lens.Lens' UpdatePresetResponse Lude.Int
uprsResponseStatus = Lens.lens (responseStatus :: UpdatePresetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdatePresetResponse)
{-# DEPRECATED uprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
