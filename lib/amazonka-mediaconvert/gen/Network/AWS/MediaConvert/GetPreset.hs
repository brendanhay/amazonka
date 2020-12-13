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
    gprsPreset,
    gprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetPreset' smart constructor.
newtype GetPreset = GetPreset'
  { -- | The name of the preset.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPreset' with the minimum fields required to make a request.
--
-- * 'name' - The name of the preset.
mkGetPreset ::
  -- | 'name'
  Lude.Text ->
  GetPreset
mkGetPreset pName_ = GetPreset' {name = pName_}

-- | The name of the preset.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpName :: Lens.Lens' GetPreset Lude.Text
gpName = Lens.lens (name :: GetPreset -> Lude.Text) (\s a -> s {name = a} :: GetPreset)
{-# DEPRECATED gpName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest GetPreset where
  type Rs GetPreset = GetPresetResponse
  request = Req.get mediaConvertService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetPresetResponse'
            Lude.<$> (x Lude..?> "preset") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetPreset where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetPreset where
  toPath GetPreset' {..} =
    Lude.mconcat ["/2017-08-29/presets/", Lude.toBS name]

instance Lude.ToQuery GetPreset where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetPresetResponse' smart constructor.
data GetPresetResponse = GetPresetResponse'
  { -- | A preset is a collection of preconfigured media conversion settings that you want MediaConvert to apply to the output during the conversion process.
    preset :: Lude.Maybe Preset,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPresetResponse' with the minimum fields required to make a request.
--
-- * 'preset' - A preset is a collection of preconfigured media conversion settings that you want MediaConvert to apply to the output during the conversion process.
-- * 'responseStatus' - The response status code.
mkGetPresetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetPresetResponse
mkGetPresetResponse pResponseStatus_ =
  GetPresetResponse'
    { preset = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A preset is a collection of preconfigured media conversion settings that you want MediaConvert to apply to the output during the conversion process.
--
-- /Note:/ Consider using 'preset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsPreset :: Lens.Lens' GetPresetResponse (Lude.Maybe Preset)
gprsPreset = Lens.lens (preset :: GetPresetResponse -> Lude.Maybe Preset) (\s a -> s {preset = a} :: GetPresetResponse)
{-# DEPRECATED gprsPreset "Use generic-lens or generic-optics with 'preset' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsResponseStatus :: Lens.Lens' GetPresetResponse Lude.Int
gprsResponseStatus = Lens.lens (responseStatus :: GetPresetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetPresetResponse)
{-# DEPRECATED gprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
