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
    rprsPreset,
    rprsResponseStatus,
  )
where

import Network.AWS.ElasticTranscoder.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The @ReadPresetRequest@ structure.
--
-- /See:/ 'mkReadPreset' smart constructor.
newtype ReadPreset = ReadPreset'
  { -- | The identifier of the preset for which you want to get detailed information.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReadPreset' with the minimum fields required to make a request.
--
-- * 'id' - The identifier of the preset for which you want to get detailed information.
mkReadPreset ::
  -- | 'id'
  Lude.Text ->
  ReadPreset
mkReadPreset pId_ = ReadPreset' {id = pId_}

-- | The identifier of the preset for which you want to get detailed information.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpId :: Lens.Lens' ReadPreset Lude.Text
rpId = Lens.lens (id :: ReadPreset -> Lude.Text) (\s a -> s {id = a} :: ReadPreset)
{-# DEPRECATED rpId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest ReadPreset where
  type Rs ReadPreset = ReadPresetResponse
  request = Req.get elasticTranscoderService
  response =
    Res.receiveJSON
      ( \s h x ->
          ReadPresetResponse'
            Lude.<$> (x Lude..?> "Preset") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ReadPreset where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ReadPreset where
  toPath ReadPreset' {..} =
    Lude.mconcat ["/2012-09-25/presets/", Lude.toBS id]

instance Lude.ToQuery ReadPreset where
  toQuery = Lude.const Lude.mempty

-- | The @ReadPresetResponse@ structure.
--
-- /See:/ 'mkReadPresetResponse' smart constructor.
data ReadPresetResponse = ReadPresetResponse'
  { -- | A section of the response body that provides information about the preset.
    preset :: Lude.Maybe Preset,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReadPresetResponse' with the minimum fields required to make a request.
--
-- * 'preset' - A section of the response body that provides information about the preset.
-- * 'responseStatus' - The response status code.
mkReadPresetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ReadPresetResponse
mkReadPresetResponse pResponseStatus_ =
  ReadPresetResponse'
    { preset = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A section of the response body that provides information about the preset.
--
-- /Note:/ Consider using 'preset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprsPreset :: Lens.Lens' ReadPresetResponse (Lude.Maybe Preset)
rprsPreset = Lens.lens (preset :: ReadPresetResponse -> Lude.Maybe Preset) (\s a -> s {preset = a} :: ReadPresetResponse)
{-# DEPRECATED rprsPreset "Use generic-lens or generic-optics with 'preset' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprsResponseStatus :: Lens.Lens' ReadPresetResponse Lude.Int
rprsResponseStatus = Lens.lens (responseStatus :: ReadPresetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ReadPresetResponse)
{-# DEPRECATED rprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
