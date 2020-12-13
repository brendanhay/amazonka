{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.DeletePreset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The DeletePreset operation removes a preset that you've added in an AWS region.
module Network.AWS.ElasticTranscoder.DeletePreset
  ( -- * Creating a request
    DeletePreset (..),
    mkDeletePreset,

    -- ** Request lenses
    dId,

    -- * Destructuring the response
    DeletePresetResponse (..),
    mkDeletePresetResponse,

    -- ** Response lenses
    dprsResponseStatus,
  )
where

import Network.AWS.ElasticTranscoder.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The @DeletePresetRequest@ structure.
--
-- /See:/ 'mkDeletePreset' smart constructor.
newtype DeletePreset = DeletePreset'
  { -- | The identifier of the preset for which you want to get detailed information.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePreset' with the minimum fields required to make a request.
--
-- * 'id' - The identifier of the preset for which you want to get detailed information.
mkDeletePreset ::
  -- | 'id'
  Lude.Text ->
  DeletePreset
mkDeletePreset pId_ = DeletePreset' {id = pId_}

-- | The identifier of the preset for which you want to get detailed information.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dId :: Lens.Lens' DeletePreset Lude.Text
dId = Lens.lens (id :: DeletePreset -> Lude.Text) (\s a -> s {id = a} :: DeletePreset)
{-# DEPRECATED dId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DeletePreset where
  type Rs DeletePreset = DeletePresetResponse
  request = Req.delete elasticTranscoderService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeletePresetResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeletePreset where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeletePreset where
  toPath DeletePreset' {..} =
    Lude.mconcat ["/2012-09-25/presets/", Lude.toBS id]

instance Lude.ToQuery DeletePreset where
  toQuery = Lude.const Lude.mempty

-- | The @DeletePresetResponse@ structure.
--
-- /See:/ 'mkDeletePresetResponse' smart constructor.
newtype DeletePresetResponse = DeletePresetResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePresetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeletePresetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeletePresetResponse
mkDeletePresetResponse pResponseStatus_ =
  DeletePresetResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsResponseStatus :: Lens.Lens' DeletePresetResponse Lude.Int
dprsResponseStatus = Lens.lens (responseStatus :: DeletePresetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeletePresetResponse)
{-# DEPRECATED dprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
