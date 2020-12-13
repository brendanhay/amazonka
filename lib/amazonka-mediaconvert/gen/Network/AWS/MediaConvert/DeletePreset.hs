{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.DeletePreset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently delete a preset you have created.
module Network.AWS.MediaConvert.DeletePreset
  ( -- * Creating a request
    DeletePreset (..),
    mkDeletePreset,

    -- ** Request lenses
    dpName,

    -- * Destructuring the response
    DeletePresetResponse (..),
    mkDeletePresetResponse,

    -- ** Response lenses
    dprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeletePreset' smart constructor.
newtype DeletePreset = DeletePreset'
  { -- | The name of the preset to be deleted.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePreset' with the minimum fields required to make a request.
--
-- * 'name' - The name of the preset to be deleted.
mkDeletePreset ::
  -- | 'name'
  Lude.Text ->
  DeletePreset
mkDeletePreset pName_ = DeletePreset' {name = pName_}

-- | The name of the preset to be deleted.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpName :: Lens.Lens' DeletePreset Lude.Text
dpName = Lens.lens (name :: DeletePreset -> Lude.Text) (\s a -> s {name = a} :: DeletePreset)
{-# DEPRECATED dpName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeletePreset where
  type Rs DeletePreset = DeletePresetResponse
  request = Req.delete mediaConvertService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeletePresetResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeletePreset where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeletePreset where
  toPath DeletePreset' {..} =
    Lude.mconcat ["/2017-08-29/presets/", Lude.toBS name]

instance Lude.ToQuery DeletePreset where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeletePresetResponse' smart constructor.
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
