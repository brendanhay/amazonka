{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.DeleteChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing Channel.
module Network.AWS.MediaPackage.DeleteChannel
  ( -- * Creating a request
    DeleteChannel (..),
    mkDeleteChannel,

    -- ** Request lenses
    dcId,

    -- * Destructuring the response
    DeleteChannelResponse (..),
    mkDeleteChannelResponse,

    -- ** Response lenses
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteChannel' smart constructor.
newtype DeleteChannel = DeleteChannel' {id :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteChannel' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the Channel to delete.
mkDeleteChannel ::
  -- | 'id'
  Lude.Text ->
  DeleteChannel
mkDeleteChannel pId_ = DeleteChannel' {id = pId_}

-- | The ID of the Channel to delete.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcId :: Lens.Lens' DeleteChannel Lude.Text
dcId = Lens.lens (id :: DeleteChannel -> Lude.Text) (\s a -> s {id = a} :: DeleteChannel)
{-# DEPRECATED dcId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DeleteChannel where
  type Rs DeleteChannel = DeleteChannelResponse
  request = Req.delete mediaPackageService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteChannelResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteChannel where
  toPath DeleteChannel' {..} =
    Lude.mconcat ["/channels/", Lude.toBS id]

instance Lude.ToQuery DeleteChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteChannelResponse' smart constructor.
newtype DeleteChannelResponse = DeleteChannelResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteChannelResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteChannelResponse
mkDeleteChannelResponse pResponseStatus_ =
  DeleteChannelResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteChannelResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeleteChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteChannelResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
