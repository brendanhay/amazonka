{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.RemoveRegion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops all replication and removes the domain controllers from the specified Region. You cannot remove the primary Region with this operation. Instead, use the @DeleteDirectory@ API.
module Network.AWS.DirectoryService.RemoveRegion
  ( -- * Creating a request
    RemoveRegion (..),
    mkRemoveRegion,

    -- ** Request lenses
    rrDirectoryId,

    -- * Destructuring the response
    RemoveRegionResponse (..),
    mkRemoveRegionResponse,

    -- ** Response lenses
    rrrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRemoveRegion' smart constructor.
newtype RemoveRegion = RemoveRegion' {directoryId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveRegion' with the minimum fields required to make a request.
--
-- * 'directoryId' - The identifier of the directory for which you want to remove Region replication.
mkRemoveRegion ::
  -- | 'directoryId'
  Lude.Text ->
  RemoveRegion
mkRemoveRegion pDirectoryId_ =
  RemoveRegion' {directoryId = pDirectoryId_}

-- | The identifier of the directory for which you want to remove Region replication.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrDirectoryId :: Lens.Lens' RemoveRegion Lude.Text
rrDirectoryId = Lens.lens (directoryId :: RemoveRegion -> Lude.Text) (\s a -> s {directoryId = a} :: RemoveRegion)
{-# DEPRECATED rrDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

instance Lude.AWSRequest RemoveRegion where
  type Rs RemoveRegion = RemoveRegionResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveEmpty
      ( \s h x ->
          RemoveRegionResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RemoveRegion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DirectoryService_20150416.RemoveRegion" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RemoveRegion where
  toJSON RemoveRegion' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("DirectoryId" Lude..= directoryId)])

instance Lude.ToPath RemoveRegion where
  toPath = Lude.const "/"

instance Lude.ToQuery RemoveRegion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRemoveRegionResponse' smart constructor.
newtype RemoveRegionResponse = RemoveRegionResponse'
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

-- | Creates a value of 'RemoveRegionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkRemoveRegionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RemoveRegionResponse
mkRemoveRegionResponse pResponseStatus_ =
  RemoveRegionResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrrsResponseStatus :: Lens.Lens' RemoveRegionResponse Lude.Int
rrrsResponseStatus = Lens.lens (responseStatus :: RemoveRegionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RemoveRegionResponse)
{-# DEPRECATED rrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
