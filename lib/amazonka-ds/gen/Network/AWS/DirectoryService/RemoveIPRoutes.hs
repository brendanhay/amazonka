{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.RemoveIPRoutes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes IP address blocks from a directory.
module Network.AWS.DirectoryService.RemoveIPRoutes
  ( -- * Creating a request
    RemoveIPRoutes (..),
    mkRemoveIPRoutes,

    -- ** Request lenses
    rirDirectoryId,
    rirCidrIPs,

    -- * Destructuring the response
    RemoveIPRoutesResponse (..),
    mkRemoveIPRoutesResponse,

    -- ** Response lenses
    rirrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRemoveIPRoutes' smart constructor.
data RemoveIPRoutes = RemoveIPRoutes'
  { directoryId :: Lude.Text,
    cidrIPs :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveIPRoutes' with the minimum fields required to make a request.
--
-- * 'cidrIPs' - IP address blocks that you want to remove.
-- * 'directoryId' - Identifier (ID) of the directory from which you want to remove the IP addresses.
mkRemoveIPRoutes ::
  -- | 'directoryId'
  Lude.Text ->
  RemoveIPRoutes
mkRemoveIPRoutes pDirectoryId_ =
  RemoveIPRoutes'
    { directoryId = pDirectoryId_,
      cidrIPs = Lude.mempty
    }

-- | Identifier (ID) of the directory from which you want to remove the IP addresses.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rirDirectoryId :: Lens.Lens' RemoveIPRoutes Lude.Text
rirDirectoryId = Lens.lens (directoryId :: RemoveIPRoutes -> Lude.Text) (\s a -> s {directoryId = a} :: RemoveIPRoutes)
{-# DEPRECATED rirDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | IP address blocks that you want to remove.
--
-- /Note:/ Consider using 'cidrIPs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rirCidrIPs :: Lens.Lens' RemoveIPRoutes [Lude.Text]
rirCidrIPs = Lens.lens (cidrIPs :: RemoveIPRoutes -> [Lude.Text]) (\s a -> s {cidrIPs = a} :: RemoveIPRoutes)
{-# DEPRECATED rirCidrIPs "Use generic-lens or generic-optics with 'cidrIPs' instead." #-}

instance Lude.AWSRequest RemoveIPRoutes where
  type Rs RemoveIPRoutes = RemoveIPRoutesResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveEmpty
      ( \s h x ->
          RemoveIPRoutesResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RemoveIPRoutes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DirectoryService_20150416.RemoveIpRoutes" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RemoveIPRoutes where
  toJSON RemoveIPRoutes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DirectoryId" Lude..= directoryId),
            Lude.Just ("CidrIps" Lude..= cidrIPs)
          ]
      )

instance Lude.ToPath RemoveIPRoutes where
  toPath = Lude.const "/"

instance Lude.ToQuery RemoveIPRoutes where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRemoveIPRoutesResponse' smart constructor.
newtype RemoveIPRoutesResponse = RemoveIPRoutesResponse'
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

-- | Creates a value of 'RemoveIPRoutesResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkRemoveIPRoutesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RemoveIPRoutesResponse
mkRemoveIPRoutesResponse pResponseStatus_ =
  RemoveIPRoutesResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rirrsResponseStatus :: Lens.Lens' RemoveIPRoutesResponse Lude.Int
rirrsResponseStatus = Lens.lens (responseStatus :: RemoveIPRoutesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RemoveIPRoutesResponse)
{-# DEPRECATED rirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
