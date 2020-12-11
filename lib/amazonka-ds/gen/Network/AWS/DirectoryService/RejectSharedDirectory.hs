{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.RejectSharedDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects a directory sharing request that was sent from the directory owner account.
module Network.AWS.DirectoryService.RejectSharedDirectory
  ( -- * Creating a request
    RejectSharedDirectory (..),
    mkRejectSharedDirectory,

    -- ** Request lenses
    rsdSharedDirectoryId,

    -- * Destructuring the response
    RejectSharedDirectoryResponse (..),
    mkRejectSharedDirectoryResponse,

    -- ** Response lenses
    rsdrsSharedDirectoryId,
    rsdrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRejectSharedDirectory' smart constructor.
newtype RejectSharedDirectory = RejectSharedDirectory'
  { sharedDirectoryId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RejectSharedDirectory' with the minimum fields required to make a request.
--
-- * 'sharedDirectoryId' - Identifier of the shared directory in the directory consumer account. This identifier is different for each directory owner account.
mkRejectSharedDirectory ::
  -- | 'sharedDirectoryId'
  Lude.Text ->
  RejectSharedDirectory
mkRejectSharedDirectory pSharedDirectoryId_ =
  RejectSharedDirectory' {sharedDirectoryId = pSharedDirectoryId_}

-- | Identifier of the shared directory in the directory consumer account. This identifier is different for each directory owner account.
--
-- /Note:/ Consider using 'sharedDirectoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsdSharedDirectoryId :: Lens.Lens' RejectSharedDirectory Lude.Text
rsdSharedDirectoryId = Lens.lens (sharedDirectoryId :: RejectSharedDirectory -> Lude.Text) (\s a -> s {sharedDirectoryId = a} :: RejectSharedDirectory)
{-# DEPRECATED rsdSharedDirectoryId "Use generic-lens or generic-optics with 'sharedDirectoryId' instead." #-}

instance Lude.AWSRequest RejectSharedDirectory where
  type Rs RejectSharedDirectory = RejectSharedDirectoryResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          RejectSharedDirectoryResponse'
            Lude.<$> (x Lude..?> "SharedDirectoryId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RejectSharedDirectory where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DirectoryService_20150416.RejectSharedDirectory" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RejectSharedDirectory where
  toJSON RejectSharedDirectory' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("SharedDirectoryId" Lude..= sharedDirectoryId)]
      )

instance Lude.ToPath RejectSharedDirectory where
  toPath = Lude.const "/"

instance Lude.ToQuery RejectSharedDirectory where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRejectSharedDirectoryResponse' smart constructor.
data RejectSharedDirectoryResponse = RejectSharedDirectoryResponse'
  { sharedDirectoryId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'RejectSharedDirectoryResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'sharedDirectoryId' - Identifier of the shared directory in the directory consumer account.
mkRejectSharedDirectoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RejectSharedDirectoryResponse
mkRejectSharedDirectoryResponse pResponseStatus_ =
  RejectSharedDirectoryResponse'
    { sharedDirectoryId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Identifier of the shared directory in the directory consumer account.
--
-- /Note:/ Consider using 'sharedDirectoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsdrsSharedDirectoryId :: Lens.Lens' RejectSharedDirectoryResponse (Lude.Maybe Lude.Text)
rsdrsSharedDirectoryId = Lens.lens (sharedDirectoryId :: RejectSharedDirectoryResponse -> Lude.Maybe Lude.Text) (\s a -> s {sharedDirectoryId = a} :: RejectSharedDirectoryResponse)
{-# DEPRECATED rsdrsSharedDirectoryId "Use generic-lens or generic-optics with 'sharedDirectoryId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsdrsResponseStatus :: Lens.Lens' RejectSharedDirectoryResponse Lude.Int
rsdrsResponseStatus = Lens.lens (responseStatus :: RejectSharedDirectoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RejectSharedDirectoryResponse)
{-# DEPRECATED rsdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
