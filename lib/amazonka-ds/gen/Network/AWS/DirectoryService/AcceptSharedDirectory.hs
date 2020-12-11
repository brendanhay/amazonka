{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.AcceptSharedDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a directory sharing request that was sent from the directory owner account.
module Network.AWS.DirectoryService.AcceptSharedDirectory
  ( -- * Creating a request
    AcceptSharedDirectory (..),
    mkAcceptSharedDirectory,

    -- ** Request lenses
    asdSharedDirectoryId,

    -- * Destructuring the response
    AcceptSharedDirectoryResponse (..),
    mkAcceptSharedDirectoryResponse,

    -- ** Response lenses
    asdrsSharedDirectory,
    asdrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAcceptSharedDirectory' smart constructor.
newtype AcceptSharedDirectory = AcceptSharedDirectory'
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

-- | Creates a value of 'AcceptSharedDirectory' with the minimum fields required to make a request.
--
-- * 'sharedDirectoryId' - Identifier of the shared directory in the directory consumer account. This identifier is different for each directory owner account.
mkAcceptSharedDirectory ::
  -- | 'sharedDirectoryId'
  Lude.Text ->
  AcceptSharedDirectory
mkAcceptSharedDirectory pSharedDirectoryId_ =
  AcceptSharedDirectory' {sharedDirectoryId = pSharedDirectoryId_}

-- | Identifier of the shared directory in the directory consumer account. This identifier is different for each directory owner account.
--
-- /Note:/ Consider using 'sharedDirectoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asdSharedDirectoryId :: Lens.Lens' AcceptSharedDirectory Lude.Text
asdSharedDirectoryId = Lens.lens (sharedDirectoryId :: AcceptSharedDirectory -> Lude.Text) (\s a -> s {sharedDirectoryId = a} :: AcceptSharedDirectory)
{-# DEPRECATED asdSharedDirectoryId "Use generic-lens or generic-optics with 'sharedDirectoryId' instead." #-}

instance Lude.AWSRequest AcceptSharedDirectory where
  type Rs AcceptSharedDirectory = AcceptSharedDirectoryResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          AcceptSharedDirectoryResponse'
            Lude.<$> (x Lude..?> "SharedDirectory")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AcceptSharedDirectory where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DirectoryService_20150416.AcceptSharedDirectory" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AcceptSharedDirectory where
  toJSON AcceptSharedDirectory' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("SharedDirectoryId" Lude..= sharedDirectoryId)]
      )

instance Lude.ToPath AcceptSharedDirectory where
  toPath = Lude.const "/"

instance Lude.ToQuery AcceptSharedDirectory where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAcceptSharedDirectoryResponse' smart constructor.
data AcceptSharedDirectoryResponse = AcceptSharedDirectoryResponse'
  { sharedDirectory ::
      Lude.Maybe SharedDirectory,
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AcceptSharedDirectoryResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'sharedDirectory' - The shared directory in the directory consumer account.
mkAcceptSharedDirectoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AcceptSharedDirectoryResponse
mkAcceptSharedDirectoryResponse pResponseStatus_ =
  AcceptSharedDirectoryResponse'
    { sharedDirectory = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The shared directory in the directory consumer account.
--
-- /Note:/ Consider using 'sharedDirectory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asdrsSharedDirectory :: Lens.Lens' AcceptSharedDirectoryResponse (Lude.Maybe SharedDirectory)
asdrsSharedDirectory = Lens.lens (sharedDirectory :: AcceptSharedDirectoryResponse -> Lude.Maybe SharedDirectory) (\s a -> s {sharedDirectory = a} :: AcceptSharedDirectoryResponse)
{-# DEPRECATED asdrsSharedDirectory "Use generic-lens or generic-optics with 'sharedDirectory' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asdrsResponseStatus :: Lens.Lens' AcceptSharedDirectoryResponse Lude.Int
asdrsResponseStatus = Lens.lens (responseStatus :: AcceptSharedDirectoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AcceptSharedDirectoryResponse)
{-# DEPRECATED asdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
