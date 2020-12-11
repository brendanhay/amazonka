{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.UnshareDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the directory sharing between the directory owner and consumer accounts.
module Network.AWS.DirectoryService.UnshareDirectory
  ( -- * Creating a request
    UnshareDirectory (..),
    mkUnshareDirectory,

    -- ** Request lenses
    udDirectoryId,
    udUnshareTarget,

    -- * Destructuring the response
    UnshareDirectoryResponse (..),
    mkUnshareDirectoryResponse,

    -- ** Response lenses
    udrsSharedDirectoryId,
    udrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUnshareDirectory' smart constructor.
data UnshareDirectory = UnshareDirectory'
  { directoryId :: Lude.Text,
    unshareTarget :: UnshareTarget
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnshareDirectory' with the minimum fields required to make a request.
--
-- * 'directoryId' - The identifier of the AWS Managed Microsoft AD directory that you want to stop sharing.
-- * 'unshareTarget' - Identifier for the directory consumer account with whom the directory has to be unshared.
mkUnshareDirectory ::
  -- | 'directoryId'
  Lude.Text ->
  -- | 'unshareTarget'
  UnshareTarget ->
  UnshareDirectory
mkUnshareDirectory pDirectoryId_ pUnshareTarget_ =
  UnshareDirectory'
    { directoryId = pDirectoryId_,
      unshareTarget = pUnshareTarget_
    }

-- | The identifier of the AWS Managed Microsoft AD directory that you want to stop sharing.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDirectoryId :: Lens.Lens' UnshareDirectory Lude.Text
udDirectoryId = Lens.lens (directoryId :: UnshareDirectory -> Lude.Text) (\s a -> s {directoryId = a} :: UnshareDirectory)
{-# DEPRECATED udDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | Identifier for the directory consumer account with whom the directory has to be unshared.
--
-- /Note:/ Consider using 'unshareTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udUnshareTarget :: Lens.Lens' UnshareDirectory UnshareTarget
udUnshareTarget = Lens.lens (unshareTarget :: UnshareDirectory -> UnshareTarget) (\s a -> s {unshareTarget = a} :: UnshareDirectory)
{-# DEPRECATED udUnshareTarget "Use generic-lens or generic-optics with 'unshareTarget' instead." #-}

instance Lude.AWSRequest UnshareDirectory where
  type Rs UnshareDirectory = UnshareDirectoryResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          UnshareDirectoryResponse'
            Lude.<$> (x Lude..?> "SharedDirectoryId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UnshareDirectory where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DirectoryService_20150416.UnshareDirectory" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UnshareDirectory where
  toJSON UnshareDirectory' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DirectoryId" Lude..= directoryId),
            Lude.Just ("UnshareTarget" Lude..= unshareTarget)
          ]
      )

instance Lude.ToPath UnshareDirectory where
  toPath = Lude.const "/"

instance Lude.ToQuery UnshareDirectory where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUnshareDirectoryResponse' smart constructor.
data UnshareDirectoryResponse = UnshareDirectoryResponse'
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

-- | Creates a value of 'UnshareDirectoryResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'sharedDirectoryId' - Identifier of the directory stored in the directory consumer account that is to be unshared from the specified directory (@DirectoryId@ ).
mkUnshareDirectoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UnshareDirectoryResponse
mkUnshareDirectoryResponse pResponseStatus_ =
  UnshareDirectoryResponse'
    { sharedDirectoryId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Identifier of the directory stored in the directory consumer account that is to be unshared from the specified directory (@DirectoryId@ ).
--
-- /Note:/ Consider using 'sharedDirectoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrsSharedDirectoryId :: Lens.Lens' UnshareDirectoryResponse (Lude.Maybe Lude.Text)
udrsSharedDirectoryId = Lens.lens (sharedDirectoryId :: UnshareDirectoryResponse -> Lude.Maybe Lude.Text) (\s a -> s {sharedDirectoryId = a} :: UnshareDirectoryResponse)
{-# DEPRECATED udrsSharedDirectoryId "Use generic-lens or generic-optics with 'sharedDirectoryId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrsResponseStatus :: Lens.Lens' UnshareDirectoryResponse Lude.Int
udrsResponseStatus = Lens.lens (responseStatus :: UnshareDirectoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UnshareDirectoryResponse)
{-# DEPRECATED udrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
