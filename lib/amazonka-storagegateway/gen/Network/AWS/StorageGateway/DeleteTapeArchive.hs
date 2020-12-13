{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DeleteTapeArchive
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified virtual tape from the virtual tape shelf (VTS). This operation is only supported in the tape gateway type.
module Network.AWS.StorageGateway.DeleteTapeArchive
  ( -- * Creating a request
    DeleteTapeArchive (..),
    mkDeleteTapeArchive,

    -- ** Request lenses
    dtaTapeARN,
    dtaBypassGovernanceRetention,

    -- * Destructuring the response
    DeleteTapeArchiveResponse (..),
    mkDeleteTapeArchiveResponse,

    -- ** Response lenses
    dtarsTapeARN,
    dtarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | DeleteTapeArchiveInput
--
-- /See:/ 'mkDeleteTapeArchive' smart constructor.
data DeleteTapeArchive = DeleteTapeArchive'
  { -- | The Amazon Resource Name (ARN) of the virtual tape to delete from the virtual tape shelf (VTS).
    tapeARN :: Lude.Text,
    -- | Set to @TRUE@ to delete an archived tape that belongs to a custom pool with tape retention lock. Only archived tapes with tape retention lock set to @governance@ can be deleted. Archived tapes with tape retention lock set to @compliance@ can't be deleted.
    bypassGovernanceRetention :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTapeArchive' with the minimum fields required to make a request.
--
-- * 'tapeARN' - The Amazon Resource Name (ARN) of the virtual tape to delete from the virtual tape shelf (VTS).
-- * 'bypassGovernanceRetention' - Set to @TRUE@ to delete an archived tape that belongs to a custom pool with tape retention lock. Only archived tapes with tape retention lock set to @governance@ can be deleted. Archived tapes with tape retention lock set to @compliance@ can't be deleted.
mkDeleteTapeArchive ::
  -- | 'tapeARN'
  Lude.Text ->
  DeleteTapeArchive
mkDeleteTapeArchive pTapeARN_ =
  DeleteTapeArchive'
    { tapeARN = pTapeARN_,
      bypassGovernanceRetention = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the virtual tape to delete from the virtual tape shelf (VTS).
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtaTapeARN :: Lens.Lens' DeleteTapeArchive Lude.Text
dtaTapeARN = Lens.lens (tapeARN :: DeleteTapeArchive -> Lude.Text) (\s a -> s {tapeARN = a} :: DeleteTapeArchive)
{-# DEPRECATED dtaTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

-- | Set to @TRUE@ to delete an archived tape that belongs to a custom pool with tape retention lock. Only archived tapes with tape retention lock set to @governance@ can be deleted. Archived tapes with tape retention lock set to @compliance@ can't be deleted.
--
-- /Note:/ Consider using 'bypassGovernanceRetention' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtaBypassGovernanceRetention :: Lens.Lens' DeleteTapeArchive (Lude.Maybe Lude.Bool)
dtaBypassGovernanceRetention = Lens.lens (bypassGovernanceRetention :: DeleteTapeArchive -> Lude.Maybe Lude.Bool) (\s a -> s {bypassGovernanceRetention = a} :: DeleteTapeArchive)
{-# DEPRECATED dtaBypassGovernanceRetention "Use generic-lens or generic-optics with 'bypassGovernanceRetention' instead." #-}

instance Lude.AWSRequest DeleteTapeArchive where
  type Rs DeleteTapeArchive = DeleteTapeArchiveResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteTapeArchiveResponse'
            Lude.<$> (x Lude..?> "TapeARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTapeArchive where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.DeleteTapeArchive" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteTapeArchive where
  toJSON DeleteTapeArchive' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TapeARN" Lude..= tapeARN),
            ("BypassGovernanceRetention" Lude..=)
              Lude.<$> bypassGovernanceRetention
          ]
      )

instance Lude.ToPath DeleteTapeArchive where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTapeArchive where
  toQuery = Lude.const Lude.mempty

-- | DeleteTapeArchiveOutput
--
-- /See:/ 'mkDeleteTapeArchiveResponse' smart constructor.
data DeleteTapeArchiveResponse = DeleteTapeArchiveResponse'
  { -- | The Amazon Resource Name (ARN) of the virtual tape that was deleted from the virtual tape shelf (VTS).
    tapeARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTapeArchiveResponse' with the minimum fields required to make a request.
--
-- * 'tapeARN' - The Amazon Resource Name (ARN) of the virtual tape that was deleted from the virtual tape shelf (VTS).
-- * 'responseStatus' - The response status code.
mkDeleteTapeArchiveResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTapeArchiveResponse
mkDeleteTapeArchiveResponse pResponseStatus_ =
  DeleteTapeArchiveResponse'
    { tapeARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the virtual tape that was deleted from the virtual tape shelf (VTS).
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtarsTapeARN :: Lens.Lens' DeleteTapeArchiveResponse (Lude.Maybe Lude.Text)
dtarsTapeARN = Lens.lens (tapeARN :: DeleteTapeArchiveResponse -> Lude.Maybe Lude.Text) (\s a -> s {tapeARN = a} :: DeleteTapeArchiveResponse)
{-# DEPRECATED dtarsTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtarsResponseStatus :: Lens.Lens' DeleteTapeArchiveResponse Lude.Int
dtarsResponseStatus = Lens.lens (responseStatus :: DeleteTapeArchiveResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTapeArchiveResponse)
{-# DEPRECATED dtarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
