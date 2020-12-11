{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.CopyBackupToRegion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copy an AWS CloudHSM cluster backup to a different region.
module Network.AWS.CloudHSMv2.CopyBackupToRegion
  ( -- * Creating a request
    CopyBackupToRegion (..),
    mkCopyBackupToRegion,

    -- ** Request lenses
    cbtrTagList,
    cbtrDestinationRegion,
    cbtrBackupId,

    -- * Destructuring the response
    CopyBackupToRegionResponse (..),
    mkCopyBackupToRegionResponse,

    -- ** Response lenses
    cbtrrsDestinationBackup,
    cbtrrsResponseStatus,
  )
where

import Network.AWS.CloudHSMv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCopyBackupToRegion' smart constructor.
data CopyBackupToRegion = CopyBackupToRegion'
  { tagList ::
      Lude.Maybe [Tag],
    destinationRegion :: Lude.Text,
    backupId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CopyBackupToRegion' with the minimum fields required to make a request.
--
-- * 'backupId' - The ID of the backup that will be copied to the destination region.
-- * 'destinationRegion' - The AWS region that will contain your copied CloudHSM cluster backup.
-- * 'tagList' - Tags to apply to the destination backup during creation. If you specify tags, only these tags will be applied to the destination backup. If you do not specify tags, the service copies tags from the source backup to the destination backup.
mkCopyBackupToRegion ::
  -- | 'destinationRegion'
  Lude.Text ->
  -- | 'backupId'
  Lude.Text ->
  CopyBackupToRegion
mkCopyBackupToRegion pDestinationRegion_ pBackupId_ =
  CopyBackupToRegion'
    { tagList = Lude.Nothing,
      destinationRegion = pDestinationRegion_,
      backupId = pBackupId_
    }

-- | Tags to apply to the destination backup during creation. If you specify tags, only these tags will be applied to the destination backup. If you do not specify tags, the service copies tags from the source backup to the destination backup.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbtrTagList :: Lens.Lens' CopyBackupToRegion (Lude.Maybe [Tag])
cbtrTagList = Lens.lens (tagList :: CopyBackupToRegion -> Lude.Maybe [Tag]) (\s a -> s {tagList = a} :: CopyBackupToRegion)
{-# DEPRECATED cbtrTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

-- | The AWS region that will contain your copied CloudHSM cluster backup.
--
-- /Note:/ Consider using 'destinationRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbtrDestinationRegion :: Lens.Lens' CopyBackupToRegion Lude.Text
cbtrDestinationRegion = Lens.lens (destinationRegion :: CopyBackupToRegion -> Lude.Text) (\s a -> s {destinationRegion = a} :: CopyBackupToRegion)
{-# DEPRECATED cbtrDestinationRegion "Use generic-lens or generic-optics with 'destinationRegion' instead." #-}

-- | The ID of the backup that will be copied to the destination region.
--
-- /Note:/ Consider using 'backupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbtrBackupId :: Lens.Lens' CopyBackupToRegion Lude.Text
cbtrBackupId = Lens.lens (backupId :: CopyBackupToRegion -> Lude.Text) (\s a -> s {backupId = a} :: CopyBackupToRegion)
{-# DEPRECATED cbtrBackupId "Use generic-lens or generic-optics with 'backupId' instead." #-}

instance Lude.AWSRequest CopyBackupToRegion where
  type Rs CopyBackupToRegion = CopyBackupToRegionResponse
  request = Req.postJSON cloudHSMv2Service
  response =
    Res.receiveJSON
      ( \s h x ->
          CopyBackupToRegionResponse'
            Lude.<$> (x Lude..?> "DestinationBackup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CopyBackupToRegion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("BaldrApiService.CopyBackupToRegion" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CopyBackupToRegion where
  toJSON CopyBackupToRegion' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TagList" Lude..=) Lude.<$> tagList,
            Lude.Just ("DestinationRegion" Lude..= destinationRegion),
            Lude.Just ("BackupId" Lude..= backupId)
          ]
      )

instance Lude.ToPath CopyBackupToRegion where
  toPath = Lude.const "/"

instance Lude.ToQuery CopyBackupToRegion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCopyBackupToRegionResponse' smart constructor.
data CopyBackupToRegionResponse = CopyBackupToRegionResponse'
  { destinationBackup ::
      Lude.Maybe DestinationBackup,
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

-- | Creates a value of 'CopyBackupToRegionResponse' with the minimum fields required to make a request.
--
-- * 'destinationBackup' - Information on the backup that will be copied to the destination region, including CreateTimestamp, SourceBackup, SourceCluster, and Source Region. CreateTimestamp of the destination backup will be the same as that of the source backup.
--
-- You will need to use the @sourceBackupID@ returned in this operation to use the 'DescribeBackups' operation on the backup that will be copied to the destination region.
-- * 'responseStatus' - The response status code.
mkCopyBackupToRegionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CopyBackupToRegionResponse
mkCopyBackupToRegionResponse pResponseStatus_ =
  CopyBackupToRegionResponse'
    { destinationBackup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information on the backup that will be copied to the destination region, including CreateTimestamp, SourceBackup, SourceCluster, and Source Region. CreateTimestamp of the destination backup will be the same as that of the source backup.
--
-- You will need to use the @sourceBackupID@ returned in this operation to use the 'DescribeBackups' operation on the backup that will be copied to the destination region.
--
-- /Note:/ Consider using 'destinationBackup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbtrrsDestinationBackup :: Lens.Lens' CopyBackupToRegionResponse (Lude.Maybe DestinationBackup)
cbtrrsDestinationBackup = Lens.lens (destinationBackup :: CopyBackupToRegionResponse -> Lude.Maybe DestinationBackup) (\s a -> s {destinationBackup = a} :: CopyBackupToRegionResponse)
{-# DEPRECATED cbtrrsDestinationBackup "Use generic-lens or generic-optics with 'destinationBackup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbtrrsResponseStatus :: Lens.Lens' CopyBackupToRegionResponse Lude.Int
cbtrrsResponseStatus = Lens.lens (responseStatus :: CopyBackupToRegionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CopyBackupToRegionResponse)
{-# DEPRECATED cbtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
