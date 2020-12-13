{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteOTAUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an OTA update.
module Network.AWS.IoT.DeleteOTAUpdate
  ( -- * Creating a request
    DeleteOTAUpdate (..),
    mkDeleteOTAUpdate,

    -- ** Request lenses
    dotauForceDeleteAWSJob,
    dotauDeleteStream,
    dotauOtaUpdateId,

    -- * Destructuring the response
    DeleteOTAUpdateResponse (..),
    mkDeleteOTAUpdateResponse,

    -- ** Response lenses
    dotaursResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteOTAUpdate' smart constructor.
data DeleteOTAUpdate = DeleteOTAUpdate'
  { -- | Specifies if the AWS Job associated with the OTA update should be deleted when the OTA update is deleted.
    forceDeleteAWSJob :: Lude.Maybe Lude.Bool,
    -- | Specifies if the stream associated with an OTA update should be deleted when the OTA update is deleted.
    deleteStream :: Lude.Maybe Lude.Bool,
    -- | The ID of the OTA update to delete.
    otaUpdateId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteOTAUpdate' with the minimum fields required to make a request.
--
-- * 'forceDeleteAWSJob' - Specifies if the AWS Job associated with the OTA update should be deleted when the OTA update is deleted.
-- * 'deleteStream' - Specifies if the stream associated with an OTA update should be deleted when the OTA update is deleted.
-- * 'otaUpdateId' - The ID of the OTA update to delete.
mkDeleteOTAUpdate ::
  -- | 'otaUpdateId'
  Lude.Text ->
  DeleteOTAUpdate
mkDeleteOTAUpdate pOtaUpdateId_ =
  DeleteOTAUpdate'
    { forceDeleteAWSJob = Lude.Nothing,
      deleteStream = Lude.Nothing,
      otaUpdateId = pOtaUpdateId_
    }

-- | Specifies if the AWS Job associated with the OTA update should be deleted when the OTA update is deleted.
--
-- /Note:/ Consider using 'forceDeleteAWSJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dotauForceDeleteAWSJob :: Lens.Lens' DeleteOTAUpdate (Lude.Maybe Lude.Bool)
dotauForceDeleteAWSJob = Lens.lens (forceDeleteAWSJob :: DeleteOTAUpdate -> Lude.Maybe Lude.Bool) (\s a -> s {forceDeleteAWSJob = a} :: DeleteOTAUpdate)
{-# DEPRECATED dotauForceDeleteAWSJob "Use generic-lens or generic-optics with 'forceDeleteAWSJob' instead." #-}

-- | Specifies if the stream associated with an OTA update should be deleted when the OTA update is deleted.
--
-- /Note:/ Consider using 'deleteStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dotauDeleteStream :: Lens.Lens' DeleteOTAUpdate (Lude.Maybe Lude.Bool)
dotauDeleteStream = Lens.lens (deleteStream :: DeleteOTAUpdate -> Lude.Maybe Lude.Bool) (\s a -> s {deleteStream = a} :: DeleteOTAUpdate)
{-# DEPRECATED dotauDeleteStream "Use generic-lens or generic-optics with 'deleteStream' instead." #-}

-- | The ID of the OTA update to delete.
--
-- /Note:/ Consider using 'otaUpdateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dotauOtaUpdateId :: Lens.Lens' DeleteOTAUpdate Lude.Text
dotauOtaUpdateId = Lens.lens (otaUpdateId :: DeleteOTAUpdate -> Lude.Text) (\s a -> s {otaUpdateId = a} :: DeleteOTAUpdate)
{-# DEPRECATED dotauOtaUpdateId "Use generic-lens or generic-optics with 'otaUpdateId' instead." #-}

instance Lude.AWSRequest DeleteOTAUpdate where
  type Rs DeleteOTAUpdate = DeleteOTAUpdateResponse
  request = Req.delete ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteOTAUpdateResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteOTAUpdate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteOTAUpdate where
  toPath DeleteOTAUpdate' {..} =
    Lude.mconcat ["/otaUpdates/", Lude.toBS otaUpdateId]

instance Lude.ToQuery DeleteOTAUpdate where
  toQuery DeleteOTAUpdate' {..} =
    Lude.mconcat
      [ "forceDeleteAWSJob" Lude.=: forceDeleteAWSJob,
        "deleteStream" Lude.=: deleteStream
      ]

-- | /See:/ 'mkDeleteOTAUpdateResponse' smart constructor.
newtype DeleteOTAUpdateResponse = DeleteOTAUpdateResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteOTAUpdateResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteOTAUpdateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteOTAUpdateResponse
mkDeleteOTAUpdateResponse pResponseStatus_ =
  DeleteOTAUpdateResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dotaursResponseStatus :: Lens.Lens' DeleteOTAUpdateResponse Lude.Int
dotaursResponseStatus = Lens.lens (responseStatus :: DeleteOTAUpdateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteOTAUpdateResponse)
{-# DEPRECATED dotaursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
