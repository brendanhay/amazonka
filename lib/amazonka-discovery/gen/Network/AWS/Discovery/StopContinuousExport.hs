{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.StopContinuousExport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stop the continuous flow of agent's discovered data into Amazon Athena.
module Network.AWS.Discovery.StopContinuousExport
  ( -- * Creating a request
    StopContinuousExport (..),
    mkStopContinuousExport,

    -- ** Request lenses
    sceExportId,

    -- * Destructuring the response
    StopContinuousExportResponse (..),
    mkStopContinuousExportResponse,

    -- ** Response lenses
    srsStartTime,
    srsStopTime,
    srsResponseStatus,
  )
where

import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopContinuousExport' smart constructor.
newtype StopContinuousExport = StopContinuousExport'
  { -- | The unique ID assigned to this export.
    exportId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopContinuousExport' with the minimum fields required to make a request.
--
-- * 'exportId' - The unique ID assigned to this export.
mkStopContinuousExport ::
  -- | 'exportId'
  Lude.Text ->
  StopContinuousExport
mkStopContinuousExport pExportId_ =
  StopContinuousExport' {exportId = pExportId_}

-- | The unique ID assigned to this export.
--
-- /Note:/ Consider using 'exportId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sceExportId :: Lens.Lens' StopContinuousExport Lude.Text
sceExportId = Lens.lens (exportId :: StopContinuousExport -> Lude.Text) (\s a -> s {exportId = a} :: StopContinuousExport)
{-# DEPRECATED sceExportId "Use generic-lens or generic-optics with 'exportId' instead." #-}

instance Lude.AWSRequest StopContinuousExport where
  type Rs StopContinuousExport = StopContinuousExportResponse
  request = Req.postJSON discoveryService
  response =
    Res.receiveJSON
      ( \s h x ->
          StopContinuousExportResponse'
            Lude.<$> (x Lude..?> "startTime")
            Lude.<*> (x Lude..?> "stopTime")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopContinuousExport where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSPoseidonService_V2015_11_01.StopContinuousExport" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopContinuousExport where
  toJSON StopContinuousExport' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("exportId" Lude..= exportId)])

instance Lude.ToPath StopContinuousExport where
  toPath = Lude.const "/"

instance Lude.ToQuery StopContinuousExport where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopContinuousExportResponse' smart constructor.
data StopContinuousExportResponse = StopContinuousExportResponse'
  { -- | Timestamp that represents when this continuous export started collecting data.
    startTime :: Lude.Maybe Lude.Timestamp,
    -- | Timestamp that represents when this continuous export was stopped.
    stopTime :: Lude.Maybe Lude.Timestamp,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopContinuousExportResponse' with the minimum fields required to make a request.
--
-- * 'startTime' - Timestamp that represents when this continuous export started collecting data.
-- * 'stopTime' - Timestamp that represents when this continuous export was stopped.
-- * 'responseStatus' - The response status code.
mkStopContinuousExportResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopContinuousExportResponse
mkStopContinuousExportResponse pResponseStatus_ =
  StopContinuousExportResponse'
    { startTime = Lude.Nothing,
      stopTime = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Timestamp that represents when this continuous export started collecting data.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsStartTime :: Lens.Lens' StopContinuousExportResponse (Lude.Maybe Lude.Timestamp)
srsStartTime = Lens.lens (startTime :: StopContinuousExportResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: StopContinuousExportResponse)
{-# DEPRECATED srsStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | Timestamp that represents when this continuous export was stopped.
--
-- /Note:/ Consider using 'stopTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsStopTime :: Lens.Lens' StopContinuousExportResponse (Lude.Maybe Lude.Timestamp)
srsStopTime = Lens.lens (stopTime :: StopContinuousExportResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {stopTime = a} :: StopContinuousExportResponse)
{-# DEPRECATED srsStopTime "Use generic-lens or generic-optics with 'stopTime' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StopContinuousExportResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: StopContinuousExportResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopContinuousExportResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
