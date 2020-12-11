{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.CreateExportJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an export job for an application.
module Network.AWS.Pinpoint.CreateExportJob
  ( -- * Creating a request
    CreateExportJob (..),
    mkCreateExportJob,

    -- ** Request lenses
    cejApplicationId,
    cejExportJobRequest,

    -- * Destructuring the response
    CreateExportJobResponse (..),
    mkCreateExportJobResponse,

    -- ** Response lenses
    cejrsResponseStatus,
    cejrsExportJobResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateExportJob' smart constructor.
data CreateExportJob = CreateExportJob'
  { applicationId :: Lude.Text,
    exportJobRequest :: ExportJobRequest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateExportJob' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'exportJobRequest' - Undocumented field.
mkCreateExportJob ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'exportJobRequest'
  ExportJobRequest ->
  CreateExportJob
mkCreateExportJob pApplicationId_ pExportJobRequest_ =
  CreateExportJob'
    { applicationId = pApplicationId_,
      exportJobRequest = pExportJobRequest_
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cejApplicationId :: Lens.Lens' CreateExportJob Lude.Text
cejApplicationId = Lens.lens (applicationId :: CreateExportJob -> Lude.Text) (\s a -> s {applicationId = a} :: CreateExportJob)
{-# DEPRECATED cejApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'exportJobRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cejExportJobRequest :: Lens.Lens' CreateExportJob ExportJobRequest
cejExportJobRequest = Lens.lens (exportJobRequest :: CreateExportJob -> ExportJobRequest) (\s a -> s {exportJobRequest = a} :: CreateExportJob)
{-# DEPRECATED cejExportJobRequest "Use generic-lens or generic-optics with 'exportJobRequest' instead." #-}

instance Lude.AWSRequest CreateExportJob where
  type Rs CreateExportJob = CreateExportJobResponse
  request = Req.postJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateExportJobResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders CreateExportJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateExportJob where
  toJSON CreateExportJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ExportJobRequest" Lude..= exportJobRequest)]
      )

instance Lude.ToPath CreateExportJob where
  toPath CreateExportJob' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/jobs/export"]

instance Lude.ToQuery CreateExportJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateExportJobResponse' smart constructor.
data CreateExportJobResponse = CreateExportJobResponse'
  { responseStatus ::
      Lude.Int,
    exportJobResponse :: ExportJobResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateExportJobResponse' with the minimum fields required to make a request.
--
-- * 'exportJobResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkCreateExportJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'exportJobResponse'
  ExportJobResponse ->
  CreateExportJobResponse
mkCreateExportJobResponse pResponseStatus_ pExportJobResponse_ =
  CreateExportJobResponse'
    { responseStatus = pResponseStatus_,
      exportJobResponse = pExportJobResponse_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cejrsResponseStatus :: Lens.Lens' CreateExportJobResponse Lude.Int
cejrsResponseStatus = Lens.lens (responseStatus :: CreateExportJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateExportJobResponse)
{-# DEPRECATED cejrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'exportJobResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cejrsExportJobResponse :: Lens.Lens' CreateExportJobResponse ExportJobResponse
cejrsExportJobResponse = Lens.lens (exportJobResponse :: CreateExportJobResponse -> ExportJobResponse) (\s a -> s {exportJobResponse = a} :: CreateExportJobResponse)
{-# DEPRECATED cejrsExportJobResponse "Use generic-lens or generic-optics with 'exportJobResponse' instead." #-}
