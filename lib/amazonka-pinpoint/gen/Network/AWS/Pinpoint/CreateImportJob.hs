{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.CreateImportJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an import job for an application.
module Network.AWS.Pinpoint.CreateImportJob
  ( -- * Creating a request
    CreateImportJob (..),
    mkCreateImportJob,

    -- ** Request lenses
    cijImportJobRequest,
    cijApplicationId,

    -- * Destructuring the response
    CreateImportJobResponse (..),
    mkCreateImportJobResponse,

    -- ** Response lenses
    cijrsImportJobResponse,
    cijrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateImportJob' smart constructor.
data CreateImportJob = CreateImportJob'
  { importJobRequest :: ImportJobRequest,
    -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateImportJob' with the minimum fields required to make a request.
--
-- * 'importJobRequest' -
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkCreateImportJob ::
  -- | 'importJobRequest'
  ImportJobRequest ->
  -- | 'applicationId'
  Lude.Text ->
  CreateImportJob
mkCreateImportJob pImportJobRequest_ pApplicationId_ =
  CreateImportJob'
    { importJobRequest = pImportJobRequest_,
      applicationId = pApplicationId_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'importJobRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cijImportJobRequest :: Lens.Lens' CreateImportJob ImportJobRequest
cijImportJobRequest = Lens.lens (importJobRequest :: CreateImportJob -> ImportJobRequest) (\s a -> s {importJobRequest = a} :: CreateImportJob)
{-# DEPRECATED cijImportJobRequest "Use generic-lens or generic-optics with 'importJobRequest' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cijApplicationId :: Lens.Lens' CreateImportJob Lude.Text
cijApplicationId = Lens.lens (applicationId :: CreateImportJob -> Lude.Text) (\s a -> s {applicationId = a} :: CreateImportJob)
{-# DEPRECATED cijApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest CreateImportJob where
  type Rs CreateImportJob = CreateImportJobResponse
  request = Req.postJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateImportJobResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateImportJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateImportJob where
  toJSON CreateImportJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ImportJobRequest" Lude..= importJobRequest)]
      )

instance Lude.ToPath CreateImportJob where
  toPath CreateImportJob' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/jobs/import"]

instance Lude.ToQuery CreateImportJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateImportJobResponse' smart constructor.
data CreateImportJobResponse = CreateImportJobResponse'
  { importJobResponse :: ImportJobResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateImportJobResponse' with the minimum fields required to make a request.
--
-- * 'importJobResponse' -
-- * 'responseStatus' - The response status code.
mkCreateImportJobResponse ::
  -- | 'importJobResponse'
  ImportJobResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  CreateImportJobResponse
mkCreateImportJobResponse pImportJobResponse_ pResponseStatus_ =
  CreateImportJobResponse'
    { importJobResponse = pImportJobResponse_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'importJobResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cijrsImportJobResponse :: Lens.Lens' CreateImportJobResponse ImportJobResponse
cijrsImportJobResponse = Lens.lens (importJobResponse :: CreateImportJobResponse -> ImportJobResponse) (\s a -> s {importJobResponse = a} :: CreateImportJobResponse)
{-# DEPRECATED cijrsImportJobResponse "Use generic-lens or generic-optics with 'importJobResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cijrsResponseStatus :: Lens.Lens' CreateImportJobResponse Lude.Int
cijrsResponseStatus = Lens.lens (responseStatus :: CreateImportJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateImportJobResponse)
{-# DEPRECATED cijrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
