{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.CreateDatasetContent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the content of a data set by applying a @queryAction@ (a SQL query) or a @containerAction@ (executing a containerized application).
module Network.AWS.IoTAnalytics.CreateDatasetContent
  ( -- * Creating a request
    CreateDatasetContent (..),
    mkCreateDatasetContent,

    -- ** Request lenses
    cdcVersionId,
    cdcDatasetName,

    -- * Destructuring the response
    CreateDatasetContentResponse (..),
    mkCreateDatasetContentResponse,

    -- ** Response lenses
    cdcrsVersionId,
    cdcrsResponseStatus,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDatasetContent' smart constructor.
data CreateDatasetContent = CreateDatasetContent'
  { -- | The version ID of the dataset content. To specify @versionId@ for a dataset content, the dataset must use a <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer> filter.
    versionId :: Lude.Maybe Lude.Text,
    -- | The name of the dataset.
    datasetName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDatasetContent' with the minimum fields required to make a request.
--
-- * 'versionId' - The version ID of the dataset content. To specify @versionId@ for a dataset content, the dataset must use a <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer> filter.
-- * 'datasetName' - The name of the dataset.
mkCreateDatasetContent ::
  -- | 'datasetName'
  Lude.Text ->
  CreateDatasetContent
mkCreateDatasetContent pDatasetName_ =
  CreateDatasetContent'
    { versionId = Lude.Nothing,
      datasetName = pDatasetName_
    }

-- | The version ID of the dataset content. To specify @versionId@ for a dataset content, the dataset must use a <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTimer> filter.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcVersionId :: Lens.Lens' CreateDatasetContent (Lude.Maybe Lude.Text)
cdcVersionId = Lens.lens (versionId :: CreateDatasetContent -> Lude.Maybe Lude.Text) (\s a -> s {versionId = a} :: CreateDatasetContent)
{-# DEPRECATED cdcVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The name of the dataset.
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcDatasetName :: Lens.Lens' CreateDatasetContent Lude.Text
cdcDatasetName = Lens.lens (datasetName :: CreateDatasetContent -> Lude.Text) (\s a -> s {datasetName = a} :: CreateDatasetContent)
{-# DEPRECATED cdcDatasetName "Use generic-lens or generic-optics with 'datasetName' instead." #-}

instance Lude.AWSRequest CreateDatasetContent where
  type Rs CreateDatasetContent = CreateDatasetContentResponse
  request = Req.postJSON ioTAnalyticsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDatasetContentResponse'
            Lude.<$> (x Lude..?> "versionId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDatasetContent where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateDatasetContent where
  toJSON CreateDatasetContent' {..} =
    Lude.object
      (Lude.catMaybes [("versionId" Lude..=) Lude.<$> versionId])

instance Lude.ToPath CreateDatasetContent where
  toPath CreateDatasetContent' {..} =
    Lude.mconcat ["/datasets/", Lude.toBS datasetName, "/content"]

instance Lude.ToQuery CreateDatasetContent where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateDatasetContentResponse' smart constructor.
data CreateDatasetContentResponse = CreateDatasetContentResponse'
  { -- | The version ID of the dataset contents that are being created.
    versionId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDatasetContentResponse' with the minimum fields required to make a request.
--
-- * 'versionId' - The version ID of the dataset contents that are being created.
-- * 'responseStatus' - The response status code.
mkCreateDatasetContentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDatasetContentResponse
mkCreateDatasetContentResponse pResponseStatus_ =
  CreateDatasetContentResponse'
    { versionId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The version ID of the dataset contents that are being created.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcrsVersionId :: Lens.Lens' CreateDatasetContentResponse (Lude.Maybe Lude.Text)
cdcrsVersionId = Lens.lens (versionId :: CreateDatasetContentResponse -> Lude.Maybe Lude.Text) (\s a -> s {versionId = a} :: CreateDatasetContentResponse)
{-# DEPRECATED cdcrsVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcrsResponseStatus :: Lens.Lens' CreateDatasetContentResponse Lude.Int
cdcrsResponseStatus = Lens.lens (responseStatus :: CreateDatasetContentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDatasetContentResponse)
{-# DEPRECATED cdcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
