{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.DescribeDataset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a dataset.
module Network.AWS.IoTAnalytics.DescribeDataset
  ( -- * Creating a request
    DescribeDataset (..),
    mkDescribeDataset,

    -- ** Request lenses
    dDatasetName,

    -- * Destructuring the response
    DescribeDatasetResponse (..),
    mkDescribeDatasetResponse,

    -- ** Response lenses
    ddrsDataset,
    ddrsResponseStatus,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeDataset' smart constructor.
newtype DescribeDataset = DescribeDataset'
  { -- | The name of the data set whose information is retrieved.
    datasetName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDataset' with the minimum fields required to make a request.
--
-- * 'datasetName' - The name of the data set whose information is retrieved.
mkDescribeDataset ::
  -- | 'datasetName'
  Lude.Text ->
  DescribeDataset
mkDescribeDataset pDatasetName_ =
  DescribeDataset' {datasetName = pDatasetName_}

-- | The name of the data set whose information is retrieved.
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDatasetName :: Lens.Lens' DescribeDataset Lude.Text
dDatasetName = Lens.lens (datasetName :: DescribeDataset -> Lude.Text) (\s a -> s {datasetName = a} :: DescribeDataset)
{-# DEPRECATED dDatasetName "Use generic-lens or generic-optics with 'datasetName' instead." #-}

instance Lude.AWSRequest DescribeDataset where
  type Rs DescribeDataset = DescribeDatasetResponse
  request = Req.get ioTAnalyticsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeDatasetResponse'
            Lude.<$> (x Lude..?> "dataset") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDataset where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeDataset where
  toPath DescribeDataset' {..} =
    Lude.mconcat ["/datasets/", Lude.toBS datasetName]

instance Lude.ToQuery DescribeDataset where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeDatasetResponse' smart constructor.
data DescribeDatasetResponse = DescribeDatasetResponse'
  { -- | An object that contains information about the data set.
    dataset :: Lude.Maybe Dataset,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDatasetResponse' with the minimum fields required to make a request.
--
-- * 'dataset' - An object that contains information about the data set.
-- * 'responseStatus' - The response status code.
mkDescribeDatasetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDatasetResponse
mkDescribeDatasetResponse pResponseStatus_ =
  DescribeDatasetResponse'
    { dataset = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that contains information about the data set.
--
-- /Note:/ Consider using 'dataset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsDataset :: Lens.Lens' DescribeDatasetResponse (Lude.Maybe Dataset)
ddrsDataset = Lens.lens (dataset :: DescribeDatasetResponse -> Lude.Maybe Dataset) (\s a -> s {dataset = a} :: DescribeDatasetResponse)
{-# DEPRECATED ddrsDataset "Use generic-lens or generic-optics with 'dataset' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsResponseStatus :: Lens.Lens' DescribeDatasetResponse Lude.Int
ddrsResponseStatus = Lens.lens (responseStatus :: DescribeDatasetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDatasetResponse)
{-# DEPRECATED ddrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
