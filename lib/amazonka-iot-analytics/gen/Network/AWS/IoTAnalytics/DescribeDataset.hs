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
    ddDatasetName,

    -- * Destructuring the response
    DescribeDatasetResponse (..),
    mkDescribeDatasetResponse,

    -- ** Response lenses
    ddrrsDataset,
    ddrrsResponseStatus,
  )
where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeDataset' smart constructor.
newtype DescribeDataset = DescribeDataset'
  { -- | The name of the data set whose information is retrieved.
    datasetName :: Types.DatasetName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDataset' value with any optional fields omitted.
mkDescribeDataset ::
  -- | 'datasetName'
  Types.DatasetName ->
  DescribeDataset
mkDescribeDataset datasetName = DescribeDataset' {datasetName}

-- | The name of the data set whose information is retrieved.
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDatasetName :: Lens.Lens' DescribeDataset Types.DatasetName
ddDatasetName = Lens.field @"datasetName"
{-# DEPRECATED ddDatasetName "Use generic-lens or generic-optics with 'datasetName' instead." #-}

instance Core.AWSRequest DescribeDataset where
  type Rs DescribeDataset = DescribeDatasetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath ("/datasets/" Core.<> (Core.toText datasetName)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDatasetResponse'
            Core.<$> (x Core..:? "dataset") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeDatasetResponse' smart constructor.
data DescribeDatasetResponse = DescribeDatasetResponse'
  { -- | An object that contains information about the data set.
    dataset :: Core.Maybe Types.Dataset,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeDatasetResponse' value with any optional fields omitted.
mkDescribeDatasetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeDatasetResponse
mkDescribeDatasetResponse responseStatus =
  DescribeDatasetResponse' {dataset = Core.Nothing, responseStatus}

-- | An object that contains information about the data set.
--
-- /Note:/ Consider using 'dataset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsDataset :: Lens.Lens' DescribeDatasetResponse (Core.Maybe Types.Dataset)
ddrrsDataset = Lens.field @"dataset"
{-# DEPRECATED ddrrsDataset "Use generic-lens or generic-optics with 'dataset' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsResponseStatus :: Lens.Lens' DescribeDatasetResponse Core.Int
ddrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
