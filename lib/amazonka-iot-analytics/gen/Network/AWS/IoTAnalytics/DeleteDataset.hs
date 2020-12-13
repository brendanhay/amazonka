{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.DeleteDataset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified dataset.
--
-- You do not have to delete the content of the dataset before you perform this operation.
module Network.AWS.IoTAnalytics.DeleteDataset
  ( -- * Creating a request
    DeleteDataset (..),
    mkDeleteDataset,

    -- ** Request lenses
    ddDatasetName,

    -- * Destructuring the response
    DeleteDatasetResponse (..),
    mkDeleteDatasetResponse,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteDataset' smart constructor.
newtype DeleteDataset = DeleteDataset'
  { -- | The name of the data set to delete.
    datasetName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDataset' with the minimum fields required to make a request.
--
-- * 'datasetName' - The name of the data set to delete.
mkDeleteDataset ::
  -- | 'datasetName'
  Lude.Text ->
  DeleteDataset
mkDeleteDataset pDatasetName_ =
  DeleteDataset' {datasetName = pDatasetName_}

-- | The name of the data set to delete.
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDatasetName :: Lens.Lens' DeleteDataset Lude.Text
ddDatasetName = Lens.lens (datasetName :: DeleteDataset -> Lude.Text) (\s a -> s {datasetName = a} :: DeleteDataset)
{-# DEPRECATED ddDatasetName "Use generic-lens or generic-optics with 'datasetName' instead." #-}

instance Lude.AWSRequest DeleteDataset where
  type Rs DeleteDataset = DeleteDatasetResponse
  request = Req.delete ioTAnalyticsService
  response = Res.receiveNull DeleteDatasetResponse'

instance Lude.ToHeaders DeleteDataset where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteDataset where
  toPath DeleteDataset' {..} =
    Lude.mconcat ["/datasets/", Lude.toBS datasetName]

instance Lude.ToQuery DeleteDataset where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDatasetResponse' smart constructor.
data DeleteDatasetResponse = DeleteDatasetResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDatasetResponse' with the minimum fields required to make a request.
mkDeleteDatasetResponse ::
  DeleteDatasetResponse
mkDeleteDatasetResponse = DeleteDatasetResponse'
