{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.DeleteDatasetContent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the content of the specified dataset.
module Network.AWS.IoTAnalytics.DeleteDatasetContent
  ( -- * Creating a request
    DeleteDatasetContent (..),
    mkDeleteDatasetContent,

    -- ** Request lenses
    ddcVersionId,
    ddcDatasetName,

    -- * Destructuring the response
    DeleteDatasetContentResponse (..),
    mkDeleteDatasetContentResponse,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteDatasetContent' smart constructor.
data DeleteDatasetContent = DeleteDatasetContent'
  { versionId ::
      Lude.Maybe Lude.Text,
    datasetName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDatasetContent' with the minimum fields required to make a request.
--
-- * 'datasetName' - The name of the dataset whose content is deleted.
-- * 'versionId' - The version of the dataset whose content is deleted. You can also use the strings "$LATEST" or "$LATEST_SUCCEEDED" to delete the latest or latest successfully completed data set. If not specified, "$LATEST_SUCCEEDED" is the default.
mkDeleteDatasetContent ::
  -- | 'datasetName'
  Lude.Text ->
  DeleteDatasetContent
mkDeleteDatasetContent pDatasetName_ =
  DeleteDatasetContent'
    { versionId = Lude.Nothing,
      datasetName = pDatasetName_
    }

-- | The version of the dataset whose content is deleted. You can also use the strings "$LATEST" or "$LATEST_SUCCEEDED" to delete the latest or latest successfully completed data set. If not specified, "$LATEST_SUCCEEDED" is the default.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcVersionId :: Lens.Lens' DeleteDatasetContent (Lude.Maybe Lude.Text)
ddcVersionId = Lens.lens (versionId :: DeleteDatasetContent -> Lude.Maybe Lude.Text) (\s a -> s {versionId = a} :: DeleteDatasetContent)
{-# DEPRECATED ddcVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The name of the dataset whose content is deleted.
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcDatasetName :: Lens.Lens' DeleteDatasetContent Lude.Text
ddcDatasetName = Lens.lens (datasetName :: DeleteDatasetContent -> Lude.Text) (\s a -> s {datasetName = a} :: DeleteDatasetContent)
{-# DEPRECATED ddcDatasetName "Use generic-lens or generic-optics with 'datasetName' instead." #-}

instance Lude.AWSRequest DeleteDatasetContent where
  type Rs DeleteDatasetContent = DeleteDatasetContentResponse
  request = Req.delete ioTAnalyticsService
  response = Res.receiveNull DeleteDatasetContentResponse'

instance Lude.ToHeaders DeleteDatasetContent where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteDatasetContent where
  toPath DeleteDatasetContent' {..} =
    Lude.mconcat ["/datasets/", Lude.toBS datasetName, "/content"]

instance Lude.ToQuery DeleteDatasetContent where
  toQuery DeleteDatasetContent' {..} =
    Lude.mconcat ["versionId" Lude.=: versionId]

-- | /See:/ 'mkDeleteDatasetContentResponse' smart constructor.
data DeleteDatasetContentResponse = DeleteDatasetContentResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDatasetContentResponse' with the minimum fields required to make a request.
mkDeleteDatasetContentResponse ::
  DeleteDatasetContentResponse
mkDeleteDatasetContentResponse = DeleteDatasetContentResponse'
