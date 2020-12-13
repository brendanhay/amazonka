{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.UpdateDataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the @DataSourceName@ of a @DataSource@ .
--
-- You can use the @GetDataSource@ operation to view the contents of the updated data element.
module Network.AWS.MachineLearning.UpdateDataSource
  ( -- * Creating a request
    UpdateDataSource (..),
    mkUpdateDataSource,

    -- ** Request lenses
    udsDataSourceName,
    udsDataSourceId,

    -- * Destructuring the response
    UpdateDataSourceResponse (..),
    mkUpdateDataSourceResponse,

    -- ** Response lenses
    udsrsDataSourceId,
    udsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateDataSource' smart constructor.
data UpdateDataSource = UpdateDataSource'
  { -- | A new user-supplied name or description of the @DataSource@ that will replace the current description.
    dataSourceName :: Lude.Text,
    -- | The ID assigned to the @DataSource@ during creation.
    dataSourceId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDataSource' with the minimum fields required to make a request.
--
-- * 'dataSourceName' - A new user-supplied name or description of the @DataSource@ that will replace the current description.
-- * 'dataSourceId' - The ID assigned to the @DataSource@ during creation.
mkUpdateDataSource ::
  -- | 'dataSourceName'
  Lude.Text ->
  -- | 'dataSourceId'
  Lude.Text ->
  UpdateDataSource
mkUpdateDataSource pDataSourceName_ pDataSourceId_ =
  UpdateDataSource'
    { dataSourceName = pDataSourceName_,
      dataSourceId = pDataSourceId_
    }

-- | A new user-supplied name or description of the @DataSource@ that will replace the current description.
--
-- /Note:/ Consider using 'dataSourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsDataSourceName :: Lens.Lens' UpdateDataSource Lude.Text
udsDataSourceName = Lens.lens (dataSourceName :: UpdateDataSource -> Lude.Text) (\s a -> s {dataSourceName = a} :: UpdateDataSource)
{-# DEPRECATED udsDataSourceName "Use generic-lens or generic-optics with 'dataSourceName' instead." #-}

-- | The ID assigned to the @DataSource@ during creation.
--
-- /Note:/ Consider using 'dataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsDataSourceId :: Lens.Lens' UpdateDataSource Lude.Text
udsDataSourceId = Lens.lens (dataSourceId :: UpdateDataSource -> Lude.Text) (\s a -> s {dataSourceId = a} :: UpdateDataSource)
{-# DEPRECATED udsDataSourceId "Use generic-lens or generic-optics with 'dataSourceId' instead." #-}

instance Lude.AWSRequest UpdateDataSource where
  type Rs UpdateDataSource = UpdateDataSourceResponse
  request = Req.postJSON machineLearningService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateDataSourceResponse'
            Lude.<$> (x Lude..?> "DataSourceId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateDataSource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonML_20141212.UpdateDataSource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateDataSource where
  toJSON UpdateDataSource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DataSourceName" Lude..= dataSourceName),
            Lude.Just ("DataSourceId" Lude..= dataSourceId)
          ]
      )

instance Lude.ToPath UpdateDataSource where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateDataSource where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of an @UpdateDataSource@ operation.
--
-- You can see the updated content by using the @GetBatchPrediction@ operation.
--
-- /See:/ 'mkUpdateDataSourceResponse' smart constructor.
data UpdateDataSourceResponse = UpdateDataSourceResponse'
  { -- | The ID assigned to the @DataSource@ during creation. This value should be identical to the value of the @DataSourceID@ in the request.
    dataSourceId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDataSourceResponse' with the minimum fields required to make a request.
--
-- * 'dataSourceId' - The ID assigned to the @DataSource@ during creation. This value should be identical to the value of the @DataSourceID@ in the request.
-- * 'responseStatus' - The response status code.
mkUpdateDataSourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateDataSourceResponse
mkUpdateDataSourceResponse pResponseStatus_ =
  UpdateDataSourceResponse'
    { dataSourceId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID assigned to the @DataSource@ during creation. This value should be identical to the value of the @DataSourceID@ in the request.
--
-- /Note:/ Consider using 'dataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsrsDataSourceId :: Lens.Lens' UpdateDataSourceResponse (Lude.Maybe Lude.Text)
udsrsDataSourceId = Lens.lens (dataSourceId :: UpdateDataSourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {dataSourceId = a} :: UpdateDataSourceResponse)
{-# DEPRECATED udsrsDataSourceId "Use generic-lens or generic-optics with 'dataSourceId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsrsResponseStatus :: Lens.Lens' UpdateDataSourceResponse Lude.Int
udsrsResponseStatus = Lens.lens (responseStatus :: UpdateDataSourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateDataSourceResponse)
{-# DEPRECATED udsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
