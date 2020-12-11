{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.DeleteDataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns the DELETED status to a @DataSource@ , rendering it unusable.
--
-- After using the @DeleteDataSource@ operation, you can use the 'GetDataSource' operation to verify that the status of the @DataSource@ changed to DELETED.
-- __Caution:__ The results of the @DeleteDataSource@ operation are irreversible.
module Network.AWS.MachineLearning.DeleteDataSource
  ( -- * Creating a request
    DeleteDataSource (..),
    mkDeleteDataSource,

    -- ** Request lenses
    ddsDataSourceId,

    -- * Destructuring the response
    DeleteDataSourceResponse (..),
    mkDeleteDataSourceResponse,

    -- ** Response lenses
    ddsrsDataSourceId,
    ddsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteDataSource' smart constructor.
newtype DeleteDataSource = DeleteDataSource'
  { dataSourceId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDataSource' with the minimum fields required to make a request.
--
-- * 'dataSourceId' - A user-supplied ID that uniquely identifies the @DataSource@ .
mkDeleteDataSource ::
  -- | 'dataSourceId'
  Lude.Text ->
  DeleteDataSource
mkDeleteDataSource pDataSourceId_ =
  DeleteDataSource' {dataSourceId = pDataSourceId_}

-- | A user-supplied ID that uniquely identifies the @DataSource@ .
--
-- /Note:/ Consider using 'dataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsDataSourceId :: Lens.Lens' DeleteDataSource Lude.Text
ddsDataSourceId = Lens.lens (dataSourceId :: DeleteDataSource -> Lude.Text) (\s a -> s {dataSourceId = a} :: DeleteDataSource)
{-# DEPRECATED ddsDataSourceId "Use generic-lens or generic-optics with 'dataSourceId' instead." #-}

instance Lude.AWSRequest DeleteDataSource where
  type Rs DeleteDataSource = DeleteDataSourceResponse
  request = Req.postJSON machineLearningService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteDataSourceResponse'
            Lude.<$> (x Lude..?> "DataSourceId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDataSource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonML_20141212.DeleteDataSource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteDataSource where
  toJSON DeleteDataSource' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("DataSourceId" Lude..= dataSourceId)])

instance Lude.ToPath DeleteDataSource where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDataSource where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @DeleteDataSource@ operation.
--
-- /See:/ 'mkDeleteDataSourceResponse' smart constructor.
data DeleteDataSourceResponse = DeleteDataSourceResponse'
  { dataSourceId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDataSourceResponse' with the minimum fields required to make a request.
--
-- * 'dataSourceId' - A user-supplied ID that uniquely identifies the @DataSource@ . This value should be identical to the value of the @DataSourceID@ in the request.
-- * 'responseStatus' - The response status code.
mkDeleteDataSourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDataSourceResponse
mkDeleteDataSourceResponse pResponseStatus_ =
  DeleteDataSourceResponse'
    { dataSourceId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A user-supplied ID that uniquely identifies the @DataSource@ . This value should be identical to the value of the @DataSourceID@ in the request.
--
-- /Note:/ Consider using 'dataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsrsDataSourceId :: Lens.Lens' DeleteDataSourceResponse (Lude.Maybe Lude.Text)
ddsrsDataSourceId = Lens.lens (dataSourceId :: DeleteDataSourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {dataSourceId = a} :: DeleteDataSourceResponse)
{-# DEPRECATED ddsrsDataSourceId "Use generic-lens or generic-optics with 'dataSourceId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsrsResponseStatus :: Lens.Lens' DeleteDataSourceResponse Lude.Int
ddsrsResponseStatus = Lens.lens (responseStatus :: DeleteDataSourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDataSourceResponse)
{-# DEPRECATED ddsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
