{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.DescribeDatastore
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a data store.
module Network.AWS.IoTAnalytics.DescribeDatastore
  ( -- * Creating a request
    DescribeDatastore (..),
    mkDescribeDatastore,

    -- ** Request lenses
    dIncludeStatistics,
    dDatastoreName,

    -- * Destructuring the response
    DescribeDatastoreResponse (..),
    mkDescribeDatastoreResponse,

    -- ** Response lenses
    drsDatastore,
    drsStatistics,
    drsResponseStatus,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeDatastore' smart constructor.
data DescribeDatastore = DescribeDatastore'
  { includeStatistics ::
      Lude.Maybe Lude.Bool,
    datastoreName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDatastore' with the minimum fields required to make a request.
--
-- * 'datastoreName' - The name of the data store
-- * 'includeStatistics' - If true, additional statistical information about the data store is included in the response. This feature cannot be used with a data store whose S3 storage is customer-managed.
mkDescribeDatastore ::
  -- | 'datastoreName'
  Lude.Text ->
  DescribeDatastore
mkDescribeDatastore pDatastoreName_ =
  DescribeDatastore'
    { includeStatistics = Lude.Nothing,
      datastoreName = pDatastoreName_
    }

-- | If true, additional statistical information about the data store is included in the response. This feature cannot be used with a data store whose S3 storage is customer-managed.
--
-- /Note:/ Consider using 'includeStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dIncludeStatistics :: Lens.Lens' DescribeDatastore (Lude.Maybe Lude.Bool)
dIncludeStatistics = Lens.lens (includeStatistics :: DescribeDatastore -> Lude.Maybe Lude.Bool) (\s a -> s {includeStatistics = a} :: DescribeDatastore)
{-# DEPRECATED dIncludeStatistics "Use generic-lens or generic-optics with 'includeStatistics' instead." #-}

-- | The name of the data store
--
-- /Note:/ Consider using 'datastoreName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDatastoreName :: Lens.Lens' DescribeDatastore Lude.Text
dDatastoreName = Lens.lens (datastoreName :: DescribeDatastore -> Lude.Text) (\s a -> s {datastoreName = a} :: DescribeDatastore)
{-# DEPRECATED dDatastoreName "Use generic-lens or generic-optics with 'datastoreName' instead." #-}

instance Lude.AWSRequest DescribeDatastore where
  type Rs DescribeDatastore = DescribeDatastoreResponse
  request = Req.get ioTAnalyticsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeDatastoreResponse'
            Lude.<$> (x Lude..?> "datastore")
            Lude.<*> (x Lude..?> "statistics")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDatastore where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeDatastore where
  toPath DescribeDatastore' {..} =
    Lude.mconcat ["/datastores/", Lude.toBS datastoreName]

instance Lude.ToQuery DescribeDatastore where
  toQuery DescribeDatastore' {..} =
    Lude.mconcat ["includeStatistics" Lude.=: includeStatistics]

-- | /See:/ 'mkDescribeDatastoreResponse' smart constructor.
data DescribeDatastoreResponse = DescribeDatastoreResponse'
  { datastore ::
      Lude.Maybe Datastore,
    statistics ::
      Lude.Maybe DatastoreStatistics,
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

-- | Creates a value of 'DescribeDatastoreResponse' with the minimum fields required to make a request.
--
-- * 'datastore' - Information about the data store.
-- * 'responseStatus' - The response status code.
-- * 'statistics' - Additional statistical information about the data store. Included if the @includeStatistics@ parameter is set to @true@ in the request.
mkDescribeDatastoreResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDatastoreResponse
mkDescribeDatastoreResponse pResponseStatus_ =
  DescribeDatastoreResponse'
    { datastore = Lude.Nothing,
      statistics = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the data store.
--
-- /Note:/ Consider using 'datastore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDatastore :: Lens.Lens' DescribeDatastoreResponse (Lude.Maybe Datastore)
drsDatastore = Lens.lens (datastore :: DescribeDatastoreResponse -> Lude.Maybe Datastore) (\s a -> s {datastore = a} :: DescribeDatastoreResponse)
{-# DEPRECATED drsDatastore "Use generic-lens or generic-optics with 'datastore' instead." #-}

-- | Additional statistical information about the data store. Included if the @includeStatistics@ parameter is set to @true@ in the request.
--
-- /Note:/ Consider using 'statistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsStatistics :: Lens.Lens' DescribeDatastoreResponse (Lude.Maybe DatastoreStatistics)
drsStatistics = Lens.lens (statistics :: DescribeDatastoreResponse -> Lude.Maybe DatastoreStatistics) (\s a -> s {statistics = a} :: DescribeDatastoreResponse)
{-# DEPRECATED drsStatistics "Use generic-lens or generic-optics with 'statistics' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeDatastoreResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeDatastoreResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDatastoreResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
