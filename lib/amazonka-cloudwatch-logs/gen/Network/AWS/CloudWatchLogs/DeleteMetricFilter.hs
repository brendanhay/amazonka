{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DeleteMetricFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified metric filter.
module Network.AWS.CloudWatchLogs.DeleteMetricFilter
  ( -- * Creating a request
    DeleteMetricFilter (..),
    mkDeleteMetricFilter,

    -- ** Request lenses
    delLogGroupName,
    delFilterName,

    -- * Destructuring the response
    DeleteMetricFilterResponse (..),
    mkDeleteMetricFilterResponse,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteMetricFilter' smart constructor.
data DeleteMetricFilter = DeleteMetricFilter'
  { logGroupName ::
      Lude.Text,
    filterName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMetricFilter' with the minimum fields required to make a request.
--
-- * 'filterName' - The name of the metric filter.
-- * 'logGroupName' - The name of the log group.
mkDeleteMetricFilter ::
  -- | 'logGroupName'
  Lude.Text ->
  -- | 'filterName'
  Lude.Text ->
  DeleteMetricFilter
mkDeleteMetricFilter pLogGroupName_ pFilterName_ =
  DeleteMetricFilter'
    { logGroupName = pLogGroupName_,
      filterName = pFilterName_
    }

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delLogGroupName :: Lens.Lens' DeleteMetricFilter Lude.Text
delLogGroupName = Lens.lens (logGroupName :: DeleteMetricFilter -> Lude.Text) (\s a -> s {logGroupName = a} :: DeleteMetricFilter)
{-# DEPRECATED delLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The name of the metric filter.
--
-- /Note:/ Consider using 'filterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delFilterName :: Lens.Lens' DeleteMetricFilter Lude.Text
delFilterName = Lens.lens (filterName :: DeleteMetricFilter -> Lude.Text) (\s a -> s {filterName = a} :: DeleteMetricFilter)
{-# DEPRECATED delFilterName "Use generic-lens or generic-optics with 'filterName' instead." #-}

instance Lude.AWSRequest DeleteMetricFilter where
  type Rs DeleteMetricFilter = DeleteMetricFilterResponse
  request = Req.postJSON cloudWatchLogsService
  response = Res.receiveNull DeleteMetricFilterResponse'

instance Lude.ToHeaders DeleteMetricFilter where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.DeleteMetricFilter" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteMetricFilter where
  toJSON DeleteMetricFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("logGroupName" Lude..= logGroupName),
            Lude.Just ("filterName" Lude..= filterName)
          ]
      )

instance Lude.ToPath DeleteMetricFilter where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteMetricFilter where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteMetricFilterResponse' smart constructor.
data DeleteMetricFilterResponse = DeleteMetricFilterResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMetricFilterResponse' with the minimum fields required to make a request.
mkDeleteMetricFilterResponse ::
  DeleteMetricFilterResponse
mkDeleteMetricFilterResponse = DeleteMetricFilterResponse'
