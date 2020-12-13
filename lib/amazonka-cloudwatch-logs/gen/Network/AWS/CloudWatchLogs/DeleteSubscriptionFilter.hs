{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DeleteSubscriptionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified subscription filter.
module Network.AWS.CloudWatchLogs.DeleteSubscriptionFilter
  ( -- * Creating a request
    DeleteSubscriptionFilter (..),
    mkDeleteSubscriptionFilter,

    -- ** Request lenses
    dFilterName,
    dLogGroupName,

    -- * Destructuring the response
    DeleteSubscriptionFilterResponse (..),
    mkDeleteSubscriptionFilterResponse,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteSubscriptionFilter' smart constructor.
data DeleteSubscriptionFilter = DeleteSubscriptionFilter'
  { -- | The name of the subscription filter.
    filterName :: Lude.Text,
    -- | The name of the log group.
    logGroupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSubscriptionFilter' with the minimum fields required to make a request.
--
-- * 'filterName' - The name of the subscription filter.
-- * 'logGroupName' - The name of the log group.
mkDeleteSubscriptionFilter ::
  -- | 'filterName'
  Lude.Text ->
  -- | 'logGroupName'
  Lude.Text ->
  DeleteSubscriptionFilter
mkDeleteSubscriptionFilter pFilterName_ pLogGroupName_ =
  DeleteSubscriptionFilter'
    { filterName = pFilterName_,
      logGroupName = pLogGroupName_
    }

-- | The name of the subscription filter.
--
-- /Note:/ Consider using 'filterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dFilterName :: Lens.Lens' DeleteSubscriptionFilter Lude.Text
dFilterName = Lens.lens (filterName :: DeleteSubscriptionFilter -> Lude.Text) (\s a -> s {filterName = a} :: DeleteSubscriptionFilter)
{-# DEPRECATED dFilterName "Use generic-lens or generic-optics with 'filterName' instead." #-}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLogGroupName :: Lens.Lens' DeleteSubscriptionFilter Lude.Text
dLogGroupName = Lens.lens (logGroupName :: DeleteSubscriptionFilter -> Lude.Text) (\s a -> s {logGroupName = a} :: DeleteSubscriptionFilter)
{-# DEPRECATED dLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

instance Lude.AWSRequest DeleteSubscriptionFilter where
  type Rs DeleteSubscriptionFilter = DeleteSubscriptionFilterResponse
  request = Req.postJSON cloudWatchLogsService
  response = Res.receiveNull DeleteSubscriptionFilterResponse'

instance Lude.ToHeaders DeleteSubscriptionFilter where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.DeleteSubscriptionFilter" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteSubscriptionFilter where
  toJSON DeleteSubscriptionFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("filterName" Lude..= filterName),
            Lude.Just ("logGroupName" Lude..= logGroupName)
          ]
      )

instance Lude.ToPath DeleteSubscriptionFilter where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteSubscriptionFilter where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteSubscriptionFilterResponse' smart constructor.
data DeleteSubscriptionFilterResponse = DeleteSubscriptionFilterResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSubscriptionFilterResponse' with the minimum fields required to make a request.
mkDeleteSubscriptionFilterResponse ::
  DeleteSubscriptionFilterResponse
mkDeleteSubscriptionFilterResponse =
  DeleteSubscriptionFilterResponse'
