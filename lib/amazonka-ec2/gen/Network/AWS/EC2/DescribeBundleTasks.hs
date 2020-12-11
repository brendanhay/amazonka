{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeBundleTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified bundle tasks or all of your bundle tasks.
module Network.AWS.EC2.DescribeBundleTasks
  ( -- * Creating a request
    DescribeBundleTasks (..),
    mkDescribeBundleTasks,

    -- ** Request lenses
    dbtBundleIds,
    dbtFilters,
    dbtDryRun,

    -- * Destructuring the response
    DescribeBundleTasksResponse (..),
    mkDescribeBundleTasksResponse,

    -- ** Response lenses
    dbtrsBundleTasks,
    dbtrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeBundleTasks' smart constructor.
data DescribeBundleTasks = DescribeBundleTasks'
  { bundleIds ::
      Lude.Maybe [Lude.Text],
    filters :: Lude.Maybe [Filter],
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeBundleTasks' with the minimum fields required to make a request.
--
-- * 'bundleIds' - The bundle task IDs.
--
-- Default: Describes all your bundle tasks.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - The filters.
--
--
--     * @bundle-id@ - The ID of the bundle task.
--
--
--     * @error-code@ - If the task failed, the error code returned.
--
--
--     * @error-message@ - If the task failed, the error message returned.
--
--
--     * @instance-id@ - The ID of the instance.
--
--
--     * @progress@ - The level of task completion, as a percentage (for example, 20%).
--
--
--     * @s3-bucket@ - The Amazon S3 bucket to store the AMI.
--
--
--     * @s3-prefix@ - The beginning of the AMI name.
--
--
--     * @start-time@ - The time the task started (for example, 2013-09-15T17:15:20.000Z).
--
--
--     * @state@ - The state of the task (@pending@ | @waiting-for-shutdown@ | @bundling@ | @storing@ | @cancelling@ | @complete@ | @failed@ ).
--
--
--     * @update-time@ - The time of the most recent update for the task.
mkDescribeBundleTasks ::
  DescribeBundleTasks
mkDescribeBundleTasks =
  DescribeBundleTasks'
    { bundleIds = Lude.Nothing,
      filters = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The bundle task IDs.
--
-- Default: Describes all your bundle tasks.
--
-- /Note:/ Consider using 'bundleIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbtBundleIds :: Lens.Lens' DescribeBundleTasks (Lude.Maybe [Lude.Text])
dbtBundleIds = Lens.lens (bundleIds :: DescribeBundleTasks -> Lude.Maybe [Lude.Text]) (\s a -> s {bundleIds = a} :: DescribeBundleTasks)
{-# DEPRECATED dbtBundleIds "Use generic-lens or generic-optics with 'bundleIds' instead." #-}

-- | The filters.
--
--
--     * @bundle-id@ - The ID of the bundle task.
--
--
--     * @error-code@ - If the task failed, the error code returned.
--
--
--     * @error-message@ - If the task failed, the error message returned.
--
--
--     * @instance-id@ - The ID of the instance.
--
--
--     * @progress@ - The level of task completion, as a percentage (for example, 20%).
--
--
--     * @s3-bucket@ - The Amazon S3 bucket to store the AMI.
--
--
--     * @s3-prefix@ - The beginning of the AMI name.
--
--
--     * @start-time@ - The time the task started (for example, 2013-09-15T17:15:20.000Z).
--
--
--     * @state@ - The state of the task (@pending@ | @waiting-for-shutdown@ | @bundling@ | @storing@ | @cancelling@ | @complete@ | @failed@ ).
--
--
--     * @update-time@ - The time of the most recent update for the task.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbtFilters :: Lens.Lens' DescribeBundleTasks (Lude.Maybe [Filter])
dbtFilters = Lens.lens (filters :: DescribeBundleTasks -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeBundleTasks)
{-# DEPRECATED dbtFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbtDryRun :: Lens.Lens' DescribeBundleTasks (Lude.Maybe Lude.Bool)
dbtDryRun = Lens.lens (dryRun :: DescribeBundleTasks -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeBundleTasks)
{-# DEPRECATED dbtDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DescribeBundleTasks where
  type Rs DescribeBundleTasks = DescribeBundleTasksResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeBundleTasksResponse'
            Lude.<$> ( x Lude..@? "bundleInstanceTasksSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeBundleTasks where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeBundleTasks where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeBundleTasks where
  toQuery DescribeBundleTasks' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeBundleTasks" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "BundleId" Lude.<$> bundleIds),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDescribeBundleTasksResponse' smart constructor.
data DescribeBundleTasksResponse = DescribeBundleTasksResponse'
  { bundleTasks ::
      Lude.Maybe [BundleTask],
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

-- | Creates a value of 'DescribeBundleTasksResponse' with the minimum fields required to make a request.
--
-- * 'bundleTasks' - Information about the bundle tasks.
-- * 'responseStatus' - The response status code.
mkDescribeBundleTasksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeBundleTasksResponse
mkDescribeBundleTasksResponse pResponseStatus_ =
  DescribeBundleTasksResponse'
    { bundleTasks = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the bundle tasks.
--
-- /Note:/ Consider using 'bundleTasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbtrsBundleTasks :: Lens.Lens' DescribeBundleTasksResponse (Lude.Maybe [BundleTask])
dbtrsBundleTasks = Lens.lens (bundleTasks :: DescribeBundleTasksResponse -> Lude.Maybe [BundleTask]) (\s a -> s {bundleTasks = a} :: DescribeBundleTasksResponse)
{-# DEPRECATED dbtrsBundleTasks "Use generic-lens or generic-optics with 'bundleTasks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbtrsResponseStatus :: Lens.Lens' DescribeBundleTasksResponse Lude.Int
dbtrsResponseStatus = Lens.lens (responseStatus :: DescribeBundleTasksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeBundleTasksResponse)
{-# DEPRECATED dbtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
