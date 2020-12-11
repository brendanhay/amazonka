{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CancelBundleTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a bundling operation for an instance store-backed Windows instance.
module Network.AWS.EC2.CancelBundleTask
  ( -- * Creating a request
    CancelBundleTask (..),
    mkCancelBundleTask,

    -- ** Request lenses
    cbtDryRun,
    cbtBundleId,

    -- * Destructuring the response
    CancelBundleTaskResponse (..),
    mkCancelBundleTaskResponse,

    -- ** Response lenses
    cbtrsBundleTask,
    cbtrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for CancelBundleTask.
--
-- /See:/ 'mkCancelBundleTask' smart constructor.
data CancelBundleTask = CancelBundleTask'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    bundleId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelBundleTask' with the minimum fields required to make a request.
--
-- * 'bundleId' - The ID of the bundle task.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkCancelBundleTask ::
  -- | 'bundleId'
  Lude.Text ->
  CancelBundleTask
mkCancelBundleTask pBundleId_ =
  CancelBundleTask' {dryRun = Lude.Nothing, bundleId = pBundleId_}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbtDryRun :: Lens.Lens' CancelBundleTask (Lude.Maybe Lude.Bool)
cbtDryRun = Lens.lens (dryRun :: CancelBundleTask -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CancelBundleTask)
{-# DEPRECATED cbtDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the bundle task.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbtBundleId :: Lens.Lens' CancelBundleTask Lude.Text
cbtBundleId = Lens.lens (bundleId :: CancelBundleTask -> Lude.Text) (\s a -> s {bundleId = a} :: CancelBundleTask)
{-# DEPRECATED cbtBundleId "Use generic-lens or generic-optics with 'bundleId' instead." #-}

instance Lude.AWSRequest CancelBundleTask where
  type Rs CancelBundleTask = CancelBundleTaskResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CancelBundleTaskResponse'
            Lude.<$> (x Lude..@? "bundleInstanceTask")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelBundleTask where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CancelBundleTask where
  toPath = Lude.const "/"

instance Lude.ToQuery CancelBundleTask where
  toQuery CancelBundleTask' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CancelBundleTask" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "BundleId" Lude.=: bundleId
      ]

-- | Contains the output of CancelBundleTask.
--
-- /See:/ 'mkCancelBundleTaskResponse' smart constructor.
data CancelBundleTaskResponse = CancelBundleTaskResponse'
  { bundleTask ::
      Lude.Maybe BundleTask,
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

-- | Creates a value of 'CancelBundleTaskResponse' with the minimum fields required to make a request.
--
-- * 'bundleTask' - Information about the bundle task.
-- * 'responseStatus' - The response status code.
mkCancelBundleTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CancelBundleTaskResponse
mkCancelBundleTaskResponse pResponseStatus_ =
  CancelBundleTaskResponse'
    { bundleTask = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the bundle task.
--
-- /Note:/ Consider using 'bundleTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbtrsBundleTask :: Lens.Lens' CancelBundleTaskResponse (Lude.Maybe BundleTask)
cbtrsBundleTask = Lens.lens (bundleTask :: CancelBundleTaskResponse -> Lude.Maybe BundleTask) (\s a -> s {bundleTask = a} :: CancelBundleTaskResponse)
{-# DEPRECATED cbtrsBundleTask "Use generic-lens or generic-optics with 'bundleTask' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbtrsResponseStatus :: Lens.Lens' CancelBundleTaskResponse Lude.Int
cbtrsResponseStatus = Lens.lens (responseStatus :: CancelBundleTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelBundleTaskResponse)
{-# DEPRECATED cbtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
