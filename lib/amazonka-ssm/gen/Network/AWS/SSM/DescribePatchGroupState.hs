{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribePatchGroupState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns high-level aggregated patch compliance state for a patch group.
module Network.AWS.SSM.DescribePatchGroupState
  ( -- * Creating a request
    DescribePatchGroupState (..),
    mkDescribePatchGroupState,

    -- ** Request lenses
    dpgsPatchGroup,

    -- * Destructuring the response
    DescribePatchGroupStateResponse (..),
    mkDescribePatchGroupStateResponse,

    -- ** Response lenses
    dpgsrsInstancesWithMissingPatches,
    dpgsrsInstancesWithInstalledOtherPatches,
    dpgsrsInstancesWithNotApplicablePatches,
    dpgsrsInstancesWithInstalledPatches,
    dpgsrsInstancesWithInstalledRejectedPatches,
    dpgsrsInstancesWithInstalledPendingRebootPatches,
    dpgsrsInstancesWithUnreportedNotApplicablePatches,
    dpgsrsInstances,
    dpgsrsInstancesWithFailedPatches,
    dpgsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDescribePatchGroupState' smart constructor.
newtype DescribePatchGroupState = DescribePatchGroupState'
  { patchGroup ::
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

-- | Creates a value of 'DescribePatchGroupState' with the minimum fields required to make a request.
--
-- * 'patchGroup' - The name of the patch group whose patch snapshot should be retrieved.
mkDescribePatchGroupState ::
  -- | 'patchGroup'
  Lude.Text ->
  DescribePatchGroupState
mkDescribePatchGroupState pPatchGroup_ =
  DescribePatchGroupState' {patchGroup = pPatchGroup_}

-- | The name of the patch group whose patch snapshot should be retrieved.
--
-- /Note:/ Consider using 'patchGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsPatchGroup :: Lens.Lens' DescribePatchGroupState Lude.Text
dpgsPatchGroup = Lens.lens (patchGroup :: DescribePatchGroupState -> Lude.Text) (\s a -> s {patchGroup = a} :: DescribePatchGroupState)
{-# DEPRECATED dpgsPatchGroup "Use generic-lens or generic-optics with 'patchGroup' instead." #-}

instance Lude.AWSRequest DescribePatchGroupState where
  type Rs DescribePatchGroupState = DescribePatchGroupStateResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribePatchGroupStateResponse'
            Lude.<$> (x Lude..?> "InstancesWithMissingPatches")
            Lude.<*> (x Lude..?> "InstancesWithInstalledOtherPatches")
            Lude.<*> (x Lude..?> "InstancesWithNotApplicablePatches")
            Lude.<*> (x Lude..?> "InstancesWithInstalledPatches")
            Lude.<*> (x Lude..?> "InstancesWithInstalledRejectedPatches")
            Lude.<*> (x Lude..?> "InstancesWithInstalledPendingRebootPatches")
            Lude.<*> (x Lude..?> "InstancesWithUnreportedNotApplicablePatches")
            Lude.<*> (x Lude..?> "Instances")
            Lude.<*> (x Lude..?> "InstancesWithFailedPatches")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribePatchGroupState where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DescribePatchGroupState" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribePatchGroupState where
  toJSON DescribePatchGroupState' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("PatchGroup" Lude..= patchGroup)])

instance Lude.ToPath DescribePatchGroupState where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribePatchGroupState where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribePatchGroupStateResponse' smart constructor.
data DescribePatchGroupStateResponse = DescribePatchGroupStateResponse'
  { instancesWithMissingPatches ::
      Lude.Maybe Lude.Int,
    instancesWithInstalledOtherPatches ::
      Lude.Maybe Lude.Int,
    instancesWithNotApplicablePatches ::
      Lude.Maybe Lude.Int,
    instancesWithInstalledPatches ::
      Lude.Maybe Lude.Int,
    instancesWithInstalledRejectedPatches ::
      Lude.Maybe Lude.Int,
    instancesWithInstalledPendingRebootPatches ::
      Lude.Maybe Lude.Int,
    instancesWithUnreportedNotApplicablePatches ::
      Lude.Maybe Lude.Int,
    instances ::
      Lude.Maybe Lude.Int,
    instancesWithFailedPatches ::
      Lude.Maybe Lude.Int,
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

-- | Creates a value of 'DescribePatchGroupStateResponse' with the minimum fields required to make a request.
--
-- * 'instances' - The number of instances in the patch group.
-- * 'instancesWithFailedPatches' - The number of instances with patches from the patch baseline that failed to install.
-- * 'instancesWithInstalledOtherPatches' - The number of instances with patches installed that aren't defined in the patch baseline.
-- * 'instancesWithInstalledPatches' - The number of instances with installed patches.
-- * 'instancesWithInstalledPendingRebootPatches' - The number of instances with patches installed by Patch Manager that have not been rebooted after the patch installation. The status of these instances is NON_COMPLIANT.
-- * 'instancesWithInstalledRejectedPatches' - The number of instances with patches installed that are specified in a RejectedPatches list. Patches with a status of /INSTALLED_REJECTED/ were typically installed before they were added to a RejectedPatches list.
-- * 'instancesWithMissingPatches' - The number of instances with missing patches from the patch baseline.
-- * 'instancesWithNotApplicablePatches' - The number of instances with patches that aren't applicable.
-- * 'instancesWithUnreportedNotApplicablePatches' - The number of instances with @NotApplicable@ patches beyond the supported limit, which are not reported by name to Systems Manager Inventory.
-- * 'responseStatus' - The response status code.
mkDescribePatchGroupStateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribePatchGroupStateResponse
mkDescribePatchGroupStateResponse pResponseStatus_ =
  DescribePatchGroupStateResponse'
    { instancesWithMissingPatches =
        Lude.Nothing,
      instancesWithInstalledOtherPatches = Lude.Nothing,
      instancesWithNotApplicablePatches = Lude.Nothing,
      instancesWithInstalledPatches = Lude.Nothing,
      instancesWithInstalledRejectedPatches = Lude.Nothing,
      instancesWithInstalledPendingRebootPatches = Lude.Nothing,
      instancesWithUnreportedNotApplicablePatches = Lude.Nothing,
      instances = Lude.Nothing,
      instancesWithFailedPatches = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The number of instances with missing patches from the patch baseline.
--
-- /Note:/ Consider using 'instancesWithMissingPatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsrsInstancesWithMissingPatches :: Lens.Lens' DescribePatchGroupStateResponse (Lude.Maybe Lude.Int)
dpgsrsInstancesWithMissingPatches = Lens.lens (instancesWithMissingPatches :: DescribePatchGroupStateResponse -> Lude.Maybe Lude.Int) (\s a -> s {instancesWithMissingPatches = a} :: DescribePatchGroupStateResponse)
{-# DEPRECATED dpgsrsInstancesWithMissingPatches "Use generic-lens or generic-optics with 'instancesWithMissingPatches' instead." #-}

-- | The number of instances with patches installed that aren't defined in the patch baseline.
--
-- /Note:/ Consider using 'instancesWithInstalledOtherPatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsrsInstancesWithInstalledOtherPatches :: Lens.Lens' DescribePatchGroupStateResponse (Lude.Maybe Lude.Int)
dpgsrsInstancesWithInstalledOtherPatches = Lens.lens (instancesWithInstalledOtherPatches :: DescribePatchGroupStateResponse -> Lude.Maybe Lude.Int) (\s a -> s {instancesWithInstalledOtherPatches = a} :: DescribePatchGroupStateResponse)
{-# DEPRECATED dpgsrsInstancesWithInstalledOtherPatches "Use generic-lens or generic-optics with 'instancesWithInstalledOtherPatches' instead." #-}

-- | The number of instances with patches that aren't applicable.
--
-- /Note:/ Consider using 'instancesWithNotApplicablePatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsrsInstancesWithNotApplicablePatches :: Lens.Lens' DescribePatchGroupStateResponse (Lude.Maybe Lude.Int)
dpgsrsInstancesWithNotApplicablePatches = Lens.lens (instancesWithNotApplicablePatches :: DescribePatchGroupStateResponse -> Lude.Maybe Lude.Int) (\s a -> s {instancesWithNotApplicablePatches = a} :: DescribePatchGroupStateResponse)
{-# DEPRECATED dpgsrsInstancesWithNotApplicablePatches "Use generic-lens or generic-optics with 'instancesWithNotApplicablePatches' instead." #-}

-- | The number of instances with installed patches.
--
-- /Note:/ Consider using 'instancesWithInstalledPatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsrsInstancesWithInstalledPatches :: Lens.Lens' DescribePatchGroupStateResponse (Lude.Maybe Lude.Int)
dpgsrsInstancesWithInstalledPatches = Lens.lens (instancesWithInstalledPatches :: DescribePatchGroupStateResponse -> Lude.Maybe Lude.Int) (\s a -> s {instancesWithInstalledPatches = a} :: DescribePatchGroupStateResponse)
{-# DEPRECATED dpgsrsInstancesWithInstalledPatches "Use generic-lens or generic-optics with 'instancesWithInstalledPatches' instead." #-}

-- | The number of instances with patches installed that are specified in a RejectedPatches list. Patches with a status of /INSTALLED_REJECTED/ were typically installed before they were added to a RejectedPatches list.
--
-- /Note:/ Consider using 'instancesWithInstalledRejectedPatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsrsInstancesWithInstalledRejectedPatches :: Lens.Lens' DescribePatchGroupStateResponse (Lude.Maybe Lude.Int)
dpgsrsInstancesWithInstalledRejectedPatches = Lens.lens (instancesWithInstalledRejectedPatches :: DescribePatchGroupStateResponse -> Lude.Maybe Lude.Int) (\s a -> s {instancesWithInstalledRejectedPatches = a} :: DescribePatchGroupStateResponse)
{-# DEPRECATED dpgsrsInstancesWithInstalledRejectedPatches "Use generic-lens or generic-optics with 'instancesWithInstalledRejectedPatches' instead." #-}

-- | The number of instances with patches installed by Patch Manager that have not been rebooted after the patch installation. The status of these instances is NON_COMPLIANT.
--
-- /Note:/ Consider using 'instancesWithInstalledPendingRebootPatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsrsInstancesWithInstalledPendingRebootPatches :: Lens.Lens' DescribePatchGroupStateResponse (Lude.Maybe Lude.Int)
dpgsrsInstancesWithInstalledPendingRebootPatches = Lens.lens (instancesWithInstalledPendingRebootPatches :: DescribePatchGroupStateResponse -> Lude.Maybe Lude.Int) (\s a -> s {instancesWithInstalledPendingRebootPatches = a} :: DescribePatchGroupStateResponse)
{-# DEPRECATED dpgsrsInstancesWithInstalledPendingRebootPatches "Use generic-lens or generic-optics with 'instancesWithInstalledPendingRebootPatches' instead." #-}

-- | The number of instances with @NotApplicable@ patches beyond the supported limit, which are not reported by name to Systems Manager Inventory.
--
-- /Note:/ Consider using 'instancesWithUnreportedNotApplicablePatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsrsInstancesWithUnreportedNotApplicablePatches :: Lens.Lens' DescribePatchGroupStateResponse (Lude.Maybe Lude.Int)
dpgsrsInstancesWithUnreportedNotApplicablePatches = Lens.lens (instancesWithUnreportedNotApplicablePatches :: DescribePatchGroupStateResponse -> Lude.Maybe Lude.Int) (\s a -> s {instancesWithUnreportedNotApplicablePatches = a} :: DescribePatchGroupStateResponse)
{-# DEPRECATED dpgsrsInstancesWithUnreportedNotApplicablePatches "Use generic-lens or generic-optics with 'instancesWithUnreportedNotApplicablePatches' instead." #-}

-- | The number of instances in the patch group.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsrsInstances :: Lens.Lens' DescribePatchGroupStateResponse (Lude.Maybe Lude.Int)
dpgsrsInstances = Lens.lens (instances :: DescribePatchGroupStateResponse -> Lude.Maybe Lude.Int) (\s a -> s {instances = a} :: DescribePatchGroupStateResponse)
{-# DEPRECATED dpgsrsInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

-- | The number of instances with patches from the patch baseline that failed to install.
--
-- /Note:/ Consider using 'instancesWithFailedPatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsrsInstancesWithFailedPatches :: Lens.Lens' DescribePatchGroupStateResponse (Lude.Maybe Lude.Int)
dpgsrsInstancesWithFailedPatches = Lens.lens (instancesWithFailedPatches :: DescribePatchGroupStateResponse -> Lude.Maybe Lude.Int) (\s a -> s {instancesWithFailedPatches = a} :: DescribePatchGroupStateResponse)
{-# DEPRECATED dpgsrsInstancesWithFailedPatches "Use generic-lens or generic-optics with 'instancesWithFailedPatches' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsrsResponseStatus :: Lens.Lens' DescribePatchGroupStateResponse Lude.Int
dpgsrsResponseStatus = Lens.lens (responseStatus :: DescribePatchGroupStateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribePatchGroupStateResponse)
{-# DEPRECATED dpgsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
