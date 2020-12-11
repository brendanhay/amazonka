{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisableFastSnapshotRestores
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables fast snapshot restores for the specified snapshots in the specified Availability Zones.
module Network.AWS.EC2.DisableFastSnapshotRestores
  ( -- * Creating a request
    DisableFastSnapshotRestores (..),
    mkDisableFastSnapshotRestores,

    -- ** Request lenses
    dfsrsDryRun,
    dfsrsAvailabilityZones,
    dfsrsSourceSnapshotIds,

    -- * Destructuring the response
    DisableFastSnapshotRestoresResponse (..),
    mkDisableFastSnapshotRestoresResponse,

    -- ** Response lenses
    dfsrrsUnsuccessful,
    dfsrrsSuccessful,
    dfsrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisableFastSnapshotRestores' smart constructor.
data DisableFastSnapshotRestores = DisableFastSnapshotRestores'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    availabilityZones :: [Lude.Text],
    sourceSnapshotIds :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableFastSnapshotRestores' with the minimum fields required to make a request.
--
-- * 'availabilityZones' - One or more Availability Zones. For example, @us-east-2a@ .
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'sourceSnapshotIds' - The IDs of one or more snapshots. For example, @snap-1234567890abcdef0@ .
mkDisableFastSnapshotRestores ::
  DisableFastSnapshotRestores
mkDisableFastSnapshotRestores =
  DisableFastSnapshotRestores'
    { dryRun = Lude.Nothing,
      availabilityZones = Lude.mempty,
      sourceSnapshotIds = Lude.mempty
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsDryRun :: Lens.Lens' DisableFastSnapshotRestores (Lude.Maybe Lude.Bool)
dfsrsDryRun = Lens.lens (dryRun :: DisableFastSnapshotRestores -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DisableFastSnapshotRestores)
{-# DEPRECATED dfsrsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | One or more Availability Zones. For example, @us-east-2a@ .
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsAvailabilityZones :: Lens.Lens' DisableFastSnapshotRestores [Lude.Text]
dfsrsAvailabilityZones = Lens.lens (availabilityZones :: DisableFastSnapshotRestores -> [Lude.Text]) (\s a -> s {availabilityZones = a} :: DisableFastSnapshotRestores)
{-# DEPRECATED dfsrsAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The IDs of one or more snapshots. For example, @snap-1234567890abcdef0@ .
--
-- /Note:/ Consider using 'sourceSnapshotIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsSourceSnapshotIds :: Lens.Lens' DisableFastSnapshotRestores [Lude.Text]
dfsrsSourceSnapshotIds = Lens.lens (sourceSnapshotIds :: DisableFastSnapshotRestores -> [Lude.Text]) (\s a -> s {sourceSnapshotIds = a} :: DisableFastSnapshotRestores)
{-# DEPRECATED dfsrsSourceSnapshotIds "Use generic-lens or generic-optics with 'sourceSnapshotIds' instead." #-}

instance Lude.AWSRequest DisableFastSnapshotRestores where
  type
    Rs DisableFastSnapshotRestores =
      DisableFastSnapshotRestoresResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DisableFastSnapshotRestoresResponse'
            Lude.<$> ( x Lude..@? "unsuccessful" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> ( x Lude..@? "successful" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisableFastSnapshotRestores where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DisableFastSnapshotRestores where
  toPath = Lude.const "/"

instance Lude.ToQuery DisableFastSnapshotRestores where
  toQuery DisableFastSnapshotRestores' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DisableFastSnapshotRestores" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        Lude.toQueryList "AvailabilityZone" availabilityZones,
        Lude.toQueryList "SourceSnapshotId" sourceSnapshotIds
      ]

-- | /See:/ 'mkDisableFastSnapshotRestoresResponse' smart constructor.
data DisableFastSnapshotRestoresResponse = DisableFastSnapshotRestoresResponse'
  { unsuccessful ::
      Lude.Maybe
        [DisableFastSnapshotRestoreErrorItem],
    successful ::
      Lude.Maybe
        [DisableFastSnapshotRestoreSuccessItem],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableFastSnapshotRestoresResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'successful' - Information about the snapshots for which fast snapshot restores were successfully disabled.
-- * 'unsuccessful' - Information about the snapshots for which fast snapshot restores could not be disabled.
mkDisableFastSnapshotRestoresResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisableFastSnapshotRestoresResponse
mkDisableFastSnapshotRestoresResponse pResponseStatus_ =
  DisableFastSnapshotRestoresResponse'
    { unsuccessful = Lude.Nothing,
      successful = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the snapshots for which fast snapshot restores could not be disabled.
--
-- /Note:/ Consider using 'unsuccessful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrrsUnsuccessful :: Lens.Lens' DisableFastSnapshotRestoresResponse (Lude.Maybe [DisableFastSnapshotRestoreErrorItem])
dfsrrsUnsuccessful = Lens.lens (unsuccessful :: DisableFastSnapshotRestoresResponse -> Lude.Maybe [DisableFastSnapshotRestoreErrorItem]) (\s a -> s {unsuccessful = a} :: DisableFastSnapshotRestoresResponse)
{-# DEPRECATED dfsrrsUnsuccessful "Use generic-lens or generic-optics with 'unsuccessful' instead." #-}

-- | Information about the snapshots for which fast snapshot restores were successfully disabled.
--
-- /Note:/ Consider using 'successful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrrsSuccessful :: Lens.Lens' DisableFastSnapshotRestoresResponse (Lude.Maybe [DisableFastSnapshotRestoreSuccessItem])
dfsrrsSuccessful = Lens.lens (successful :: DisableFastSnapshotRestoresResponse -> Lude.Maybe [DisableFastSnapshotRestoreSuccessItem]) (\s a -> s {successful = a} :: DisableFastSnapshotRestoresResponse)
{-# DEPRECATED dfsrrsSuccessful "Use generic-lens or generic-optics with 'successful' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrrsResponseStatus :: Lens.Lens' DisableFastSnapshotRestoresResponse Lude.Int
dfsrrsResponseStatus = Lens.lens (responseStatus :: DisableFastSnapshotRestoresResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisableFastSnapshotRestoresResponse)
{-# DEPRECATED dfsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
