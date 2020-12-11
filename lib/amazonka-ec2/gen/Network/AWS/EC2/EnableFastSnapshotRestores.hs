{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.EnableFastSnapshotRestores
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables fast snapshot restores for the specified snapshots in the specified Availability Zones.
--
-- You get the full benefit of fast snapshot restores after they enter the @enabled@ state. To get the current state of fast snapshot restores, use 'DescribeFastSnapshotRestores' . To disable fast snapshot restores, use 'DisableFastSnapshotRestores' .
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-fast-snapshot-restore.html Amazon EBS fast snapshot restore> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.EnableFastSnapshotRestores
  ( -- * Creating a request
    EnableFastSnapshotRestores (..),
    mkEnableFastSnapshotRestores,

    -- ** Request lenses
    efsrDryRun,
    efsrAvailabilityZones,
    efsrSourceSnapshotIds,

    -- * Destructuring the response
    EnableFastSnapshotRestoresResponse (..),
    mkEnableFastSnapshotRestoresResponse,

    -- ** Response lenses
    efsrrsUnsuccessful,
    efsrrsSuccessful,
    efsrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkEnableFastSnapshotRestores' smart constructor.
data EnableFastSnapshotRestores = EnableFastSnapshotRestores'
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

-- | Creates a value of 'EnableFastSnapshotRestores' with the minimum fields required to make a request.
--
-- * 'availabilityZones' - One or more Availability Zones. For example, @us-east-2a@ .
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'sourceSnapshotIds' - The IDs of one or more snapshots. For example, @snap-1234567890abcdef0@ . You can specify a snapshot that was shared with you from another AWS account.
mkEnableFastSnapshotRestores ::
  EnableFastSnapshotRestores
mkEnableFastSnapshotRestores =
  EnableFastSnapshotRestores'
    { dryRun = Lude.Nothing,
      availabilityZones = Lude.mempty,
      sourceSnapshotIds = Lude.mempty
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrDryRun :: Lens.Lens' EnableFastSnapshotRestores (Lude.Maybe Lude.Bool)
efsrDryRun = Lens.lens (dryRun :: EnableFastSnapshotRestores -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: EnableFastSnapshotRestores)
{-# DEPRECATED efsrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | One or more Availability Zones. For example, @us-east-2a@ .
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrAvailabilityZones :: Lens.Lens' EnableFastSnapshotRestores [Lude.Text]
efsrAvailabilityZones = Lens.lens (availabilityZones :: EnableFastSnapshotRestores -> [Lude.Text]) (\s a -> s {availabilityZones = a} :: EnableFastSnapshotRestores)
{-# DEPRECATED efsrAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The IDs of one or more snapshots. For example, @snap-1234567890abcdef0@ . You can specify a snapshot that was shared with you from another AWS account.
--
-- /Note:/ Consider using 'sourceSnapshotIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrSourceSnapshotIds :: Lens.Lens' EnableFastSnapshotRestores [Lude.Text]
efsrSourceSnapshotIds = Lens.lens (sourceSnapshotIds :: EnableFastSnapshotRestores -> [Lude.Text]) (\s a -> s {sourceSnapshotIds = a} :: EnableFastSnapshotRestores)
{-# DEPRECATED efsrSourceSnapshotIds "Use generic-lens or generic-optics with 'sourceSnapshotIds' instead." #-}

instance Lude.AWSRequest EnableFastSnapshotRestores where
  type
    Rs EnableFastSnapshotRestores =
      EnableFastSnapshotRestoresResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          EnableFastSnapshotRestoresResponse'
            Lude.<$> ( x Lude..@? "unsuccessful" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> ( x Lude..@? "successful" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders EnableFastSnapshotRestores where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath EnableFastSnapshotRestores where
  toPath = Lude.const "/"

instance Lude.ToQuery EnableFastSnapshotRestores where
  toQuery EnableFastSnapshotRestores' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("EnableFastSnapshotRestores" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        Lude.toQueryList "AvailabilityZone" availabilityZones,
        Lude.toQueryList "SourceSnapshotId" sourceSnapshotIds
      ]

-- | /See:/ 'mkEnableFastSnapshotRestoresResponse' smart constructor.
data EnableFastSnapshotRestoresResponse = EnableFastSnapshotRestoresResponse'
  { unsuccessful ::
      Lude.Maybe
        [EnableFastSnapshotRestoreErrorItem],
    successful ::
      Lude.Maybe
        [EnableFastSnapshotRestoreSuccessItem],
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

-- | Creates a value of 'EnableFastSnapshotRestoresResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'successful' - Information about the snapshots for which fast snapshot restores were successfully enabled.
-- * 'unsuccessful' - Information about the snapshots for which fast snapshot restores could not be enabled.
mkEnableFastSnapshotRestoresResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  EnableFastSnapshotRestoresResponse
mkEnableFastSnapshotRestoresResponse pResponseStatus_ =
  EnableFastSnapshotRestoresResponse'
    { unsuccessful = Lude.Nothing,
      successful = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the snapshots for which fast snapshot restores could not be enabled.
--
-- /Note:/ Consider using 'unsuccessful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrrsUnsuccessful :: Lens.Lens' EnableFastSnapshotRestoresResponse (Lude.Maybe [EnableFastSnapshotRestoreErrorItem])
efsrrsUnsuccessful = Lens.lens (unsuccessful :: EnableFastSnapshotRestoresResponse -> Lude.Maybe [EnableFastSnapshotRestoreErrorItem]) (\s a -> s {unsuccessful = a} :: EnableFastSnapshotRestoresResponse)
{-# DEPRECATED efsrrsUnsuccessful "Use generic-lens or generic-optics with 'unsuccessful' instead." #-}

-- | Information about the snapshots for which fast snapshot restores were successfully enabled.
--
-- /Note:/ Consider using 'successful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrrsSuccessful :: Lens.Lens' EnableFastSnapshotRestoresResponse (Lude.Maybe [EnableFastSnapshotRestoreSuccessItem])
efsrrsSuccessful = Lens.lens (successful :: EnableFastSnapshotRestoresResponse -> Lude.Maybe [EnableFastSnapshotRestoreSuccessItem]) (\s a -> s {successful = a} :: EnableFastSnapshotRestoresResponse)
{-# DEPRECATED efsrrsSuccessful "Use generic-lens or generic-optics with 'successful' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrrsResponseStatus :: Lens.Lens' EnableFastSnapshotRestoresResponse Lude.Int
efsrrsResponseStatus = Lens.lens (responseStatus :: EnableFastSnapshotRestoresResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: EnableFastSnapshotRestoresResponse)
{-# DEPRECATED efsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
