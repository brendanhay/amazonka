{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.ServiceInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.ServiceInfo
  ( ServiceInfo (..),

    -- * Smart constructor
    mkServiceInfo,

    -- * Lenses
    siCount,
    siEventFirstSeen,
    siAction,
    siDetectorId,
    siServiceName,
    siUserFeedback,
    siEvidence,
    siEventLastSeen,
    siResourceRole,
    siArchived,
  )
where

import Network.AWS.GuardDuty.Types.Action
import Network.AWS.GuardDuty.Types.Evidence
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains additional information about the generated finding.
--
-- /See:/ 'mkServiceInfo' smart constructor.
data ServiceInfo = ServiceInfo'
  { count :: Lude.Maybe Lude.Int,
    eventFirstSeen :: Lude.Maybe Lude.Text,
    action :: Lude.Maybe Action,
    detectorId :: Lude.Maybe Lude.Text,
    serviceName :: Lude.Maybe Lude.Text,
    userFeedback :: Lude.Maybe Lude.Text,
    evidence :: Lude.Maybe Evidence,
    eventLastSeen :: Lude.Maybe Lude.Text,
    resourceRole :: Lude.Maybe Lude.Text,
    archived :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServiceInfo' with the minimum fields required to make a request.
--
-- * 'action' - Information about the activity that is described in a finding.
-- * 'archived' - Indicates whether this finding is archived.
-- * 'count' - The total count of the occurrences of this finding type.
-- * 'detectorId' - The detector ID for the GuardDuty service.
-- * 'eventFirstSeen' - The first-seen timestamp of the activity that prompted GuardDuty to generate this finding.
-- * 'eventLastSeen' - The last-seen timestamp of the activity that prompted GuardDuty to generate this finding.
-- * 'evidence' - An evidence object associated with the service.
-- * 'resourceRole' - The resource role information for this finding.
-- * 'serviceName' - The name of the AWS service (GuardDuty) that generated a finding.
-- * 'userFeedback' - Feedback that was submitted about the finding.
mkServiceInfo ::
  ServiceInfo
mkServiceInfo =
  ServiceInfo'
    { count = Lude.Nothing,
      eventFirstSeen = Lude.Nothing,
      action = Lude.Nothing,
      detectorId = Lude.Nothing,
      serviceName = Lude.Nothing,
      userFeedback = Lude.Nothing,
      evidence = Lude.Nothing,
      eventLastSeen = Lude.Nothing,
      resourceRole = Lude.Nothing,
      archived = Lude.Nothing
    }

-- | The total count of the occurrences of this finding type.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siCount :: Lens.Lens' ServiceInfo (Lude.Maybe Lude.Int)
siCount = Lens.lens (count :: ServiceInfo -> Lude.Maybe Lude.Int) (\s a -> s {count = a} :: ServiceInfo)
{-# DEPRECATED siCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | The first-seen timestamp of the activity that prompted GuardDuty to generate this finding.
--
-- /Note:/ Consider using 'eventFirstSeen' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siEventFirstSeen :: Lens.Lens' ServiceInfo (Lude.Maybe Lude.Text)
siEventFirstSeen = Lens.lens (eventFirstSeen :: ServiceInfo -> Lude.Maybe Lude.Text) (\s a -> s {eventFirstSeen = a} :: ServiceInfo)
{-# DEPRECATED siEventFirstSeen "Use generic-lens or generic-optics with 'eventFirstSeen' instead." #-}

-- | Information about the activity that is described in a finding.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siAction :: Lens.Lens' ServiceInfo (Lude.Maybe Action)
siAction = Lens.lens (action :: ServiceInfo -> Lude.Maybe Action) (\s a -> s {action = a} :: ServiceInfo)
{-# DEPRECATED siAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The detector ID for the GuardDuty service.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siDetectorId :: Lens.Lens' ServiceInfo (Lude.Maybe Lude.Text)
siDetectorId = Lens.lens (detectorId :: ServiceInfo -> Lude.Maybe Lude.Text) (\s a -> s {detectorId = a} :: ServiceInfo)
{-# DEPRECATED siDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The name of the AWS service (GuardDuty) that generated a finding.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siServiceName :: Lens.Lens' ServiceInfo (Lude.Maybe Lude.Text)
siServiceName = Lens.lens (serviceName :: ServiceInfo -> Lude.Maybe Lude.Text) (\s a -> s {serviceName = a} :: ServiceInfo)
{-# DEPRECATED siServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

-- | Feedback that was submitted about the finding.
--
-- /Note:/ Consider using 'userFeedback' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siUserFeedback :: Lens.Lens' ServiceInfo (Lude.Maybe Lude.Text)
siUserFeedback = Lens.lens (userFeedback :: ServiceInfo -> Lude.Maybe Lude.Text) (\s a -> s {userFeedback = a} :: ServiceInfo)
{-# DEPRECATED siUserFeedback "Use generic-lens or generic-optics with 'userFeedback' instead." #-}

-- | An evidence object associated with the service.
--
-- /Note:/ Consider using 'evidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siEvidence :: Lens.Lens' ServiceInfo (Lude.Maybe Evidence)
siEvidence = Lens.lens (evidence :: ServiceInfo -> Lude.Maybe Evidence) (\s a -> s {evidence = a} :: ServiceInfo)
{-# DEPRECATED siEvidence "Use generic-lens or generic-optics with 'evidence' instead." #-}

-- | The last-seen timestamp of the activity that prompted GuardDuty to generate this finding.
--
-- /Note:/ Consider using 'eventLastSeen' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siEventLastSeen :: Lens.Lens' ServiceInfo (Lude.Maybe Lude.Text)
siEventLastSeen = Lens.lens (eventLastSeen :: ServiceInfo -> Lude.Maybe Lude.Text) (\s a -> s {eventLastSeen = a} :: ServiceInfo)
{-# DEPRECATED siEventLastSeen "Use generic-lens or generic-optics with 'eventLastSeen' instead." #-}

-- | The resource role information for this finding.
--
-- /Note:/ Consider using 'resourceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siResourceRole :: Lens.Lens' ServiceInfo (Lude.Maybe Lude.Text)
siResourceRole = Lens.lens (resourceRole :: ServiceInfo -> Lude.Maybe Lude.Text) (\s a -> s {resourceRole = a} :: ServiceInfo)
{-# DEPRECATED siResourceRole "Use generic-lens or generic-optics with 'resourceRole' instead." #-}

-- | Indicates whether this finding is archived.
--
-- /Note:/ Consider using 'archived' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siArchived :: Lens.Lens' ServiceInfo (Lude.Maybe Lude.Bool)
siArchived = Lens.lens (archived :: ServiceInfo -> Lude.Maybe Lude.Bool) (\s a -> s {archived = a} :: ServiceInfo)
{-# DEPRECATED siArchived "Use generic-lens or generic-optics with 'archived' instead." #-}

instance Lude.FromJSON ServiceInfo where
  parseJSON =
    Lude.withObject
      "ServiceInfo"
      ( \x ->
          ServiceInfo'
            Lude.<$> (x Lude..:? "count")
            Lude.<*> (x Lude..:? "eventFirstSeen")
            Lude.<*> (x Lude..:? "action")
            Lude.<*> (x Lude..:? "detectorId")
            Lude.<*> (x Lude..:? "serviceName")
            Lude.<*> (x Lude..:? "userFeedback")
            Lude.<*> (x Lude..:? "evidence")
            Lude.<*> (x Lude..:? "eventLastSeen")
            Lude.<*> (x Lude..:? "resourceRole")
            Lude.<*> (x Lude..:? "archived")
      )
