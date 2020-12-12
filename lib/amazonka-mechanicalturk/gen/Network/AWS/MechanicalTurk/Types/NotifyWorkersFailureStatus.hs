{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.NotifyWorkersFailureStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.NotifyWorkersFailureStatus
  ( NotifyWorkersFailureStatus (..),

    -- * Smart constructor
    mkNotifyWorkersFailureStatus,

    -- * Lenses
    nwfsNotifyWorkersFailureMessage,
    nwfsNotifyWorkersFailureCode,
    nwfsWorkerId,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types.NotifyWorkersFailureCode
import qualified Network.AWS.Prelude as Lude

-- | When MTurk encounters an issue with notifying the Workers you specified, it returns back this object with failure details.
--
-- /See:/ 'mkNotifyWorkersFailureStatus' smart constructor.
data NotifyWorkersFailureStatus = NotifyWorkersFailureStatus'
  { notifyWorkersFailureMessage ::
      Lude.Maybe Lude.Text,
    notifyWorkersFailureCode ::
      Lude.Maybe NotifyWorkersFailureCode,
    workerId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NotifyWorkersFailureStatus' with the minimum fields required to make a request.
--
-- * 'notifyWorkersFailureCode' - Encoded value for the failure type.
-- * 'notifyWorkersFailureMessage' - A message detailing the reason the Worker could not be notified.
-- * 'workerId' - The ID of the Worker.
mkNotifyWorkersFailureStatus ::
  NotifyWorkersFailureStatus
mkNotifyWorkersFailureStatus =
  NotifyWorkersFailureStatus'
    { notifyWorkersFailureMessage =
        Lude.Nothing,
      notifyWorkersFailureCode = Lude.Nothing,
      workerId = Lude.Nothing
    }

-- | A message detailing the reason the Worker could not be notified.
--
-- /Note:/ Consider using 'notifyWorkersFailureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nwfsNotifyWorkersFailureMessage :: Lens.Lens' NotifyWorkersFailureStatus (Lude.Maybe Lude.Text)
nwfsNotifyWorkersFailureMessage = Lens.lens (notifyWorkersFailureMessage :: NotifyWorkersFailureStatus -> Lude.Maybe Lude.Text) (\s a -> s {notifyWorkersFailureMessage = a} :: NotifyWorkersFailureStatus)
{-# DEPRECATED nwfsNotifyWorkersFailureMessage "Use generic-lens or generic-optics with 'notifyWorkersFailureMessage' instead." #-}

-- | Encoded value for the failure type.
--
-- /Note:/ Consider using 'notifyWorkersFailureCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nwfsNotifyWorkersFailureCode :: Lens.Lens' NotifyWorkersFailureStatus (Lude.Maybe NotifyWorkersFailureCode)
nwfsNotifyWorkersFailureCode = Lens.lens (notifyWorkersFailureCode :: NotifyWorkersFailureStatus -> Lude.Maybe NotifyWorkersFailureCode) (\s a -> s {notifyWorkersFailureCode = a} :: NotifyWorkersFailureStatus)
{-# DEPRECATED nwfsNotifyWorkersFailureCode "Use generic-lens or generic-optics with 'notifyWorkersFailureCode' instead." #-}

-- | The ID of the Worker.
--
-- /Note:/ Consider using 'workerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nwfsWorkerId :: Lens.Lens' NotifyWorkersFailureStatus (Lude.Maybe Lude.Text)
nwfsWorkerId = Lens.lens (workerId :: NotifyWorkersFailureStatus -> Lude.Maybe Lude.Text) (\s a -> s {workerId = a} :: NotifyWorkersFailureStatus)
{-# DEPRECATED nwfsWorkerId "Use generic-lens or generic-optics with 'workerId' instead." #-}

instance Lude.FromJSON NotifyWorkersFailureStatus where
  parseJSON =
    Lude.withObject
      "NotifyWorkersFailureStatus"
      ( \x ->
          NotifyWorkersFailureStatus'
            Lude.<$> (x Lude..:? "NotifyWorkersFailureMessage")
            Lude.<*> (x Lude..:? "NotifyWorkersFailureCode")
            Lude.<*> (x Lude..:? "WorkerId")
      )
