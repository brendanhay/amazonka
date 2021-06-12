{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.NotifyWorkersFailureStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.NotifyWorkersFailureStatus where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types.NotifyWorkersFailureCode

-- | When MTurk encounters an issue with notifying the Workers you specified,
-- it returns back this object with failure details.
--
-- /See:/ 'newNotifyWorkersFailureStatus' smart constructor.
data NotifyWorkersFailureStatus = NotifyWorkersFailureStatus'
  { -- | The ID of the Worker.
    workerId :: Core.Maybe Core.Text,
    -- | Encoded value for the failure type.
    notifyWorkersFailureCode :: Core.Maybe NotifyWorkersFailureCode,
    -- | A message detailing the reason the Worker could not be notified.
    notifyWorkersFailureMessage :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NotifyWorkersFailureStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workerId', 'notifyWorkersFailureStatus_workerId' - The ID of the Worker.
--
-- 'notifyWorkersFailureCode', 'notifyWorkersFailureStatus_notifyWorkersFailureCode' - Encoded value for the failure type.
--
-- 'notifyWorkersFailureMessage', 'notifyWorkersFailureStatus_notifyWorkersFailureMessage' - A message detailing the reason the Worker could not be notified.
newNotifyWorkersFailureStatus ::
  NotifyWorkersFailureStatus
newNotifyWorkersFailureStatus =
  NotifyWorkersFailureStatus'
    { workerId =
        Core.Nothing,
      notifyWorkersFailureCode = Core.Nothing,
      notifyWorkersFailureMessage = Core.Nothing
    }

-- | The ID of the Worker.
notifyWorkersFailureStatus_workerId :: Lens.Lens' NotifyWorkersFailureStatus (Core.Maybe Core.Text)
notifyWorkersFailureStatus_workerId = Lens.lens (\NotifyWorkersFailureStatus' {workerId} -> workerId) (\s@NotifyWorkersFailureStatus' {} a -> s {workerId = a} :: NotifyWorkersFailureStatus)

-- | Encoded value for the failure type.
notifyWorkersFailureStatus_notifyWorkersFailureCode :: Lens.Lens' NotifyWorkersFailureStatus (Core.Maybe NotifyWorkersFailureCode)
notifyWorkersFailureStatus_notifyWorkersFailureCode = Lens.lens (\NotifyWorkersFailureStatus' {notifyWorkersFailureCode} -> notifyWorkersFailureCode) (\s@NotifyWorkersFailureStatus' {} a -> s {notifyWorkersFailureCode = a} :: NotifyWorkersFailureStatus)

-- | A message detailing the reason the Worker could not be notified.
notifyWorkersFailureStatus_notifyWorkersFailureMessage :: Lens.Lens' NotifyWorkersFailureStatus (Core.Maybe Core.Text)
notifyWorkersFailureStatus_notifyWorkersFailureMessage = Lens.lens (\NotifyWorkersFailureStatus' {notifyWorkersFailureMessage} -> notifyWorkersFailureMessage) (\s@NotifyWorkersFailureStatus' {} a -> s {notifyWorkersFailureMessage = a} :: NotifyWorkersFailureStatus)

instance Core.FromJSON NotifyWorkersFailureStatus where
  parseJSON =
    Core.withObject
      "NotifyWorkersFailureStatus"
      ( \x ->
          NotifyWorkersFailureStatus'
            Core.<$> (x Core..:? "WorkerId")
            Core.<*> (x Core..:? "NotifyWorkersFailureCode")
            Core.<*> (x Core..:? "NotifyWorkersFailureMessage")
      )

instance Core.Hashable NotifyWorkersFailureStatus

instance Core.NFData NotifyWorkersFailureStatus
