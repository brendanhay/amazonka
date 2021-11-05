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
-- Module      : Amazonka.MechanicalTurk.Types.NotifyWorkersFailureStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MechanicalTurk.Types.NotifyWorkersFailureStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MechanicalTurk.Types.NotifyWorkersFailureCode
import qualified Amazonka.Prelude as Prelude

-- | When MTurk encounters an issue with notifying the Workers you specified,
-- it returns back this object with failure details.
--
-- /See:/ 'newNotifyWorkersFailureStatus' smart constructor.
data NotifyWorkersFailureStatus = NotifyWorkersFailureStatus'
  { -- | A message detailing the reason the Worker could not be notified.
    notifyWorkersFailureMessage :: Prelude.Maybe Prelude.Text,
    -- | Encoded value for the failure type.
    notifyWorkersFailureCode :: Prelude.Maybe NotifyWorkersFailureCode,
    -- | The ID of the Worker.
    workerId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotifyWorkersFailureStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notifyWorkersFailureMessage', 'notifyWorkersFailureStatus_notifyWorkersFailureMessage' - A message detailing the reason the Worker could not be notified.
--
-- 'notifyWorkersFailureCode', 'notifyWorkersFailureStatus_notifyWorkersFailureCode' - Encoded value for the failure type.
--
-- 'workerId', 'notifyWorkersFailureStatus_workerId' - The ID of the Worker.
newNotifyWorkersFailureStatus ::
  NotifyWorkersFailureStatus
newNotifyWorkersFailureStatus =
  NotifyWorkersFailureStatus'
    { notifyWorkersFailureMessage =
        Prelude.Nothing,
      notifyWorkersFailureCode = Prelude.Nothing,
      workerId = Prelude.Nothing
    }

-- | A message detailing the reason the Worker could not be notified.
notifyWorkersFailureStatus_notifyWorkersFailureMessage :: Lens.Lens' NotifyWorkersFailureStatus (Prelude.Maybe Prelude.Text)
notifyWorkersFailureStatus_notifyWorkersFailureMessage = Lens.lens (\NotifyWorkersFailureStatus' {notifyWorkersFailureMessage} -> notifyWorkersFailureMessage) (\s@NotifyWorkersFailureStatus' {} a -> s {notifyWorkersFailureMessage = a} :: NotifyWorkersFailureStatus)

-- | Encoded value for the failure type.
notifyWorkersFailureStatus_notifyWorkersFailureCode :: Lens.Lens' NotifyWorkersFailureStatus (Prelude.Maybe NotifyWorkersFailureCode)
notifyWorkersFailureStatus_notifyWorkersFailureCode = Lens.lens (\NotifyWorkersFailureStatus' {notifyWorkersFailureCode} -> notifyWorkersFailureCode) (\s@NotifyWorkersFailureStatus' {} a -> s {notifyWorkersFailureCode = a} :: NotifyWorkersFailureStatus)

-- | The ID of the Worker.
notifyWorkersFailureStatus_workerId :: Lens.Lens' NotifyWorkersFailureStatus (Prelude.Maybe Prelude.Text)
notifyWorkersFailureStatus_workerId = Lens.lens (\NotifyWorkersFailureStatus' {workerId} -> workerId) (\s@NotifyWorkersFailureStatus' {} a -> s {workerId = a} :: NotifyWorkersFailureStatus)

instance Core.FromJSON NotifyWorkersFailureStatus where
  parseJSON =
    Core.withObject
      "NotifyWorkersFailureStatus"
      ( \x ->
          NotifyWorkersFailureStatus'
            Prelude.<$> (x Core..:? "NotifyWorkersFailureMessage")
            Prelude.<*> (x Core..:? "NotifyWorkersFailureCode")
            Prelude.<*> (x Core..:? "WorkerId")
      )

instance Prelude.Hashable NotifyWorkersFailureStatus

instance Prelude.NFData NotifyWorkersFailureStatus
