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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MechanicalTurk.Types.NotifyWorkersFailureStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MechanicalTurk.Types.NotifyWorkersFailureCode
import qualified Amazonka.Prelude as Prelude

-- | When MTurk encounters an issue with notifying the Workers you specified,
-- it returns back this object with failure details.
--
-- /See:/ 'newNotifyWorkersFailureStatus' smart constructor.
data NotifyWorkersFailureStatus = NotifyWorkersFailureStatus'
  { -- | Encoded value for the failure type.
    notifyWorkersFailureCode :: Prelude.Maybe NotifyWorkersFailureCode,
    -- | A message detailing the reason the Worker could not be notified.
    notifyWorkersFailureMessage :: Prelude.Maybe Prelude.Text,
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
-- 'notifyWorkersFailureCode', 'notifyWorkersFailureStatus_notifyWorkersFailureCode' - Encoded value for the failure type.
--
-- 'notifyWorkersFailureMessage', 'notifyWorkersFailureStatus_notifyWorkersFailureMessage' - A message detailing the reason the Worker could not be notified.
--
-- 'workerId', 'notifyWorkersFailureStatus_workerId' - The ID of the Worker.
newNotifyWorkersFailureStatus ::
  NotifyWorkersFailureStatus
newNotifyWorkersFailureStatus =
  NotifyWorkersFailureStatus'
    { notifyWorkersFailureCode =
        Prelude.Nothing,
      notifyWorkersFailureMessage = Prelude.Nothing,
      workerId = Prelude.Nothing
    }

-- | Encoded value for the failure type.
notifyWorkersFailureStatus_notifyWorkersFailureCode :: Lens.Lens' NotifyWorkersFailureStatus (Prelude.Maybe NotifyWorkersFailureCode)
notifyWorkersFailureStatus_notifyWorkersFailureCode = Lens.lens (\NotifyWorkersFailureStatus' {notifyWorkersFailureCode} -> notifyWorkersFailureCode) (\s@NotifyWorkersFailureStatus' {} a -> s {notifyWorkersFailureCode = a} :: NotifyWorkersFailureStatus)

-- | A message detailing the reason the Worker could not be notified.
notifyWorkersFailureStatus_notifyWorkersFailureMessage :: Lens.Lens' NotifyWorkersFailureStatus (Prelude.Maybe Prelude.Text)
notifyWorkersFailureStatus_notifyWorkersFailureMessage = Lens.lens (\NotifyWorkersFailureStatus' {notifyWorkersFailureMessage} -> notifyWorkersFailureMessage) (\s@NotifyWorkersFailureStatus' {} a -> s {notifyWorkersFailureMessage = a} :: NotifyWorkersFailureStatus)

-- | The ID of the Worker.
notifyWorkersFailureStatus_workerId :: Lens.Lens' NotifyWorkersFailureStatus (Prelude.Maybe Prelude.Text)
notifyWorkersFailureStatus_workerId = Lens.lens (\NotifyWorkersFailureStatus' {workerId} -> workerId) (\s@NotifyWorkersFailureStatus' {} a -> s {workerId = a} :: NotifyWorkersFailureStatus)

instance Data.FromJSON NotifyWorkersFailureStatus where
  parseJSON =
    Data.withObject
      "NotifyWorkersFailureStatus"
      ( \x ->
          NotifyWorkersFailureStatus'
            Prelude.<$> (x Data..:? "NotifyWorkersFailureCode")
            Prelude.<*> (x Data..:? "NotifyWorkersFailureMessage")
            Prelude.<*> (x Data..:? "WorkerId")
      )

instance Prelude.Hashable NotifyWorkersFailureStatus where
  hashWithSalt _salt NotifyWorkersFailureStatus' {..} =
    _salt
      `Prelude.hashWithSalt` notifyWorkersFailureCode
      `Prelude.hashWithSalt` notifyWorkersFailureMessage
      `Prelude.hashWithSalt` workerId

instance Prelude.NFData NotifyWorkersFailureStatus where
  rnf NotifyWorkersFailureStatus' {..} =
    Prelude.rnf notifyWorkersFailureCode
      `Prelude.seq` Prelude.rnf notifyWorkersFailureMessage
      `Prelude.seq` Prelude.rnf workerId
