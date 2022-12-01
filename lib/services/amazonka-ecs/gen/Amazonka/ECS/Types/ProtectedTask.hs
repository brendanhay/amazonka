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
-- Module      : Amazonka.ECS.Types.ProtectedTask
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.ProtectedTask where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object representing the protection status details for a task. You can
-- set the protection status with the UpdateTaskProtection API and get the
-- status of tasks with the GetTaskProtection API.
--
-- /See:/ 'newProtectedTask' smart constructor.
data ProtectedTask = ProtectedTask'
  { -- | The task ARN.
    taskArn :: Prelude.Maybe Prelude.Text,
    -- | The protection status of the task. If scale-in protection is enabled for
    -- a task, the value is @true@. Otherwise, it is @false@.
    protectionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The epoch time when protection for the task will expire.
    expirationDate :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProtectedTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskArn', 'protectedTask_taskArn' - The task ARN.
--
-- 'protectionEnabled', 'protectedTask_protectionEnabled' - The protection status of the task. If scale-in protection is enabled for
-- a task, the value is @true@. Otherwise, it is @false@.
--
-- 'expirationDate', 'protectedTask_expirationDate' - The epoch time when protection for the task will expire.
newProtectedTask ::
  ProtectedTask
newProtectedTask =
  ProtectedTask'
    { taskArn = Prelude.Nothing,
      protectionEnabled = Prelude.Nothing,
      expirationDate = Prelude.Nothing
    }

-- | The task ARN.
protectedTask_taskArn :: Lens.Lens' ProtectedTask (Prelude.Maybe Prelude.Text)
protectedTask_taskArn = Lens.lens (\ProtectedTask' {taskArn} -> taskArn) (\s@ProtectedTask' {} a -> s {taskArn = a} :: ProtectedTask)

-- | The protection status of the task. If scale-in protection is enabled for
-- a task, the value is @true@. Otherwise, it is @false@.
protectedTask_protectionEnabled :: Lens.Lens' ProtectedTask (Prelude.Maybe Prelude.Bool)
protectedTask_protectionEnabled = Lens.lens (\ProtectedTask' {protectionEnabled} -> protectionEnabled) (\s@ProtectedTask' {} a -> s {protectionEnabled = a} :: ProtectedTask)

-- | The epoch time when protection for the task will expire.
protectedTask_expirationDate :: Lens.Lens' ProtectedTask (Prelude.Maybe Prelude.UTCTime)
protectedTask_expirationDate = Lens.lens (\ProtectedTask' {expirationDate} -> expirationDate) (\s@ProtectedTask' {} a -> s {expirationDate = a} :: ProtectedTask) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON ProtectedTask where
  parseJSON =
    Core.withObject
      "ProtectedTask"
      ( \x ->
          ProtectedTask'
            Prelude.<$> (x Core..:? "taskArn")
            Prelude.<*> (x Core..:? "protectionEnabled")
            Prelude.<*> (x Core..:? "expirationDate")
      )

instance Prelude.Hashable ProtectedTask where
  hashWithSalt _salt ProtectedTask' {..} =
    _salt `Prelude.hashWithSalt` taskArn
      `Prelude.hashWithSalt` protectionEnabled
      `Prelude.hashWithSalt` expirationDate

instance Prelude.NFData ProtectedTask where
  rnf ProtectedTask' {..} =
    Prelude.rnf taskArn
      `Prelude.seq` Prelude.rnf protectionEnabled
      `Prelude.seq` Prelude.rnf expirationDate
