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
-- Module      : Amazonka.AutoScaling.Types.SuspendedProcess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.SuspendedProcess where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an auto scaling process that has been suspended.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-suspend-resume-processes.html#process-types Scaling processes>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- /See:/ 'newSuspendedProcess' smart constructor.
data SuspendedProcess = SuspendedProcess'
  { -- | The name of the suspended process.
    processName :: Prelude.Maybe Prelude.Text,
    -- | The reason that the process was suspended.
    suspensionReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuspendedProcess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'processName', 'suspendedProcess_processName' - The name of the suspended process.
--
-- 'suspensionReason', 'suspendedProcess_suspensionReason' - The reason that the process was suspended.
newSuspendedProcess ::
  SuspendedProcess
newSuspendedProcess =
  SuspendedProcess'
    { processName = Prelude.Nothing,
      suspensionReason = Prelude.Nothing
    }

-- | The name of the suspended process.
suspendedProcess_processName :: Lens.Lens' SuspendedProcess (Prelude.Maybe Prelude.Text)
suspendedProcess_processName = Lens.lens (\SuspendedProcess' {processName} -> processName) (\s@SuspendedProcess' {} a -> s {processName = a} :: SuspendedProcess)

-- | The reason that the process was suspended.
suspendedProcess_suspensionReason :: Lens.Lens' SuspendedProcess (Prelude.Maybe Prelude.Text)
suspendedProcess_suspensionReason = Lens.lens (\SuspendedProcess' {suspensionReason} -> suspensionReason) (\s@SuspendedProcess' {} a -> s {suspensionReason = a} :: SuspendedProcess)

instance Data.FromXML SuspendedProcess where
  parseXML x =
    SuspendedProcess'
      Prelude.<$> (x Data..@? "ProcessName")
      Prelude.<*> (x Data..@? "SuspensionReason")

instance Prelude.Hashable SuspendedProcess where
  hashWithSalt _salt SuspendedProcess' {..} =
    _salt
      `Prelude.hashWithSalt` processName
      `Prelude.hashWithSalt` suspensionReason

instance Prelude.NFData SuspendedProcess where
  rnf SuspendedProcess' {..} =
    Prelude.rnf processName
      `Prelude.seq` Prelude.rnf suspensionReason
