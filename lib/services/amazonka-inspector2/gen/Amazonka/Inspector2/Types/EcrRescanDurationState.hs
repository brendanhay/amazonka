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
-- Module      : Amazonka.Inspector2.Types.EcrRescanDurationState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.EcrRescanDurationState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.EcrRescanDuration
import Amazonka.Inspector2.Types.EcrRescanDurationStatus
import qualified Amazonka.Prelude as Prelude

-- | Details about the state of any changes to the ECR automated re-scan
-- duration setting.
--
-- /See:/ 'newEcrRescanDurationState' smart constructor.
data EcrRescanDurationState = EcrRescanDurationState'
  { -- | The ECR automated re-scan duration defines how long an ECR image will be
    -- actively scanned by Amazon Inspector. When the number of days since an
    -- image was last pushed exceeds the automated re-scan duration the
    -- monitoring state of that image becomes @inactive@ and all associated
    -- findings are scheduled for closure.
    rescanDuration :: Prelude.Maybe EcrRescanDuration,
    -- | The status of changes to the ECR automated re-scan duration.
    status :: Prelude.Maybe EcrRescanDurationStatus,
    -- | A timestamp representing when the last time the ECR scan duration
    -- setting was changed.
    updatedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EcrRescanDurationState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rescanDuration', 'ecrRescanDurationState_rescanDuration' - The ECR automated re-scan duration defines how long an ECR image will be
-- actively scanned by Amazon Inspector. When the number of days since an
-- image was last pushed exceeds the automated re-scan duration the
-- monitoring state of that image becomes @inactive@ and all associated
-- findings are scheduled for closure.
--
-- 'status', 'ecrRescanDurationState_status' - The status of changes to the ECR automated re-scan duration.
--
-- 'updatedAt', 'ecrRescanDurationState_updatedAt' - A timestamp representing when the last time the ECR scan duration
-- setting was changed.
newEcrRescanDurationState ::
  EcrRescanDurationState
newEcrRescanDurationState =
  EcrRescanDurationState'
    { rescanDuration =
        Prelude.Nothing,
      status = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The ECR automated re-scan duration defines how long an ECR image will be
-- actively scanned by Amazon Inspector. When the number of days since an
-- image was last pushed exceeds the automated re-scan duration the
-- monitoring state of that image becomes @inactive@ and all associated
-- findings are scheduled for closure.
ecrRescanDurationState_rescanDuration :: Lens.Lens' EcrRescanDurationState (Prelude.Maybe EcrRescanDuration)
ecrRescanDurationState_rescanDuration = Lens.lens (\EcrRescanDurationState' {rescanDuration} -> rescanDuration) (\s@EcrRescanDurationState' {} a -> s {rescanDuration = a} :: EcrRescanDurationState)

-- | The status of changes to the ECR automated re-scan duration.
ecrRescanDurationState_status :: Lens.Lens' EcrRescanDurationState (Prelude.Maybe EcrRescanDurationStatus)
ecrRescanDurationState_status = Lens.lens (\EcrRescanDurationState' {status} -> status) (\s@EcrRescanDurationState' {} a -> s {status = a} :: EcrRescanDurationState)

-- | A timestamp representing when the last time the ECR scan duration
-- setting was changed.
ecrRescanDurationState_updatedAt :: Lens.Lens' EcrRescanDurationState (Prelude.Maybe Prelude.UTCTime)
ecrRescanDurationState_updatedAt = Lens.lens (\EcrRescanDurationState' {updatedAt} -> updatedAt) (\s@EcrRescanDurationState' {} a -> s {updatedAt = a} :: EcrRescanDurationState) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON EcrRescanDurationState where
  parseJSON =
    Data.withObject
      "EcrRescanDurationState"
      ( \x ->
          EcrRescanDurationState'
            Prelude.<$> (x Data..:? "rescanDuration")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "updatedAt")
      )

instance Prelude.Hashable EcrRescanDurationState where
  hashWithSalt _salt EcrRescanDurationState' {..} =
    _salt
      `Prelude.hashWithSalt` rescanDuration
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData EcrRescanDurationState where
  rnf EcrRescanDurationState' {..} =
    Prelude.rnf rescanDuration
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf updatedAt
