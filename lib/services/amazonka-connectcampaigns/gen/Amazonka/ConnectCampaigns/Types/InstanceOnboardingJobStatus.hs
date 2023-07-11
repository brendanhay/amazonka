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
-- Module      : Amazonka.ConnectCampaigns.Types.InstanceOnboardingJobStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCampaigns.Types.InstanceOnboardingJobStatus where

import Amazonka.ConnectCampaigns.Types.InstanceOnboardingJobFailureCode
import Amazonka.ConnectCampaigns.Types.InstanceOnboardingJobStatusCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Instance onboarding job status object
--
-- /See:/ 'newInstanceOnboardingJobStatus' smart constructor.
data InstanceOnboardingJobStatus = InstanceOnboardingJobStatus'
  { failureCode :: Prelude.Maybe InstanceOnboardingJobFailureCode,
    connectInstanceId :: Prelude.Text,
    status :: InstanceOnboardingJobStatusCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceOnboardingJobStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureCode', 'instanceOnboardingJobStatus_failureCode' - Undocumented member.
--
-- 'connectInstanceId', 'instanceOnboardingJobStatus_connectInstanceId' - Undocumented member.
--
-- 'status', 'instanceOnboardingJobStatus_status' - Undocumented member.
newInstanceOnboardingJobStatus ::
  -- | 'connectInstanceId'
  Prelude.Text ->
  -- | 'status'
  InstanceOnboardingJobStatusCode ->
  InstanceOnboardingJobStatus
newInstanceOnboardingJobStatus
  pConnectInstanceId_
  pStatus_ =
    InstanceOnboardingJobStatus'
      { failureCode =
          Prelude.Nothing,
        connectInstanceId = pConnectInstanceId_,
        status = pStatus_
      }

-- | Undocumented member.
instanceOnboardingJobStatus_failureCode :: Lens.Lens' InstanceOnboardingJobStatus (Prelude.Maybe InstanceOnboardingJobFailureCode)
instanceOnboardingJobStatus_failureCode = Lens.lens (\InstanceOnboardingJobStatus' {failureCode} -> failureCode) (\s@InstanceOnboardingJobStatus' {} a -> s {failureCode = a} :: InstanceOnboardingJobStatus)

-- | Undocumented member.
instanceOnboardingJobStatus_connectInstanceId :: Lens.Lens' InstanceOnboardingJobStatus Prelude.Text
instanceOnboardingJobStatus_connectInstanceId = Lens.lens (\InstanceOnboardingJobStatus' {connectInstanceId} -> connectInstanceId) (\s@InstanceOnboardingJobStatus' {} a -> s {connectInstanceId = a} :: InstanceOnboardingJobStatus)

-- | Undocumented member.
instanceOnboardingJobStatus_status :: Lens.Lens' InstanceOnboardingJobStatus InstanceOnboardingJobStatusCode
instanceOnboardingJobStatus_status = Lens.lens (\InstanceOnboardingJobStatus' {status} -> status) (\s@InstanceOnboardingJobStatus' {} a -> s {status = a} :: InstanceOnboardingJobStatus)

instance Data.FromJSON InstanceOnboardingJobStatus where
  parseJSON =
    Data.withObject
      "InstanceOnboardingJobStatus"
      ( \x ->
          InstanceOnboardingJobStatus'
            Prelude.<$> (x Data..:? "failureCode")
            Prelude.<*> (x Data..: "connectInstanceId")
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable InstanceOnboardingJobStatus where
  hashWithSalt _salt InstanceOnboardingJobStatus' {..} =
    _salt
      `Prelude.hashWithSalt` failureCode
      `Prelude.hashWithSalt` connectInstanceId
      `Prelude.hashWithSalt` status

instance Prelude.NFData InstanceOnboardingJobStatus where
  rnf InstanceOnboardingJobStatus' {..} =
    Prelude.rnf failureCode
      `Prelude.seq` Prelude.rnf connectInstanceId
      `Prelude.seq` Prelude.rnf status
