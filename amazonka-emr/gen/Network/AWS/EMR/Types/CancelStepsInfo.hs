{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EMR.Types.CancelStepsInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.CancelStepsInfo where

import Network.AWS.EMR.Types.CancelStepsRequestStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specification of the status of a CancelSteps request. Available only in
-- Amazon EMR version 4.8.0 and later, excluding version 5.0.0.
--
-- /See:/ 'newCancelStepsInfo' smart constructor.
data CancelStepsInfo = CancelStepsInfo'
  { -- | The encrypted StepId of a step.
    stepId :: Prelude.Maybe Prelude.Text,
    -- | The status of a CancelSteps Request. The value may be SUBMITTED or
    -- FAILED.
    status :: Prelude.Maybe CancelStepsRequestStatus,
    -- | The reason for the failure if the CancelSteps request fails.
    reason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CancelStepsInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stepId', 'cancelStepsInfo_stepId' - The encrypted StepId of a step.
--
-- 'status', 'cancelStepsInfo_status' - The status of a CancelSteps Request. The value may be SUBMITTED or
-- FAILED.
--
-- 'reason', 'cancelStepsInfo_reason' - The reason for the failure if the CancelSteps request fails.
newCancelStepsInfo ::
  CancelStepsInfo
newCancelStepsInfo =
  CancelStepsInfo'
    { stepId = Prelude.Nothing,
      status = Prelude.Nothing,
      reason = Prelude.Nothing
    }

-- | The encrypted StepId of a step.
cancelStepsInfo_stepId :: Lens.Lens' CancelStepsInfo (Prelude.Maybe Prelude.Text)
cancelStepsInfo_stepId = Lens.lens (\CancelStepsInfo' {stepId} -> stepId) (\s@CancelStepsInfo' {} a -> s {stepId = a} :: CancelStepsInfo)

-- | The status of a CancelSteps Request. The value may be SUBMITTED or
-- FAILED.
cancelStepsInfo_status :: Lens.Lens' CancelStepsInfo (Prelude.Maybe CancelStepsRequestStatus)
cancelStepsInfo_status = Lens.lens (\CancelStepsInfo' {status} -> status) (\s@CancelStepsInfo' {} a -> s {status = a} :: CancelStepsInfo)

-- | The reason for the failure if the CancelSteps request fails.
cancelStepsInfo_reason :: Lens.Lens' CancelStepsInfo (Prelude.Maybe Prelude.Text)
cancelStepsInfo_reason = Lens.lens (\CancelStepsInfo' {reason} -> reason) (\s@CancelStepsInfo' {} a -> s {reason = a} :: CancelStepsInfo)

instance Prelude.FromJSON CancelStepsInfo where
  parseJSON =
    Prelude.withObject
      "CancelStepsInfo"
      ( \x ->
          CancelStepsInfo'
            Prelude.<$> (x Prelude..:? "StepId")
            Prelude.<*> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "Reason")
      )

instance Prelude.Hashable CancelStepsInfo

instance Prelude.NFData CancelStepsInfo
