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
-- Module      : Amazonka.SageMaker.Types.TrialComponentStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TrialComponentStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.TrialComponentPrimaryStatus

-- | The status of the trial component.
--
-- /See:/ 'newTrialComponentStatus' smart constructor.
data TrialComponentStatus = TrialComponentStatus'
  { -- | The status of the trial component.
    primaryStatus :: Prelude.Maybe TrialComponentPrimaryStatus,
    -- | If the component failed, a message describing why.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrialComponentStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'primaryStatus', 'trialComponentStatus_primaryStatus' - The status of the trial component.
--
-- 'message', 'trialComponentStatus_message' - If the component failed, a message describing why.
newTrialComponentStatus ::
  TrialComponentStatus
newTrialComponentStatus =
  TrialComponentStatus'
    { primaryStatus =
        Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The status of the trial component.
trialComponentStatus_primaryStatus :: Lens.Lens' TrialComponentStatus (Prelude.Maybe TrialComponentPrimaryStatus)
trialComponentStatus_primaryStatus = Lens.lens (\TrialComponentStatus' {primaryStatus} -> primaryStatus) (\s@TrialComponentStatus' {} a -> s {primaryStatus = a} :: TrialComponentStatus)

-- | If the component failed, a message describing why.
trialComponentStatus_message :: Lens.Lens' TrialComponentStatus (Prelude.Maybe Prelude.Text)
trialComponentStatus_message = Lens.lens (\TrialComponentStatus' {message} -> message) (\s@TrialComponentStatus' {} a -> s {message = a} :: TrialComponentStatus)

instance Core.FromJSON TrialComponentStatus where
  parseJSON =
    Core.withObject
      "TrialComponentStatus"
      ( \x ->
          TrialComponentStatus'
            Prelude.<$> (x Core..:? "PrimaryStatus")
            Prelude.<*> (x Core..:? "Message")
      )

instance Prelude.Hashable TrialComponentStatus

instance Prelude.NFData TrialComponentStatus

instance Core.ToJSON TrialComponentStatus where
  toJSON TrialComponentStatus' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PrimaryStatus" Core..=) Prelude.<$> primaryStatus,
            ("Message" Core..=) Prelude.<$> message
          ]
      )
