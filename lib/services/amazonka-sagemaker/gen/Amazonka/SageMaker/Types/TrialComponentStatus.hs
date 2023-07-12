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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TrialComponentStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.TrialComponentPrimaryStatus

-- | The status of the trial component.
--
-- /See:/ 'newTrialComponentStatus' smart constructor.
data TrialComponentStatus = TrialComponentStatus'
  { -- | If the component failed, a message describing why.
    message :: Prelude.Maybe Prelude.Text,
    -- | The status of the trial component.
    primaryStatus :: Prelude.Maybe TrialComponentPrimaryStatus
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
-- 'message', 'trialComponentStatus_message' - If the component failed, a message describing why.
--
-- 'primaryStatus', 'trialComponentStatus_primaryStatus' - The status of the trial component.
newTrialComponentStatus ::
  TrialComponentStatus
newTrialComponentStatus =
  TrialComponentStatus'
    { message = Prelude.Nothing,
      primaryStatus = Prelude.Nothing
    }

-- | If the component failed, a message describing why.
trialComponentStatus_message :: Lens.Lens' TrialComponentStatus (Prelude.Maybe Prelude.Text)
trialComponentStatus_message = Lens.lens (\TrialComponentStatus' {message} -> message) (\s@TrialComponentStatus' {} a -> s {message = a} :: TrialComponentStatus)

-- | The status of the trial component.
trialComponentStatus_primaryStatus :: Lens.Lens' TrialComponentStatus (Prelude.Maybe TrialComponentPrimaryStatus)
trialComponentStatus_primaryStatus = Lens.lens (\TrialComponentStatus' {primaryStatus} -> primaryStatus) (\s@TrialComponentStatus' {} a -> s {primaryStatus = a} :: TrialComponentStatus)

instance Data.FromJSON TrialComponentStatus where
  parseJSON =
    Data.withObject
      "TrialComponentStatus"
      ( \x ->
          TrialComponentStatus'
            Prelude.<$> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "PrimaryStatus")
      )

instance Prelude.Hashable TrialComponentStatus where
  hashWithSalt _salt TrialComponentStatus' {..} =
    _salt
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` primaryStatus

instance Prelude.NFData TrialComponentStatus where
  rnf TrialComponentStatus' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf primaryStatus

instance Data.ToJSON TrialComponentStatus where
  toJSON TrialComponentStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Message" Data..=) Prelude.<$> message,
            ("PrimaryStatus" Data..=) Prelude.<$> primaryStatus
          ]
      )
