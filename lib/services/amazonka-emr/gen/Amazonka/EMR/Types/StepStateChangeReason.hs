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
-- Module      : Amazonka.EMR.Types.StepStateChangeReason
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.StepStateChangeReason where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types.StepStateChangeReasonCode
import qualified Amazonka.Prelude as Prelude

-- | The details of the step state change reason.
--
-- /See:/ 'newStepStateChangeReason' smart constructor.
data StepStateChangeReason = StepStateChangeReason'
  { -- | The programmable code for the state change reason. Note: Currently, the
    -- service provides no code for the state change.
    code :: Prelude.Maybe StepStateChangeReasonCode,
    -- | The descriptive message for the state change reason.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StepStateChangeReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'stepStateChangeReason_code' - The programmable code for the state change reason. Note: Currently, the
-- service provides no code for the state change.
--
-- 'message', 'stepStateChangeReason_message' - The descriptive message for the state change reason.
newStepStateChangeReason ::
  StepStateChangeReason
newStepStateChangeReason =
  StepStateChangeReason'
    { code = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The programmable code for the state change reason. Note: Currently, the
-- service provides no code for the state change.
stepStateChangeReason_code :: Lens.Lens' StepStateChangeReason (Prelude.Maybe StepStateChangeReasonCode)
stepStateChangeReason_code = Lens.lens (\StepStateChangeReason' {code} -> code) (\s@StepStateChangeReason' {} a -> s {code = a} :: StepStateChangeReason)

-- | The descriptive message for the state change reason.
stepStateChangeReason_message :: Lens.Lens' StepStateChangeReason (Prelude.Maybe Prelude.Text)
stepStateChangeReason_message = Lens.lens (\StepStateChangeReason' {message} -> message) (\s@StepStateChangeReason' {} a -> s {message = a} :: StepStateChangeReason)

instance Data.FromJSON StepStateChangeReason where
  parseJSON =
    Data.withObject
      "StepStateChangeReason"
      ( \x ->
          StepStateChangeReason'
            Prelude.<$> (x Data..:? "Code")
            Prelude.<*> (x Data..:? "Message")
      )

instance Prelude.Hashable StepStateChangeReason where
  hashWithSalt _salt StepStateChangeReason' {..} =
    _salt `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData StepStateChangeReason where
  rnf StepStateChangeReason' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
