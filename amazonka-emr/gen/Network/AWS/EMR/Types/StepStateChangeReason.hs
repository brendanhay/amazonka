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
-- Module      : Network.AWS.EMR.Types.StepStateChangeReason
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.StepStateChangeReason where

import Network.AWS.EMR.Types.StepStateChangeReasonCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The details of the step state change reason.
--
-- /See:/ 'newStepStateChangeReason' smart constructor.
data StepStateChangeReason = StepStateChangeReason'
  { -- | The descriptive message for the state change reason.
    message :: Prelude.Maybe Prelude.Text,
    -- | The programmable code for the state change reason. Note: Currently, the
    -- service provides no code for the state change.
    code :: Prelude.Maybe StepStateChangeReasonCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StepStateChangeReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'stepStateChangeReason_message' - The descriptive message for the state change reason.
--
-- 'code', 'stepStateChangeReason_code' - The programmable code for the state change reason. Note: Currently, the
-- service provides no code for the state change.
newStepStateChangeReason ::
  StepStateChangeReason
newStepStateChangeReason =
  StepStateChangeReason'
    { message = Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | The descriptive message for the state change reason.
stepStateChangeReason_message :: Lens.Lens' StepStateChangeReason (Prelude.Maybe Prelude.Text)
stepStateChangeReason_message = Lens.lens (\StepStateChangeReason' {message} -> message) (\s@StepStateChangeReason' {} a -> s {message = a} :: StepStateChangeReason)

-- | The programmable code for the state change reason. Note: Currently, the
-- service provides no code for the state change.
stepStateChangeReason_code :: Lens.Lens' StepStateChangeReason (Prelude.Maybe StepStateChangeReasonCode)
stepStateChangeReason_code = Lens.lens (\StepStateChangeReason' {code} -> code) (\s@StepStateChangeReason' {} a -> s {code = a} :: StepStateChangeReason)

instance Prelude.FromJSON StepStateChangeReason where
  parseJSON =
    Prelude.withObject
      "StepStateChangeReason"
      ( \x ->
          StepStateChangeReason'
            Prelude.<$> (x Prelude..:? "Message")
            Prelude.<*> (x Prelude..:? "Code")
      )

instance Prelude.Hashable StepStateChangeReason

instance Prelude.NFData StepStateChangeReason
