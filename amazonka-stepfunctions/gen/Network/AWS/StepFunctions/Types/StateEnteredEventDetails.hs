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
-- Module      : Network.AWS.StepFunctions.Types.StateEnteredEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.StateEnteredEventDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails

-- | Contains details about a state entered during an execution.
--
-- /See:/ 'newStateEnteredEventDetails' smart constructor.
data StateEnteredEventDetails = StateEnteredEventDetails'
  { -- | Contains details about the input for an execution history event.
    inputDetails :: Prelude.Maybe HistoryEventExecutionDataDetails,
    -- | The string that contains the JSON input data for the state. Length
    -- constraints apply to the payload size, and are expressed as bytes in
    -- UTF-8 encoding.
    input :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The name of the state.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StateEnteredEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputDetails', 'stateEnteredEventDetails_inputDetails' - Contains details about the input for an execution history event.
--
-- 'input', 'stateEnteredEventDetails_input' - The string that contains the JSON input data for the state. Length
-- constraints apply to the payload size, and are expressed as bytes in
-- UTF-8 encoding.
--
-- 'name', 'stateEnteredEventDetails_name' - The name of the state.
newStateEnteredEventDetails ::
  -- | 'name'
  Prelude.Text ->
  StateEnteredEventDetails
newStateEnteredEventDetails pName_ =
  StateEnteredEventDetails'
    { inputDetails =
        Prelude.Nothing,
      input = Prelude.Nothing,
      name = pName_
    }

-- | Contains details about the input for an execution history event.
stateEnteredEventDetails_inputDetails :: Lens.Lens' StateEnteredEventDetails (Prelude.Maybe HistoryEventExecutionDataDetails)
stateEnteredEventDetails_inputDetails = Lens.lens (\StateEnteredEventDetails' {inputDetails} -> inputDetails) (\s@StateEnteredEventDetails' {} a -> s {inputDetails = a} :: StateEnteredEventDetails)

-- | The string that contains the JSON input data for the state. Length
-- constraints apply to the payload size, and are expressed as bytes in
-- UTF-8 encoding.
stateEnteredEventDetails_input :: Lens.Lens' StateEnteredEventDetails (Prelude.Maybe Prelude.Text)
stateEnteredEventDetails_input = Lens.lens (\StateEnteredEventDetails' {input} -> input) (\s@StateEnteredEventDetails' {} a -> s {input = a} :: StateEnteredEventDetails) Prelude.. Lens.mapping Prelude._Sensitive

-- | The name of the state.
stateEnteredEventDetails_name :: Lens.Lens' StateEnteredEventDetails Prelude.Text
stateEnteredEventDetails_name = Lens.lens (\StateEnteredEventDetails' {name} -> name) (\s@StateEnteredEventDetails' {} a -> s {name = a} :: StateEnteredEventDetails)

instance Prelude.FromJSON StateEnteredEventDetails where
  parseJSON =
    Prelude.withObject
      "StateEnteredEventDetails"
      ( \x ->
          StateEnteredEventDetails'
            Prelude.<$> (x Prelude..:? "inputDetails")
            Prelude.<*> (x Prelude..:? "input")
            Prelude.<*> (x Prelude..: "name")
      )

instance Prelude.Hashable StateEnteredEventDetails

instance Prelude.NFData StateEnteredEventDetails
