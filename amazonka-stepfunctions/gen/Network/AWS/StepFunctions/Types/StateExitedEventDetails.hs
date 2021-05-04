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
-- Module      : Network.AWS.StepFunctions.Types.StateExitedEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.StateExitedEventDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails

-- | Contains details about an exit from a state during an execution.
--
-- /See:/ 'newStateExitedEventDetails' smart constructor.
data StateExitedEventDetails = StateExitedEventDetails'
  { -- | The JSON output data of the state. Length constraints apply to the
    -- payload size, and are expressed as bytes in UTF-8 encoding.
    output :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | Contains details about the output of an execution history event.
    outputDetails :: Prelude.Maybe HistoryEventExecutionDataDetails,
    -- | The name of the state.
    --
    -- A name must /not/ contain:
    --
    -- -   white space
    --
    -- -   brackets @\< > { } [ ]@
    --
    -- -   wildcard characters @? *@
    --
    -- -   special characters @\" # % \\ ^ | ~ \` $ & , ; : \/@
    --
    -- -   control characters (@U+0000-001F@, @U+007F-009F@)
    --
    -- To enable logging with CloudWatch Logs, the name should only contain
    -- 0-9, A-Z, a-z, - and _.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StateExitedEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'output', 'stateExitedEventDetails_output' - The JSON output data of the state. Length constraints apply to the
-- payload size, and are expressed as bytes in UTF-8 encoding.
--
-- 'outputDetails', 'stateExitedEventDetails_outputDetails' - Contains details about the output of an execution history event.
--
-- 'name', 'stateExitedEventDetails_name' - The name of the state.
--
-- A name must /not/ contain:
--
-- -   white space
--
-- -   brackets @\< > { } [ ]@
--
-- -   wildcard characters @? *@
--
-- -   special characters @\" # % \\ ^ | ~ \` $ & , ; : \/@
--
-- -   control characters (@U+0000-001F@, @U+007F-009F@)
--
-- To enable logging with CloudWatch Logs, the name should only contain
-- 0-9, A-Z, a-z, - and _.
newStateExitedEventDetails ::
  -- | 'name'
  Prelude.Text ->
  StateExitedEventDetails
newStateExitedEventDetails pName_ =
  StateExitedEventDetails'
    { output = Prelude.Nothing,
      outputDetails = Prelude.Nothing,
      name = pName_
    }

-- | The JSON output data of the state. Length constraints apply to the
-- payload size, and are expressed as bytes in UTF-8 encoding.
stateExitedEventDetails_output :: Lens.Lens' StateExitedEventDetails (Prelude.Maybe Prelude.Text)
stateExitedEventDetails_output = Lens.lens (\StateExitedEventDetails' {output} -> output) (\s@StateExitedEventDetails' {} a -> s {output = a} :: StateExitedEventDetails) Prelude.. Lens.mapping Prelude._Sensitive

-- | Contains details about the output of an execution history event.
stateExitedEventDetails_outputDetails :: Lens.Lens' StateExitedEventDetails (Prelude.Maybe HistoryEventExecutionDataDetails)
stateExitedEventDetails_outputDetails = Lens.lens (\StateExitedEventDetails' {outputDetails} -> outputDetails) (\s@StateExitedEventDetails' {} a -> s {outputDetails = a} :: StateExitedEventDetails)

-- | The name of the state.
--
-- A name must /not/ contain:
--
-- -   white space
--
-- -   brackets @\< > { } [ ]@
--
-- -   wildcard characters @? *@
--
-- -   special characters @\" # % \\ ^ | ~ \` $ & , ; : \/@
--
-- -   control characters (@U+0000-001F@, @U+007F-009F@)
--
-- To enable logging with CloudWatch Logs, the name should only contain
-- 0-9, A-Z, a-z, - and _.
stateExitedEventDetails_name :: Lens.Lens' StateExitedEventDetails Prelude.Text
stateExitedEventDetails_name = Lens.lens (\StateExitedEventDetails' {name} -> name) (\s@StateExitedEventDetails' {} a -> s {name = a} :: StateExitedEventDetails)

instance Prelude.FromJSON StateExitedEventDetails where
  parseJSON =
    Prelude.withObject
      "StateExitedEventDetails"
      ( \x ->
          StateExitedEventDetails'
            Prelude.<$> (x Prelude..:? "output")
            Prelude.<*> (x Prelude..:? "outputDetails")
            Prelude.<*> (x Prelude..: "name")
      )

instance Prelude.Hashable StateExitedEventDetails

instance Prelude.NFData StateExitedEventDetails
