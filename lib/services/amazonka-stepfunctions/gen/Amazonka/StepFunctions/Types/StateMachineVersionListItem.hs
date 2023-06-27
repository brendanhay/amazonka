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
-- Module      : Amazonka.StepFunctions.Types.StateMachineVersionListItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.StateMachineVersionListItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details about a specific state machine version.
--
-- /See:/ 'newStateMachineVersionListItem' smart constructor.
data StateMachineVersionListItem = StateMachineVersionListItem'
  { -- | The Amazon Resource Name (ARN) that identifies a state machine version.
    -- The version ARN is a combination of state machine ARN and the version
    -- number separated by a colon (:). For example, @stateMachineARN:1@.
    stateMachineVersionArn :: Prelude.Text,
    -- | The creation date of a state machine version.
    creationDate :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StateMachineVersionListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stateMachineVersionArn', 'stateMachineVersionListItem_stateMachineVersionArn' - The Amazon Resource Name (ARN) that identifies a state machine version.
-- The version ARN is a combination of state machine ARN and the version
-- number separated by a colon (:). For example, @stateMachineARN:1@.
--
-- 'creationDate', 'stateMachineVersionListItem_creationDate' - The creation date of a state machine version.
newStateMachineVersionListItem ::
  -- | 'stateMachineVersionArn'
  Prelude.Text ->
  -- | 'creationDate'
  Prelude.UTCTime ->
  StateMachineVersionListItem
newStateMachineVersionListItem
  pStateMachineVersionArn_
  pCreationDate_ =
    StateMachineVersionListItem'
      { stateMachineVersionArn =
          pStateMachineVersionArn_,
        creationDate =
          Data._Time Lens.# pCreationDate_
      }

-- | The Amazon Resource Name (ARN) that identifies a state machine version.
-- The version ARN is a combination of state machine ARN and the version
-- number separated by a colon (:). For example, @stateMachineARN:1@.
stateMachineVersionListItem_stateMachineVersionArn :: Lens.Lens' StateMachineVersionListItem Prelude.Text
stateMachineVersionListItem_stateMachineVersionArn = Lens.lens (\StateMachineVersionListItem' {stateMachineVersionArn} -> stateMachineVersionArn) (\s@StateMachineVersionListItem' {} a -> s {stateMachineVersionArn = a} :: StateMachineVersionListItem)

-- | The creation date of a state machine version.
stateMachineVersionListItem_creationDate :: Lens.Lens' StateMachineVersionListItem Prelude.UTCTime
stateMachineVersionListItem_creationDate = Lens.lens (\StateMachineVersionListItem' {creationDate} -> creationDate) (\s@StateMachineVersionListItem' {} a -> s {creationDate = a} :: StateMachineVersionListItem) Prelude.. Data._Time

instance Data.FromJSON StateMachineVersionListItem where
  parseJSON =
    Data.withObject
      "StateMachineVersionListItem"
      ( \x ->
          StateMachineVersionListItem'
            Prelude.<$> (x Data..: "stateMachineVersionArn")
            Prelude.<*> (x Data..: "creationDate")
      )

instance Prelude.Hashable StateMachineVersionListItem where
  hashWithSalt _salt StateMachineVersionListItem' {..} =
    _salt
      `Prelude.hashWithSalt` stateMachineVersionArn
      `Prelude.hashWithSalt` creationDate

instance Prelude.NFData StateMachineVersionListItem where
  rnf StateMachineVersionListItem' {..} =
    Prelude.rnf stateMachineVersionArn
      `Prelude.seq` Prelude.rnf creationDate
