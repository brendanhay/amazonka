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
-- Module      : Amazonka.StepFunctions.Types.StateMachineAliasListItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.StateMachineAliasListItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details about a specific state machine alias.
--
-- /See:/ 'newStateMachineAliasListItem' smart constructor.
data StateMachineAliasListItem = StateMachineAliasListItem'
  { -- | The Amazon Resource Name (ARN) that identifies a state machine alias.
    -- The alias ARN is a combination of state machine ARN and the alias name
    -- separated by a colon (:). For example, @stateMachineARN:PROD@.
    stateMachineAliasArn :: Prelude.Text,
    -- | The creation date of a state machine alias.
    creationDate :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StateMachineAliasListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stateMachineAliasArn', 'stateMachineAliasListItem_stateMachineAliasArn' - The Amazon Resource Name (ARN) that identifies a state machine alias.
-- The alias ARN is a combination of state machine ARN and the alias name
-- separated by a colon (:). For example, @stateMachineARN:PROD@.
--
-- 'creationDate', 'stateMachineAliasListItem_creationDate' - The creation date of a state machine alias.
newStateMachineAliasListItem ::
  -- | 'stateMachineAliasArn'
  Prelude.Text ->
  -- | 'creationDate'
  Prelude.UTCTime ->
  StateMachineAliasListItem
newStateMachineAliasListItem
  pStateMachineAliasArn_
  pCreationDate_ =
    StateMachineAliasListItem'
      { stateMachineAliasArn =
          pStateMachineAliasArn_,
        creationDate = Data._Time Lens.# pCreationDate_
      }

-- | The Amazon Resource Name (ARN) that identifies a state machine alias.
-- The alias ARN is a combination of state machine ARN and the alias name
-- separated by a colon (:). For example, @stateMachineARN:PROD@.
stateMachineAliasListItem_stateMachineAliasArn :: Lens.Lens' StateMachineAliasListItem Prelude.Text
stateMachineAliasListItem_stateMachineAliasArn = Lens.lens (\StateMachineAliasListItem' {stateMachineAliasArn} -> stateMachineAliasArn) (\s@StateMachineAliasListItem' {} a -> s {stateMachineAliasArn = a} :: StateMachineAliasListItem)

-- | The creation date of a state machine alias.
stateMachineAliasListItem_creationDate :: Lens.Lens' StateMachineAliasListItem Prelude.UTCTime
stateMachineAliasListItem_creationDate = Lens.lens (\StateMachineAliasListItem' {creationDate} -> creationDate) (\s@StateMachineAliasListItem' {} a -> s {creationDate = a} :: StateMachineAliasListItem) Prelude.. Data._Time

instance Data.FromJSON StateMachineAliasListItem where
  parseJSON =
    Data.withObject
      "StateMachineAliasListItem"
      ( \x ->
          StateMachineAliasListItem'
            Prelude.<$> (x Data..: "stateMachineAliasArn")
            Prelude.<*> (x Data..: "creationDate")
      )

instance Prelude.Hashable StateMachineAliasListItem where
  hashWithSalt _salt StateMachineAliasListItem' {..} =
    _salt
      `Prelude.hashWithSalt` stateMachineAliasArn
      `Prelude.hashWithSalt` creationDate

instance Prelude.NFData StateMachineAliasListItem where
  rnf StateMachineAliasListItem' {..} =
    Prelude.rnf stateMachineAliasArn
      `Prelude.seq` Prelude.rnf creationDate
