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
-- Module      : Amazonka.Connect.Types.ActionSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.ActionSummary where

import Amazonka.Connect.Types.ActionType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an action.
--
-- /See:/ 'newActionSummary' smart constructor.
data ActionSummary = ActionSummary'
  { -- | The action type.
    actionType :: ActionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionType', 'actionSummary_actionType' - The action type.
newActionSummary ::
  -- | 'actionType'
  ActionType ->
  ActionSummary
newActionSummary pActionType_ =
  ActionSummary' {actionType = pActionType_}

-- | The action type.
actionSummary_actionType :: Lens.Lens' ActionSummary ActionType
actionSummary_actionType = Lens.lens (\ActionSummary' {actionType} -> actionType) (\s@ActionSummary' {} a -> s {actionType = a} :: ActionSummary)

instance Data.FromJSON ActionSummary where
  parseJSON =
    Data.withObject
      "ActionSummary"
      ( \x ->
          ActionSummary' Prelude.<$> (x Data..: "ActionType")
      )

instance Prelude.Hashable ActionSummary where
  hashWithSalt _salt ActionSummary' {..} =
    _salt `Prelude.hashWithSalt` actionType

instance Prelude.NFData ActionSummary where
  rnf ActionSummary' {..} = Prelude.rnf actionType
