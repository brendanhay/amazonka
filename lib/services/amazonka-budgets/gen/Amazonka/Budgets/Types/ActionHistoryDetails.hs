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
-- Module      : Amazonka.Budgets.Types.ActionHistoryDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Budgets.Types.ActionHistoryDetails where

import Amazonka.Budgets.Types.Action
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The description of the details for the event.
--
-- /See:/ 'newActionHistoryDetails' smart constructor.
data ActionHistoryDetails = ActionHistoryDetails'
  { message :: Prelude.Text,
    -- | The budget action resource.
    action :: Action
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActionHistoryDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'actionHistoryDetails_message' - Undocumented member.
--
-- 'action', 'actionHistoryDetails_action' - The budget action resource.
newActionHistoryDetails ::
  -- | 'message'
  Prelude.Text ->
  -- | 'action'
  Action ->
  ActionHistoryDetails
newActionHistoryDetails pMessage_ pAction_ =
  ActionHistoryDetails'
    { message = pMessage_,
      action = pAction_
    }

-- | Undocumented member.
actionHistoryDetails_message :: Lens.Lens' ActionHistoryDetails Prelude.Text
actionHistoryDetails_message = Lens.lens (\ActionHistoryDetails' {message} -> message) (\s@ActionHistoryDetails' {} a -> s {message = a} :: ActionHistoryDetails)

-- | The budget action resource.
actionHistoryDetails_action :: Lens.Lens' ActionHistoryDetails Action
actionHistoryDetails_action = Lens.lens (\ActionHistoryDetails' {action} -> action) (\s@ActionHistoryDetails' {} a -> s {action = a} :: ActionHistoryDetails)

instance Core.FromJSON ActionHistoryDetails where
  parseJSON =
    Core.withObject
      "ActionHistoryDetails"
      ( \x ->
          ActionHistoryDetails'
            Prelude.<$> (x Core..: "Message")
            Prelude.<*> (x Core..: "Action")
      )

instance Prelude.Hashable ActionHistoryDetails where
  hashWithSalt _salt ActionHistoryDetails' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` action

instance Prelude.NFData ActionHistoryDetails where
  rnf ActionHistoryDetails' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf action
