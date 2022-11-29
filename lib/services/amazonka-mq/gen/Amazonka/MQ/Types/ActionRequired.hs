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
-- Module      : Amazonka.MQ.Types.ActionRequired
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MQ.Types.ActionRequired where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The action required to resolve a broker issue when the broker is in a
-- CRITICAL_ACTION_REQUIRED state.
--
-- /See:/ 'newActionRequired' smart constructor.
data ActionRequired = ActionRequired'
  { -- | Information about the action required to resolve your broker issue when
    -- the broker is in a CRITICAL_ACTION_REQUIRED state.
    actionRequiredInfo :: Prelude.Maybe Prelude.Text,
    -- | The code you can use to resolve your broker issue when the broker is in
    -- a CRITICAL_ACTION_REQUIRED state. You can find instructions by choosing
    -- the link for your code from the list of action required codes in
    -- <https://docs.aws.amazon.com//latest/developer-guide/troubleshooting-action-required-codes.html Amazon MQ action required codes>.
    -- Each code references a topic with detailed information, instructions,
    -- and recommendations for how to resolve the issue and prevent future
    -- occurrences.
    actionRequiredCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActionRequired' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionRequiredInfo', 'actionRequired_actionRequiredInfo' - Information about the action required to resolve your broker issue when
-- the broker is in a CRITICAL_ACTION_REQUIRED state.
--
-- 'actionRequiredCode', 'actionRequired_actionRequiredCode' - The code you can use to resolve your broker issue when the broker is in
-- a CRITICAL_ACTION_REQUIRED state. You can find instructions by choosing
-- the link for your code from the list of action required codes in
-- <https://docs.aws.amazon.com//latest/developer-guide/troubleshooting-action-required-codes.html Amazon MQ action required codes>.
-- Each code references a topic with detailed information, instructions,
-- and recommendations for how to resolve the issue and prevent future
-- occurrences.
newActionRequired ::
  ActionRequired
newActionRequired =
  ActionRequired'
    { actionRequiredInfo =
        Prelude.Nothing,
      actionRequiredCode = Prelude.Nothing
    }

-- | Information about the action required to resolve your broker issue when
-- the broker is in a CRITICAL_ACTION_REQUIRED state.
actionRequired_actionRequiredInfo :: Lens.Lens' ActionRequired (Prelude.Maybe Prelude.Text)
actionRequired_actionRequiredInfo = Lens.lens (\ActionRequired' {actionRequiredInfo} -> actionRequiredInfo) (\s@ActionRequired' {} a -> s {actionRequiredInfo = a} :: ActionRequired)

-- | The code you can use to resolve your broker issue when the broker is in
-- a CRITICAL_ACTION_REQUIRED state. You can find instructions by choosing
-- the link for your code from the list of action required codes in
-- <https://docs.aws.amazon.com//latest/developer-guide/troubleshooting-action-required-codes.html Amazon MQ action required codes>.
-- Each code references a topic with detailed information, instructions,
-- and recommendations for how to resolve the issue and prevent future
-- occurrences.
actionRequired_actionRequiredCode :: Lens.Lens' ActionRequired (Prelude.Maybe Prelude.Text)
actionRequired_actionRequiredCode = Lens.lens (\ActionRequired' {actionRequiredCode} -> actionRequiredCode) (\s@ActionRequired' {} a -> s {actionRequiredCode = a} :: ActionRequired)

instance Core.FromJSON ActionRequired where
  parseJSON =
    Core.withObject
      "ActionRequired"
      ( \x ->
          ActionRequired'
            Prelude.<$> (x Core..:? "actionRequiredInfo")
            Prelude.<*> (x Core..:? "actionRequiredCode")
      )

instance Prelude.Hashable ActionRequired where
  hashWithSalt _salt ActionRequired' {..} =
    _salt `Prelude.hashWithSalt` actionRequiredInfo
      `Prelude.hashWithSalt` actionRequiredCode

instance Prelude.NFData ActionRequired where
  rnf ActionRequired' {..} =
    Prelude.rnf actionRequiredInfo
      `Prelude.seq` Prelude.rnf actionRequiredCode
