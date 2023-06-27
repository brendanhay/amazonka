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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MQ.Types.ActionRequired where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Action required for a broker.
--
-- /See:/ 'newActionRequired' smart constructor.
data ActionRequired = ActionRequired'
  { -- | The code you can use to find instructions on the action required to
    -- resolve your broker issue.
    actionRequiredCode :: Prelude.Maybe Prelude.Text,
    -- | Information about the action required to resolve your broker issue.
    actionRequiredInfo :: Prelude.Maybe Prelude.Text
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
-- 'actionRequiredCode', 'actionRequired_actionRequiredCode' - The code you can use to find instructions on the action required to
-- resolve your broker issue.
--
-- 'actionRequiredInfo', 'actionRequired_actionRequiredInfo' - Information about the action required to resolve your broker issue.
newActionRequired ::
  ActionRequired
newActionRequired =
  ActionRequired'
    { actionRequiredCode =
        Prelude.Nothing,
      actionRequiredInfo = Prelude.Nothing
    }

-- | The code you can use to find instructions on the action required to
-- resolve your broker issue.
actionRequired_actionRequiredCode :: Lens.Lens' ActionRequired (Prelude.Maybe Prelude.Text)
actionRequired_actionRequiredCode = Lens.lens (\ActionRequired' {actionRequiredCode} -> actionRequiredCode) (\s@ActionRequired' {} a -> s {actionRequiredCode = a} :: ActionRequired)

-- | Information about the action required to resolve your broker issue.
actionRequired_actionRequiredInfo :: Lens.Lens' ActionRequired (Prelude.Maybe Prelude.Text)
actionRequired_actionRequiredInfo = Lens.lens (\ActionRequired' {actionRequiredInfo} -> actionRequiredInfo) (\s@ActionRequired' {} a -> s {actionRequiredInfo = a} :: ActionRequired)

instance Data.FromJSON ActionRequired where
  parseJSON =
    Data.withObject
      "ActionRequired"
      ( \x ->
          ActionRequired'
            Prelude.<$> (x Data..:? "actionRequiredCode")
            Prelude.<*> (x Data..:? "actionRequiredInfo")
      )

instance Prelude.Hashable ActionRequired where
  hashWithSalt _salt ActionRequired' {..} =
    _salt
      `Prelude.hashWithSalt` actionRequiredCode
      `Prelude.hashWithSalt` actionRequiredInfo

instance Prelude.NFData ActionRequired where
  rnf ActionRequired' {..} =
    Prelude.rnf actionRequiredCode
      `Prelude.seq` Prelude.rnf actionRequiredInfo
