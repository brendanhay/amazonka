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
-- Module      : Amazonka.ControlTower.Types.EnabledControlSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ControlTower.Types.EnabledControlSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A summary of enabled controls.
--
-- /See:/ 'newEnabledControlSummary' smart constructor.
data EnabledControlSummary = EnabledControlSummary'
  { -- | The ARN of the control. Only __Strongly recommended__ and __Elective__
    -- controls are permitted, with the exception of the __Region deny__
    -- guardrail.
    controlIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnabledControlSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controlIdentifier', 'enabledControlSummary_controlIdentifier' - The ARN of the control. Only __Strongly recommended__ and __Elective__
-- controls are permitted, with the exception of the __Region deny__
-- guardrail.
newEnabledControlSummary ::
  EnabledControlSummary
newEnabledControlSummary =
  EnabledControlSummary'
    { controlIdentifier =
        Prelude.Nothing
    }

-- | The ARN of the control. Only __Strongly recommended__ and __Elective__
-- controls are permitted, with the exception of the __Region deny__
-- guardrail.
enabledControlSummary_controlIdentifier :: Lens.Lens' EnabledControlSummary (Prelude.Maybe Prelude.Text)
enabledControlSummary_controlIdentifier = Lens.lens (\EnabledControlSummary' {controlIdentifier} -> controlIdentifier) (\s@EnabledControlSummary' {} a -> s {controlIdentifier = a} :: EnabledControlSummary)

instance Data.FromJSON EnabledControlSummary where
  parseJSON =
    Data.withObject
      "EnabledControlSummary"
      ( \x ->
          EnabledControlSummary'
            Prelude.<$> (x Data..:? "controlIdentifier")
      )

instance Prelude.Hashable EnabledControlSummary where
  hashWithSalt _salt EnabledControlSummary' {..} =
    _salt `Prelude.hashWithSalt` controlIdentifier

instance Prelude.NFData EnabledControlSummary where
  rnf EnabledControlSummary' {..} =
    Prelude.rnf controlIdentifier
