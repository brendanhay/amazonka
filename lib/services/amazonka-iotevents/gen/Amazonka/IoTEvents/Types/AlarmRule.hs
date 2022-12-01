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
-- Module      : Amazonka.IoTEvents.Types.AlarmRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.AlarmRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTEvents.Types.SimpleRule
import qualified Amazonka.Prelude as Prelude

-- | Defines when your alarm is invoked.
--
-- /See:/ 'newAlarmRule' smart constructor.
data AlarmRule = AlarmRule'
  { -- | A rule that compares an input property value to a threshold value with a
    -- comparison operator.
    simpleRule :: Prelude.Maybe SimpleRule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AlarmRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'simpleRule', 'alarmRule_simpleRule' - A rule that compares an input property value to a threshold value with a
-- comparison operator.
newAlarmRule ::
  AlarmRule
newAlarmRule =
  AlarmRule' {simpleRule = Prelude.Nothing}

-- | A rule that compares an input property value to a threshold value with a
-- comparison operator.
alarmRule_simpleRule :: Lens.Lens' AlarmRule (Prelude.Maybe SimpleRule)
alarmRule_simpleRule = Lens.lens (\AlarmRule' {simpleRule} -> simpleRule) (\s@AlarmRule' {} a -> s {simpleRule = a} :: AlarmRule)

instance Core.FromJSON AlarmRule where
  parseJSON =
    Core.withObject
      "AlarmRule"
      ( \x ->
          AlarmRule' Prelude.<$> (x Core..:? "simpleRule")
      )

instance Prelude.Hashable AlarmRule where
  hashWithSalt _salt AlarmRule' {..} =
    _salt `Prelude.hashWithSalt` simpleRule

instance Prelude.NFData AlarmRule where
  rnf AlarmRule' {..} = Prelude.rnf simpleRule

instance Core.ToJSON AlarmRule where
  toJSON AlarmRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [("simpleRule" Core..=) Prelude.<$> simpleRule]
      )
