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
-- Module      : Amazonka.SageMaker.Types.AutoRollbackConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoRollbackConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.Alarm

-- | Currently, the @AutoRollbackConfig@ API is not supported.
--
-- /See:/ 'newAutoRollbackConfig' smart constructor.
data AutoRollbackConfig = AutoRollbackConfig'
  { alarms :: Prelude.Maybe (Prelude.NonEmpty Alarm)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoRollbackConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarms', 'autoRollbackConfig_alarms' -
newAutoRollbackConfig ::
  AutoRollbackConfig
newAutoRollbackConfig =
  AutoRollbackConfig' {alarms = Prelude.Nothing}

-- |
autoRollbackConfig_alarms :: Lens.Lens' AutoRollbackConfig (Prelude.Maybe (Prelude.NonEmpty Alarm))
autoRollbackConfig_alarms = Lens.lens (\AutoRollbackConfig' {alarms} -> alarms) (\s@AutoRollbackConfig' {} a -> s {alarms = a} :: AutoRollbackConfig) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON AutoRollbackConfig where
  parseJSON =
    Core.withObject
      "AutoRollbackConfig"
      ( \x ->
          AutoRollbackConfig'
            Prelude.<$> (x Core..:? "Alarms")
      )

instance Prelude.Hashable AutoRollbackConfig where
  hashWithSalt _salt AutoRollbackConfig' {..} =
    _salt `Prelude.hashWithSalt` alarms

instance Prelude.NFData AutoRollbackConfig where
  rnf AutoRollbackConfig' {..} = Prelude.rnf alarms

instance Core.ToJSON AutoRollbackConfig where
  toJSON AutoRollbackConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Alarms" Core..=) Prelude.<$> alarms]
      )
