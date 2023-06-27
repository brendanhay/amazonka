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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.RealTimeAlertConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.RealTimeAlertConfiguration where

import Amazonka.ChimeSdkMediaPipelines.Types.RealTimeAlertRule
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains the configuration settings for real-time
-- alerts.
--
-- /See:/ 'newRealTimeAlertConfiguration' smart constructor.
data RealTimeAlertConfiguration = RealTimeAlertConfiguration'
  { -- | Turns off real-time alerts.
    disabled :: Prelude.Maybe Prelude.Bool,
    -- | The rules in the alert. Rules specify the words or phrases that you want
    -- to be notified about.
    rules :: Prelude.Maybe (Prelude.NonEmpty RealTimeAlertRule)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RealTimeAlertConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disabled', 'realTimeAlertConfiguration_disabled' - Turns off real-time alerts.
--
-- 'rules', 'realTimeAlertConfiguration_rules' - The rules in the alert. Rules specify the words or phrases that you want
-- to be notified about.
newRealTimeAlertConfiguration ::
  RealTimeAlertConfiguration
newRealTimeAlertConfiguration =
  RealTimeAlertConfiguration'
    { disabled =
        Prelude.Nothing,
      rules = Prelude.Nothing
    }

-- | Turns off real-time alerts.
realTimeAlertConfiguration_disabled :: Lens.Lens' RealTimeAlertConfiguration (Prelude.Maybe Prelude.Bool)
realTimeAlertConfiguration_disabled = Lens.lens (\RealTimeAlertConfiguration' {disabled} -> disabled) (\s@RealTimeAlertConfiguration' {} a -> s {disabled = a} :: RealTimeAlertConfiguration)

-- | The rules in the alert. Rules specify the words or phrases that you want
-- to be notified about.
realTimeAlertConfiguration_rules :: Lens.Lens' RealTimeAlertConfiguration (Prelude.Maybe (Prelude.NonEmpty RealTimeAlertRule))
realTimeAlertConfiguration_rules = Lens.lens (\RealTimeAlertConfiguration' {rules} -> rules) (\s@RealTimeAlertConfiguration' {} a -> s {rules = a} :: RealTimeAlertConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON RealTimeAlertConfiguration where
  parseJSON =
    Data.withObject
      "RealTimeAlertConfiguration"
      ( \x ->
          RealTimeAlertConfiguration'
            Prelude.<$> (x Data..:? "Disabled")
            Prelude.<*> (x Data..:? "Rules")
      )

instance Prelude.Hashable RealTimeAlertConfiguration where
  hashWithSalt _salt RealTimeAlertConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` disabled
      `Prelude.hashWithSalt` rules

instance Prelude.NFData RealTimeAlertConfiguration where
  rnf RealTimeAlertConfiguration' {..} =
    Prelude.rnf disabled
      `Prelude.seq` Prelude.rnf rules

instance Data.ToJSON RealTimeAlertConfiguration where
  toJSON RealTimeAlertConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Disabled" Data..=) Prelude.<$> disabled,
            ("Rules" Data..=) Prelude.<$> rules
          ]
      )
