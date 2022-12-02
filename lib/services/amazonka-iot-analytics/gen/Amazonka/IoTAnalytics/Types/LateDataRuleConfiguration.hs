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
-- Module      : Amazonka.IoTAnalytics.Types.LateDataRuleConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.LateDataRuleConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types.DeltaTimeSessionWindowConfiguration
import qualified Amazonka.Prelude as Prelude

-- | The information needed to configure a delta time session window.
--
-- /See:/ 'newLateDataRuleConfiguration' smart constructor.
data LateDataRuleConfiguration = LateDataRuleConfiguration'
  { -- | The information needed to configure a delta time session window.
    deltaTimeSessionWindowConfiguration :: Prelude.Maybe DeltaTimeSessionWindowConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LateDataRuleConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deltaTimeSessionWindowConfiguration', 'lateDataRuleConfiguration_deltaTimeSessionWindowConfiguration' - The information needed to configure a delta time session window.
newLateDataRuleConfiguration ::
  LateDataRuleConfiguration
newLateDataRuleConfiguration =
  LateDataRuleConfiguration'
    { deltaTimeSessionWindowConfiguration =
        Prelude.Nothing
    }

-- | The information needed to configure a delta time session window.
lateDataRuleConfiguration_deltaTimeSessionWindowConfiguration :: Lens.Lens' LateDataRuleConfiguration (Prelude.Maybe DeltaTimeSessionWindowConfiguration)
lateDataRuleConfiguration_deltaTimeSessionWindowConfiguration = Lens.lens (\LateDataRuleConfiguration' {deltaTimeSessionWindowConfiguration} -> deltaTimeSessionWindowConfiguration) (\s@LateDataRuleConfiguration' {} a -> s {deltaTimeSessionWindowConfiguration = a} :: LateDataRuleConfiguration)

instance Data.FromJSON LateDataRuleConfiguration where
  parseJSON =
    Data.withObject
      "LateDataRuleConfiguration"
      ( \x ->
          LateDataRuleConfiguration'
            Prelude.<$> (x Data..:? "deltaTimeSessionWindowConfiguration")
      )

instance Prelude.Hashable LateDataRuleConfiguration where
  hashWithSalt _salt LateDataRuleConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` deltaTimeSessionWindowConfiguration

instance Prelude.NFData LateDataRuleConfiguration where
  rnf LateDataRuleConfiguration' {..} =
    Prelude.rnf deltaTimeSessionWindowConfiguration

instance Data.ToJSON LateDataRuleConfiguration where
  toJSON LateDataRuleConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("deltaTimeSessionWindowConfiguration" Data..=)
              Prelude.<$> deltaTimeSessionWindowConfiguration
          ]
      )
