{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoTAnalytics.Types.LateDataRuleConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.LateDataRuleConfiguration where

import Network.AWS.IoTAnalytics.Types.DeltaTimeSessionWindowConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The information needed to configure a delta time session window.
--
-- /See:/ 'newLateDataRuleConfiguration' smart constructor.
data LateDataRuleConfiguration = LateDataRuleConfiguration'
  { -- | The information needed to configure a delta time session window.
    deltaTimeSessionWindowConfiguration :: Prelude.Maybe DeltaTimeSessionWindowConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON LateDataRuleConfiguration where
  parseJSON =
    Prelude.withObject
      "LateDataRuleConfiguration"
      ( \x ->
          LateDataRuleConfiguration'
            Prelude.<$> ( x
                            Prelude..:? "deltaTimeSessionWindowConfiguration"
                        )
      )

instance Prelude.Hashable LateDataRuleConfiguration

instance Prelude.NFData LateDataRuleConfiguration

instance Prelude.ToJSON LateDataRuleConfiguration where
  toJSON LateDataRuleConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("deltaTimeSessionWindowConfiguration" Prelude..=)
              Prelude.<$> deltaTimeSessionWindowConfiguration
          ]
      )
