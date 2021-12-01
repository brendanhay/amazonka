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
-- Module      : Amazonka.IoTDeviceAdvisor.Types.SuiteRunConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTDeviceAdvisor.Types.SuiteRunConfiguration where

import qualified Amazonka.Core as Core
import Amazonka.IoTDeviceAdvisor.Types.DeviceUnderTest
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Gets suite run configuration.
--
-- /See:/ 'newSuiteRunConfiguration' smart constructor.
data SuiteRunConfiguration = SuiteRunConfiguration'
  { -- | Gets the primary device for suite run.
    primaryDevice :: Prelude.Maybe DeviceUnderTest,
    -- | Gets test case list.
    selectedTestList :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuiteRunConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'primaryDevice', 'suiteRunConfiguration_primaryDevice' - Gets the primary device for suite run.
--
-- 'selectedTestList', 'suiteRunConfiguration_selectedTestList' - Gets test case list.
newSuiteRunConfiguration ::
  SuiteRunConfiguration
newSuiteRunConfiguration =
  SuiteRunConfiguration'
    { primaryDevice =
        Prelude.Nothing,
      selectedTestList = Prelude.Nothing
    }

-- | Gets the primary device for suite run.
suiteRunConfiguration_primaryDevice :: Lens.Lens' SuiteRunConfiguration (Prelude.Maybe DeviceUnderTest)
suiteRunConfiguration_primaryDevice = Lens.lens (\SuiteRunConfiguration' {primaryDevice} -> primaryDevice) (\s@SuiteRunConfiguration' {} a -> s {primaryDevice = a} :: SuiteRunConfiguration)

-- | Gets test case list.
suiteRunConfiguration_selectedTestList :: Lens.Lens' SuiteRunConfiguration (Prelude.Maybe [Prelude.Text])
suiteRunConfiguration_selectedTestList = Lens.lens (\SuiteRunConfiguration' {selectedTestList} -> selectedTestList) (\s@SuiteRunConfiguration' {} a -> s {selectedTestList = a} :: SuiteRunConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON SuiteRunConfiguration where
  parseJSON =
    Core.withObject
      "SuiteRunConfiguration"
      ( \x ->
          SuiteRunConfiguration'
            Prelude.<$> (x Core..:? "primaryDevice")
            Prelude.<*> ( x Core..:? "selectedTestList"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable SuiteRunConfiguration where
  hashWithSalt salt' SuiteRunConfiguration' {..} =
    salt' `Prelude.hashWithSalt` selectedTestList
      `Prelude.hashWithSalt` primaryDevice

instance Prelude.NFData SuiteRunConfiguration where
  rnf SuiteRunConfiguration' {..} =
    Prelude.rnf primaryDevice
      `Prelude.seq` Prelude.rnf selectedTestList

instance Core.ToJSON SuiteRunConfiguration where
  toJSON SuiteRunConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("primaryDevice" Core..=) Prelude.<$> primaryDevice,
            ("selectedTestList" Core..=)
              Prelude.<$> selectedTestList
          ]
      )
