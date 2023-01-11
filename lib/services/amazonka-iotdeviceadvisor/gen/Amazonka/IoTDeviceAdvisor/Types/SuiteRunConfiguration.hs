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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTDeviceAdvisor.Types.SuiteRunConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTDeviceAdvisor.Types.DeviceUnderTest
import qualified Amazonka.Prelude as Prelude

-- | Gets suite run configuration.
--
-- /See:/ 'newSuiteRunConfiguration' smart constructor.
data SuiteRunConfiguration = SuiteRunConfiguration'
  { -- | TRUE if multiple test suites run in parallel.
    parallelRun :: Prelude.Maybe Prelude.Bool,
    -- | Sets test case list.
    selectedTestList :: Prelude.Maybe [Prelude.Text],
    -- | Sets the primary device for the test suite run. This requires a thing
    -- ARN or a certificate ARN.
    primaryDevice :: DeviceUnderTest
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
-- 'parallelRun', 'suiteRunConfiguration_parallelRun' - TRUE if multiple test suites run in parallel.
--
-- 'selectedTestList', 'suiteRunConfiguration_selectedTestList' - Sets test case list.
--
-- 'primaryDevice', 'suiteRunConfiguration_primaryDevice' - Sets the primary device for the test suite run. This requires a thing
-- ARN or a certificate ARN.
newSuiteRunConfiguration ::
  -- | 'primaryDevice'
  DeviceUnderTest ->
  SuiteRunConfiguration
newSuiteRunConfiguration pPrimaryDevice_ =
  SuiteRunConfiguration'
    { parallelRun =
        Prelude.Nothing,
      selectedTestList = Prelude.Nothing,
      primaryDevice = pPrimaryDevice_
    }

-- | TRUE if multiple test suites run in parallel.
suiteRunConfiguration_parallelRun :: Lens.Lens' SuiteRunConfiguration (Prelude.Maybe Prelude.Bool)
suiteRunConfiguration_parallelRun = Lens.lens (\SuiteRunConfiguration' {parallelRun} -> parallelRun) (\s@SuiteRunConfiguration' {} a -> s {parallelRun = a} :: SuiteRunConfiguration)

-- | Sets test case list.
suiteRunConfiguration_selectedTestList :: Lens.Lens' SuiteRunConfiguration (Prelude.Maybe [Prelude.Text])
suiteRunConfiguration_selectedTestList = Lens.lens (\SuiteRunConfiguration' {selectedTestList} -> selectedTestList) (\s@SuiteRunConfiguration' {} a -> s {selectedTestList = a} :: SuiteRunConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Sets the primary device for the test suite run. This requires a thing
-- ARN or a certificate ARN.
suiteRunConfiguration_primaryDevice :: Lens.Lens' SuiteRunConfiguration DeviceUnderTest
suiteRunConfiguration_primaryDevice = Lens.lens (\SuiteRunConfiguration' {primaryDevice} -> primaryDevice) (\s@SuiteRunConfiguration' {} a -> s {primaryDevice = a} :: SuiteRunConfiguration)

instance Data.FromJSON SuiteRunConfiguration where
  parseJSON =
    Data.withObject
      "SuiteRunConfiguration"
      ( \x ->
          SuiteRunConfiguration'
            Prelude.<$> (x Data..:? "parallelRun")
            Prelude.<*> ( x Data..:? "selectedTestList"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "primaryDevice")
      )

instance Prelude.Hashable SuiteRunConfiguration where
  hashWithSalt _salt SuiteRunConfiguration' {..} =
    _salt `Prelude.hashWithSalt` parallelRun
      `Prelude.hashWithSalt` selectedTestList
      `Prelude.hashWithSalt` primaryDevice

instance Prelude.NFData SuiteRunConfiguration where
  rnf SuiteRunConfiguration' {..} =
    Prelude.rnf parallelRun
      `Prelude.seq` Prelude.rnf selectedTestList
      `Prelude.seq` Prelude.rnf primaryDevice

instance Data.ToJSON SuiteRunConfiguration where
  toJSON SuiteRunConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("parallelRun" Data..=) Prelude.<$> parallelRun,
            ("selectedTestList" Data..=)
              Prelude.<$> selectedTestList,
            Prelude.Just
              ("primaryDevice" Data..= primaryDevice)
          ]
      )
