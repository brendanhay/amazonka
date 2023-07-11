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
-- Module      : Amazonka.IoTDeviceAdvisor.Types.TestResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTDeviceAdvisor.Types.TestResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTDeviceAdvisor.Types.GroupResult
import qualified Amazonka.Prelude as Prelude

-- | Show each group result.
--
-- /See:/ 'newTestResult' smart constructor.
data TestResult = TestResult'
  { -- | Show each group of test results.
    groups :: Prelude.Maybe [GroupResult]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'testResult_groups' - Show each group of test results.
newTestResult ::
  TestResult
newTestResult = TestResult' {groups = Prelude.Nothing}

-- | Show each group of test results.
testResult_groups :: Lens.Lens' TestResult (Prelude.Maybe [GroupResult])
testResult_groups = Lens.lens (\TestResult' {groups} -> groups) (\s@TestResult' {} a -> s {groups = a} :: TestResult) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON TestResult where
  parseJSON =
    Data.withObject
      "TestResult"
      ( \x ->
          TestResult'
            Prelude.<$> (x Data..:? "groups" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable TestResult where
  hashWithSalt _salt TestResult' {..} =
    _salt `Prelude.hashWithSalt` groups

instance Prelude.NFData TestResult where
  rnf TestResult' {..} = Prelude.rnf groups
