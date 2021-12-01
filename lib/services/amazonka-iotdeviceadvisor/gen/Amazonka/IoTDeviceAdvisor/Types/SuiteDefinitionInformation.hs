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
-- Module      : Amazonka.IoTDeviceAdvisor.Types.SuiteDefinitionInformation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTDeviceAdvisor.Types.SuiteDefinitionInformation where

import qualified Amazonka.Core as Core
import Amazonka.IoTDeviceAdvisor.Types.DeviceUnderTest
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the suite definition.
--
-- /See:/ 'newSuiteDefinitionInformation' smart constructor.
data SuiteDefinitionInformation = SuiteDefinitionInformation'
  { -- | Date (in Unix epoch time) when the test suite was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | Specifies the devices under test for the test suite.
    defaultDevices :: Prelude.Maybe [DeviceUnderTest],
    -- | Suite definition Id of the test suite.
    suiteDefinitionId :: Prelude.Maybe Prelude.Text,
    -- | Suite name of the test suite.
    suiteDefinitionName :: Prelude.Maybe Prelude.Text,
    -- | Specifies if the test suite is intended for qualification.
    intendedForQualification :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuiteDefinitionInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'suiteDefinitionInformation_createdAt' - Date (in Unix epoch time) when the test suite was created.
--
-- 'defaultDevices', 'suiteDefinitionInformation_defaultDevices' - Specifies the devices under test for the test suite.
--
-- 'suiteDefinitionId', 'suiteDefinitionInformation_suiteDefinitionId' - Suite definition Id of the test suite.
--
-- 'suiteDefinitionName', 'suiteDefinitionInformation_suiteDefinitionName' - Suite name of the test suite.
--
-- 'intendedForQualification', 'suiteDefinitionInformation_intendedForQualification' - Specifies if the test suite is intended for qualification.
newSuiteDefinitionInformation ::
  SuiteDefinitionInformation
newSuiteDefinitionInformation =
  SuiteDefinitionInformation'
    { createdAt =
        Prelude.Nothing,
      defaultDevices = Prelude.Nothing,
      suiteDefinitionId = Prelude.Nothing,
      suiteDefinitionName = Prelude.Nothing,
      intendedForQualification = Prelude.Nothing
    }

-- | Date (in Unix epoch time) when the test suite was created.
suiteDefinitionInformation_createdAt :: Lens.Lens' SuiteDefinitionInformation (Prelude.Maybe Prelude.UTCTime)
suiteDefinitionInformation_createdAt = Lens.lens (\SuiteDefinitionInformation' {createdAt} -> createdAt) (\s@SuiteDefinitionInformation' {} a -> s {createdAt = a} :: SuiteDefinitionInformation) Prelude.. Lens.mapping Core._Time

-- | Specifies the devices under test for the test suite.
suiteDefinitionInformation_defaultDevices :: Lens.Lens' SuiteDefinitionInformation (Prelude.Maybe [DeviceUnderTest])
suiteDefinitionInformation_defaultDevices = Lens.lens (\SuiteDefinitionInformation' {defaultDevices} -> defaultDevices) (\s@SuiteDefinitionInformation' {} a -> s {defaultDevices = a} :: SuiteDefinitionInformation) Prelude.. Lens.mapping Lens.coerced

-- | Suite definition Id of the test suite.
suiteDefinitionInformation_suiteDefinitionId :: Lens.Lens' SuiteDefinitionInformation (Prelude.Maybe Prelude.Text)
suiteDefinitionInformation_suiteDefinitionId = Lens.lens (\SuiteDefinitionInformation' {suiteDefinitionId} -> suiteDefinitionId) (\s@SuiteDefinitionInformation' {} a -> s {suiteDefinitionId = a} :: SuiteDefinitionInformation)

-- | Suite name of the test suite.
suiteDefinitionInformation_suiteDefinitionName :: Lens.Lens' SuiteDefinitionInformation (Prelude.Maybe Prelude.Text)
suiteDefinitionInformation_suiteDefinitionName = Lens.lens (\SuiteDefinitionInformation' {suiteDefinitionName} -> suiteDefinitionName) (\s@SuiteDefinitionInformation' {} a -> s {suiteDefinitionName = a} :: SuiteDefinitionInformation)

-- | Specifies if the test suite is intended for qualification.
suiteDefinitionInformation_intendedForQualification :: Lens.Lens' SuiteDefinitionInformation (Prelude.Maybe Prelude.Bool)
suiteDefinitionInformation_intendedForQualification = Lens.lens (\SuiteDefinitionInformation' {intendedForQualification} -> intendedForQualification) (\s@SuiteDefinitionInformation' {} a -> s {intendedForQualification = a} :: SuiteDefinitionInformation)

instance Core.FromJSON SuiteDefinitionInformation where
  parseJSON =
    Core.withObject
      "SuiteDefinitionInformation"
      ( \x ->
          SuiteDefinitionInformation'
            Prelude.<$> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "defaultDevices" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "suiteDefinitionId")
            Prelude.<*> (x Core..:? "suiteDefinitionName")
            Prelude.<*> (x Core..:? "intendedForQualification")
      )

instance Prelude.Hashable SuiteDefinitionInformation where
  hashWithSalt salt' SuiteDefinitionInformation' {..} =
    salt'
      `Prelude.hashWithSalt` intendedForQualification
      `Prelude.hashWithSalt` suiteDefinitionName
      `Prelude.hashWithSalt` suiteDefinitionId
      `Prelude.hashWithSalt` defaultDevices
      `Prelude.hashWithSalt` createdAt

instance Prelude.NFData SuiteDefinitionInformation where
  rnf SuiteDefinitionInformation' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf intendedForQualification
      `Prelude.seq` Prelude.rnf suiteDefinitionName
      `Prelude.seq` Prelude.rnf suiteDefinitionId
      `Prelude.seq` Prelude.rnf defaultDevices
