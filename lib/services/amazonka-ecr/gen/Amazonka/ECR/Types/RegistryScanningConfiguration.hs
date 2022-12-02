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
-- Module      : Amazonka.ECR.Types.RegistryScanningConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.RegistryScanningConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.Types.RegistryScanningRule
import Amazonka.ECR.Types.ScanType
import qualified Amazonka.Prelude as Prelude

-- | The scanning configuration for a private registry.
--
-- /See:/ 'newRegistryScanningConfiguration' smart constructor.
data RegistryScanningConfiguration = RegistryScanningConfiguration'
  { -- | The scanning rules associated with the registry.
    rules :: Prelude.Maybe [RegistryScanningRule],
    -- | The type of scanning configured for the registry.
    scanType :: Prelude.Maybe ScanType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegistryScanningConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rules', 'registryScanningConfiguration_rules' - The scanning rules associated with the registry.
--
-- 'scanType', 'registryScanningConfiguration_scanType' - The type of scanning configured for the registry.
newRegistryScanningConfiguration ::
  RegistryScanningConfiguration
newRegistryScanningConfiguration =
  RegistryScanningConfiguration'
    { rules =
        Prelude.Nothing,
      scanType = Prelude.Nothing
    }

-- | The scanning rules associated with the registry.
registryScanningConfiguration_rules :: Lens.Lens' RegistryScanningConfiguration (Prelude.Maybe [RegistryScanningRule])
registryScanningConfiguration_rules = Lens.lens (\RegistryScanningConfiguration' {rules} -> rules) (\s@RegistryScanningConfiguration' {} a -> s {rules = a} :: RegistryScanningConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The type of scanning configured for the registry.
registryScanningConfiguration_scanType :: Lens.Lens' RegistryScanningConfiguration (Prelude.Maybe ScanType)
registryScanningConfiguration_scanType = Lens.lens (\RegistryScanningConfiguration' {scanType} -> scanType) (\s@RegistryScanningConfiguration' {} a -> s {scanType = a} :: RegistryScanningConfiguration)

instance Data.FromJSON RegistryScanningConfiguration where
  parseJSON =
    Data.withObject
      "RegistryScanningConfiguration"
      ( \x ->
          RegistryScanningConfiguration'
            Prelude.<$> (x Data..:? "rules" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "scanType")
      )

instance
  Prelude.Hashable
    RegistryScanningConfiguration
  where
  hashWithSalt _salt RegistryScanningConfiguration' {..} =
    _salt `Prelude.hashWithSalt` rules
      `Prelude.hashWithSalt` scanType

instance Prelude.NFData RegistryScanningConfiguration where
  rnf RegistryScanningConfiguration' {..} =
    Prelude.rnf rules
      `Prelude.seq` Prelude.rnf scanType
