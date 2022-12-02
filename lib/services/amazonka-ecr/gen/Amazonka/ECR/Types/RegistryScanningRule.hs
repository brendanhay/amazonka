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
-- Module      : Amazonka.ECR.Types.RegistryScanningRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.RegistryScanningRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.Types.ScanFrequency
import Amazonka.ECR.Types.ScanningRepositoryFilter
import qualified Amazonka.Prelude as Prelude

-- | The details of a scanning rule for a private registry.
--
-- /See:/ 'newRegistryScanningRule' smart constructor.
data RegistryScanningRule = RegistryScanningRule'
  { -- | The frequency that scans are performed at for a private registry. When
    -- the @ENHANCED@ scan type is specified, the supported scan frequencies
    -- are @CONTINUOUS_SCAN@ and @SCAN_ON_PUSH@. When the @BASIC@ scan type is
    -- specified, the @SCAN_ON_PUSH@ and @MANUAL@ scan frequencies are
    -- supported.
    scanFrequency :: ScanFrequency,
    -- | The repository filters associated with the scanning configuration for a
    -- private registry.
    repositoryFilters :: [ScanningRepositoryFilter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegistryScanningRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scanFrequency', 'registryScanningRule_scanFrequency' - The frequency that scans are performed at for a private registry. When
-- the @ENHANCED@ scan type is specified, the supported scan frequencies
-- are @CONTINUOUS_SCAN@ and @SCAN_ON_PUSH@. When the @BASIC@ scan type is
-- specified, the @SCAN_ON_PUSH@ and @MANUAL@ scan frequencies are
-- supported.
--
-- 'repositoryFilters', 'registryScanningRule_repositoryFilters' - The repository filters associated with the scanning configuration for a
-- private registry.
newRegistryScanningRule ::
  -- | 'scanFrequency'
  ScanFrequency ->
  RegistryScanningRule
newRegistryScanningRule pScanFrequency_ =
  RegistryScanningRule'
    { scanFrequency =
        pScanFrequency_,
      repositoryFilters = Prelude.mempty
    }

-- | The frequency that scans are performed at for a private registry. When
-- the @ENHANCED@ scan type is specified, the supported scan frequencies
-- are @CONTINUOUS_SCAN@ and @SCAN_ON_PUSH@. When the @BASIC@ scan type is
-- specified, the @SCAN_ON_PUSH@ and @MANUAL@ scan frequencies are
-- supported.
registryScanningRule_scanFrequency :: Lens.Lens' RegistryScanningRule ScanFrequency
registryScanningRule_scanFrequency = Lens.lens (\RegistryScanningRule' {scanFrequency} -> scanFrequency) (\s@RegistryScanningRule' {} a -> s {scanFrequency = a} :: RegistryScanningRule)

-- | The repository filters associated with the scanning configuration for a
-- private registry.
registryScanningRule_repositoryFilters :: Lens.Lens' RegistryScanningRule [ScanningRepositoryFilter]
registryScanningRule_repositoryFilters = Lens.lens (\RegistryScanningRule' {repositoryFilters} -> repositoryFilters) (\s@RegistryScanningRule' {} a -> s {repositoryFilters = a} :: RegistryScanningRule) Prelude.. Lens.coerced

instance Data.FromJSON RegistryScanningRule where
  parseJSON =
    Data.withObject
      "RegistryScanningRule"
      ( \x ->
          RegistryScanningRule'
            Prelude.<$> (x Data..: "scanFrequency")
            Prelude.<*> ( x Data..:? "repositoryFilters"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable RegistryScanningRule where
  hashWithSalt _salt RegistryScanningRule' {..} =
    _salt `Prelude.hashWithSalt` scanFrequency
      `Prelude.hashWithSalt` repositoryFilters

instance Prelude.NFData RegistryScanningRule where
  rnf RegistryScanningRule' {..} =
    Prelude.rnf scanFrequency
      `Prelude.seq` Prelude.rnf repositoryFilters

instance Data.ToJSON RegistryScanningRule where
  toJSON RegistryScanningRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("scanFrequency" Data..= scanFrequency),
            Prelude.Just
              ("repositoryFilters" Data..= repositoryFilters)
          ]
      )
