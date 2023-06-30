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
-- Module      : Amazonka.Inspector2.Types.PackageAggregationResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.PackageAggregationResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.SeverityCounts
import qualified Amazonka.Prelude as Prelude

-- | A response that contains the results of a finding aggregation by image
-- layer.
--
-- /See:/ 'newPackageAggregationResponse' smart constructor.
data PackageAggregationResponse = PackageAggregationResponse'
  { -- | The ID of the Amazon Web Services account associated with the findings.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | An object that contains the count of matched findings per severity.
    severityCounts :: Prelude.Maybe SeverityCounts,
    -- | The name of the operating system package.
    packageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PackageAggregationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'packageAggregationResponse_accountId' - The ID of the Amazon Web Services account associated with the findings.
--
-- 'severityCounts', 'packageAggregationResponse_severityCounts' - An object that contains the count of matched findings per severity.
--
-- 'packageName', 'packageAggregationResponse_packageName' - The name of the operating system package.
newPackageAggregationResponse ::
  -- | 'packageName'
  Prelude.Text ->
  PackageAggregationResponse
newPackageAggregationResponse pPackageName_ =
  PackageAggregationResponse'
    { accountId =
        Prelude.Nothing,
      severityCounts = Prelude.Nothing,
      packageName = pPackageName_
    }

-- | The ID of the Amazon Web Services account associated with the findings.
packageAggregationResponse_accountId :: Lens.Lens' PackageAggregationResponse (Prelude.Maybe Prelude.Text)
packageAggregationResponse_accountId = Lens.lens (\PackageAggregationResponse' {accountId} -> accountId) (\s@PackageAggregationResponse' {} a -> s {accountId = a} :: PackageAggregationResponse)

-- | An object that contains the count of matched findings per severity.
packageAggregationResponse_severityCounts :: Lens.Lens' PackageAggregationResponse (Prelude.Maybe SeverityCounts)
packageAggregationResponse_severityCounts = Lens.lens (\PackageAggregationResponse' {severityCounts} -> severityCounts) (\s@PackageAggregationResponse' {} a -> s {severityCounts = a} :: PackageAggregationResponse)

-- | The name of the operating system package.
packageAggregationResponse_packageName :: Lens.Lens' PackageAggregationResponse Prelude.Text
packageAggregationResponse_packageName = Lens.lens (\PackageAggregationResponse' {packageName} -> packageName) (\s@PackageAggregationResponse' {} a -> s {packageName = a} :: PackageAggregationResponse)

instance Data.FromJSON PackageAggregationResponse where
  parseJSON =
    Data.withObject
      "PackageAggregationResponse"
      ( \x ->
          PackageAggregationResponse'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "severityCounts")
            Prelude.<*> (x Data..: "packageName")
      )

instance Prelude.Hashable PackageAggregationResponse where
  hashWithSalt _salt PackageAggregationResponse' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` severityCounts
      `Prelude.hashWithSalt` packageName

instance Prelude.NFData PackageAggregationResponse where
  rnf PackageAggregationResponse' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf severityCounts
      `Prelude.seq` Prelude.rnf packageName
