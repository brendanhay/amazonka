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
-- Module      : Amazonka.LicenseManager.Types.ConsumedLicenseSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.ConsumedLicenseSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types.ResourceType
import qualified Amazonka.Prelude as Prelude

-- | Details about license consumption.
--
-- /See:/ 'newConsumedLicenseSummary' smart constructor.
data ConsumedLicenseSummary = ConsumedLicenseSummary'
  { -- | Resource type of the resource consuming a license.
    resourceType :: Prelude.Maybe ResourceType,
    -- | Number of licenses consumed by the resource.
    consumedLicenses :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConsumedLicenseSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'consumedLicenseSummary_resourceType' - Resource type of the resource consuming a license.
--
-- 'consumedLicenses', 'consumedLicenseSummary_consumedLicenses' - Number of licenses consumed by the resource.
newConsumedLicenseSummary ::
  ConsumedLicenseSummary
newConsumedLicenseSummary =
  ConsumedLicenseSummary'
    { resourceType =
        Prelude.Nothing,
      consumedLicenses = Prelude.Nothing
    }

-- | Resource type of the resource consuming a license.
consumedLicenseSummary_resourceType :: Lens.Lens' ConsumedLicenseSummary (Prelude.Maybe ResourceType)
consumedLicenseSummary_resourceType = Lens.lens (\ConsumedLicenseSummary' {resourceType} -> resourceType) (\s@ConsumedLicenseSummary' {} a -> s {resourceType = a} :: ConsumedLicenseSummary)

-- | Number of licenses consumed by the resource.
consumedLicenseSummary_consumedLicenses :: Lens.Lens' ConsumedLicenseSummary (Prelude.Maybe Prelude.Integer)
consumedLicenseSummary_consumedLicenses = Lens.lens (\ConsumedLicenseSummary' {consumedLicenses} -> consumedLicenses) (\s@ConsumedLicenseSummary' {} a -> s {consumedLicenses = a} :: ConsumedLicenseSummary)

instance Data.FromJSON ConsumedLicenseSummary where
  parseJSON =
    Data.withObject
      "ConsumedLicenseSummary"
      ( \x ->
          ConsumedLicenseSummary'
            Prelude.<$> (x Data..:? "ResourceType")
            Prelude.<*> (x Data..:? "ConsumedLicenses")
      )

instance Prelude.Hashable ConsumedLicenseSummary where
  hashWithSalt _salt ConsumedLicenseSummary' {..} =
    _salt `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` consumedLicenses

instance Prelude.NFData ConsumedLicenseSummary where
  rnf ConsumedLicenseSummary' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf consumedLicenses
