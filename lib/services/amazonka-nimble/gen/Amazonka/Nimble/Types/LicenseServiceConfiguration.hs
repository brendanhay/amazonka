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
-- Module      : Amazonka.Nimble.Types.LicenseServiceConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.LicenseServiceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The configuration for a license service that is associated with a studio
-- resource.
--
-- /See:/ 'newLicenseServiceConfiguration' smart constructor.
data LicenseServiceConfiguration = LicenseServiceConfiguration'
  { -- | The endpoint of the license service that is accessed by the studio
    -- component resource.
    endpoint :: Prelude.Maybe (Core.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LicenseServiceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpoint', 'licenseServiceConfiguration_endpoint' - The endpoint of the license service that is accessed by the studio
-- component resource.
newLicenseServiceConfiguration ::
  LicenseServiceConfiguration
newLicenseServiceConfiguration =
  LicenseServiceConfiguration'
    { endpoint =
        Prelude.Nothing
    }

-- | The endpoint of the license service that is accessed by the studio
-- component resource.
licenseServiceConfiguration_endpoint :: Lens.Lens' LicenseServiceConfiguration (Prelude.Maybe Prelude.Text)
licenseServiceConfiguration_endpoint = Lens.lens (\LicenseServiceConfiguration' {endpoint} -> endpoint) (\s@LicenseServiceConfiguration' {} a -> s {endpoint = a} :: LicenseServiceConfiguration) Prelude.. Lens.mapping Core._Sensitive

instance Core.FromJSON LicenseServiceConfiguration where
  parseJSON =
    Core.withObject
      "LicenseServiceConfiguration"
      ( \x ->
          LicenseServiceConfiguration'
            Prelude.<$> (x Core..:? "endpoint")
      )

instance Prelude.Hashable LicenseServiceConfiguration where
  hashWithSalt _salt LicenseServiceConfiguration' {..} =
    _salt `Prelude.hashWithSalt` endpoint

instance Prelude.NFData LicenseServiceConfiguration where
  rnf LicenseServiceConfiguration' {..} =
    Prelude.rnf endpoint

instance Core.ToJSON LicenseServiceConfiguration where
  toJSON LicenseServiceConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [("endpoint" Core..=) Prelude.<$> endpoint]
      )
