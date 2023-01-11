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
-- Module      : Amazonka.LicenseManager.Types.LicenseConversionContext
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.LicenseConversionContext where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a license type conversion task.
--
-- /See:/ 'newLicenseConversionContext' smart constructor.
data LicenseConversionContext = LicenseConversionContext'
  { -- | The Usage operation value that corresponds to the license type you are
    -- converting your resource from. For more information about which
    -- platforms correspond to which usage operation values see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/billing-info-fields.html#billing-info Sample data: usage operation by platform>
    usageOperation :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LicenseConversionContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'usageOperation', 'licenseConversionContext_usageOperation' - The Usage operation value that corresponds to the license type you are
-- converting your resource from. For more information about which
-- platforms correspond to which usage operation values see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/billing-info-fields.html#billing-info Sample data: usage operation by platform>
newLicenseConversionContext ::
  LicenseConversionContext
newLicenseConversionContext =
  LicenseConversionContext'
    { usageOperation =
        Prelude.Nothing
    }

-- | The Usage operation value that corresponds to the license type you are
-- converting your resource from. For more information about which
-- platforms correspond to which usage operation values see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/billing-info-fields.html#billing-info Sample data: usage operation by platform>
licenseConversionContext_usageOperation :: Lens.Lens' LicenseConversionContext (Prelude.Maybe Prelude.Text)
licenseConversionContext_usageOperation = Lens.lens (\LicenseConversionContext' {usageOperation} -> usageOperation) (\s@LicenseConversionContext' {} a -> s {usageOperation = a} :: LicenseConversionContext)

instance Data.FromJSON LicenseConversionContext where
  parseJSON =
    Data.withObject
      "LicenseConversionContext"
      ( \x ->
          LicenseConversionContext'
            Prelude.<$> (x Data..:? "UsageOperation")
      )

instance Prelude.Hashable LicenseConversionContext where
  hashWithSalt _salt LicenseConversionContext' {..} =
    _salt `Prelude.hashWithSalt` usageOperation

instance Prelude.NFData LicenseConversionContext where
  rnf LicenseConversionContext' {..} =
    Prelude.rnf usageOperation

instance Data.ToJSON LicenseConversionContext where
  toJSON LicenseConversionContext' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("UsageOperation" Data..=)
              Prelude.<$> usageOperation
          ]
      )
