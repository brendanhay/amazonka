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
-- Module      : Amazonka.LicenseManager.Types.ReportContext
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.ReportContext where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details of the license configuration that this generator reports on.
--
-- /See:/ 'newReportContext' smart constructor.
data ReportContext = ReportContext'
  { -- | Amazon Resource Name (ARN) of the license configuration that this
    -- generator reports on.
    licenseConfigurationArns :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReportContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'licenseConfigurationArns', 'reportContext_licenseConfigurationArns' - Amazon Resource Name (ARN) of the license configuration that this
-- generator reports on.
newReportContext ::
  ReportContext
newReportContext =
  ReportContext'
    { licenseConfigurationArns =
        Prelude.mempty
    }

-- | Amazon Resource Name (ARN) of the license configuration that this
-- generator reports on.
reportContext_licenseConfigurationArns :: Lens.Lens' ReportContext [Prelude.Text]
reportContext_licenseConfigurationArns = Lens.lens (\ReportContext' {licenseConfigurationArns} -> licenseConfigurationArns) (\s@ReportContext' {} a -> s {licenseConfigurationArns = a} :: ReportContext) Prelude.. Lens.coerced

instance Data.FromJSON ReportContext where
  parseJSON =
    Data.withObject
      "ReportContext"
      ( \x ->
          ReportContext'
            Prelude.<$> ( x Data..:? "licenseConfigurationArns"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ReportContext where
  hashWithSalt _salt ReportContext' {..} =
    _salt
      `Prelude.hashWithSalt` licenseConfigurationArns

instance Prelude.NFData ReportContext where
  rnf ReportContext' {..} =
    Prelude.rnf licenseConfigurationArns

instance Data.ToJSON ReportContext where
  toJSON ReportContext' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "licenseConfigurationArns"
                  Data..= licenseConfigurationArns
              )
          ]
      )
