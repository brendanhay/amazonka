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
-- Module      : Amazonka.QuickSight.Types.ExportHiddenFieldsOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ExportHiddenFieldsOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DashboardBehavior

-- |
--
-- /See:/ 'newExportHiddenFieldsOption' smart constructor.
data ExportHiddenFieldsOption = ExportHiddenFieldsOption'
  { availabilityStatus :: Prelude.Maybe DashboardBehavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportHiddenFieldsOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityStatus', 'exportHiddenFieldsOption_availabilityStatus' -
newExportHiddenFieldsOption ::
  ExportHiddenFieldsOption
newExportHiddenFieldsOption =
  ExportHiddenFieldsOption'
    { availabilityStatus =
        Prelude.Nothing
    }

-- |
exportHiddenFieldsOption_availabilityStatus :: Lens.Lens' ExportHiddenFieldsOption (Prelude.Maybe DashboardBehavior)
exportHiddenFieldsOption_availabilityStatus = Lens.lens (\ExportHiddenFieldsOption' {availabilityStatus} -> availabilityStatus) (\s@ExportHiddenFieldsOption' {} a -> s {availabilityStatus = a} :: ExportHiddenFieldsOption)

instance Prelude.Hashable ExportHiddenFieldsOption where
  hashWithSalt _salt ExportHiddenFieldsOption' {..} =
    _salt `Prelude.hashWithSalt` availabilityStatus

instance Prelude.NFData ExportHiddenFieldsOption where
  rnf ExportHiddenFieldsOption' {..} =
    Prelude.rnf availabilityStatus

instance Data.ToJSON ExportHiddenFieldsOption where
  toJSON ExportHiddenFieldsOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AvailabilityStatus" Data..=)
              Prelude.<$> availabilityStatus
          ]
      )
