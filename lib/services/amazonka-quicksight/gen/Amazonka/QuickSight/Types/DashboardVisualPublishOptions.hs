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
-- Module      : Amazonka.QuickSight.Types.DashboardVisualPublishOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DashboardVisualPublishOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ExportHiddenFieldsOption

-- |
--
-- /See:/ 'newDashboardVisualPublishOptions' smart constructor.
data DashboardVisualPublishOptions = DashboardVisualPublishOptions'
  { exportHiddenFieldsOption :: Prelude.Maybe ExportHiddenFieldsOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DashboardVisualPublishOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportHiddenFieldsOption', 'dashboardVisualPublishOptions_exportHiddenFieldsOption' -
newDashboardVisualPublishOptions ::
  DashboardVisualPublishOptions
newDashboardVisualPublishOptions =
  DashboardVisualPublishOptions'
    { exportHiddenFieldsOption =
        Prelude.Nothing
    }

dashboardVisualPublishOptions_exportHiddenFieldsOption :: Lens.Lens' DashboardVisualPublishOptions (Prelude.Maybe ExportHiddenFieldsOption)
dashboardVisualPublishOptions_exportHiddenFieldsOption = Lens.lens (\DashboardVisualPublishOptions' {exportHiddenFieldsOption} -> exportHiddenFieldsOption) (\s@DashboardVisualPublishOptions' {} a -> s {exportHiddenFieldsOption = a} :: DashboardVisualPublishOptions)

instance
  Prelude.Hashable
    DashboardVisualPublishOptions
  where
  hashWithSalt _salt DashboardVisualPublishOptions' {..} =
    _salt
      `Prelude.hashWithSalt` exportHiddenFieldsOption

instance Prelude.NFData DashboardVisualPublishOptions where
  rnf DashboardVisualPublishOptions' {..} =
    Prelude.rnf exportHiddenFieldsOption

instance Data.ToJSON DashboardVisualPublishOptions where
  toJSON DashboardVisualPublishOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExportHiddenFieldsOption" Data..=)
              Prelude.<$> exportHiddenFieldsOption
          ]
      )
