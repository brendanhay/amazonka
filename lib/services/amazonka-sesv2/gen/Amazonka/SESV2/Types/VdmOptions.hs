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
-- Module      : Amazonka.SESV2.Types.VdmOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.VdmOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.DashboardOptions
import Amazonka.SESV2.Types.GuardianOptions

-- | An object that defines the VDM settings that apply to emails that you
-- send using the configuration set.
--
-- /See:/ 'newVdmOptions' smart constructor.
data VdmOptions = VdmOptions'
  { -- | Specifies additional settings for your VDM configuration as applicable
    -- to the Dashboard.
    dashboardOptions :: Prelude.Maybe DashboardOptions,
    -- | Specifies additional settings for your VDM configuration as applicable
    -- to the Guardian.
    guardianOptions :: Prelude.Maybe GuardianOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VdmOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dashboardOptions', 'vdmOptions_dashboardOptions' - Specifies additional settings for your VDM configuration as applicable
-- to the Dashboard.
--
-- 'guardianOptions', 'vdmOptions_guardianOptions' - Specifies additional settings for your VDM configuration as applicable
-- to the Guardian.
newVdmOptions ::
  VdmOptions
newVdmOptions =
  VdmOptions'
    { dashboardOptions = Prelude.Nothing,
      guardianOptions = Prelude.Nothing
    }

-- | Specifies additional settings for your VDM configuration as applicable
-- to the Dashboard.
vdmOptions_dashboardOptions :: Lens.Lens' VdmOptions (Prelude.Maybe DashboardOptions)
vdmOptions_dashboardOptions = Lens.lens (\VdmOptions' {dashboardOptions} -> dashboardOptions) (\s@VdmOptions' {} a -> s {dashboardOptions = a} :: VdmOptions)

-- | Specifies additional settings for your VDM configuration as applicable
-- to the Guardian.
vdmOptions_guardianOptions :: Lens.Lens' VdmOptions (Prelude.Maybe GuardianOptions)
vdmOptions_guardianOptions = Lens.lens (\VdmOptions' {guardianOptions} -> guardianOptions) (\s@VdmOptions' {} a -> s {guardianOptions = a} :: VdmOptions)

instance Data.FromJSON VdmOptions where
  parseJSON =
    Data.withObject
      "VdmOptions"
      ( \x ->
          VdmOptions'
            Prelude.<$> (x Data..:? "DashboardOptions")
            Prelude.<*> (x Data..:? "GuardianOptions")
      )

instance Prelude.Hashable VdmOptions where
  hashWithSalt _salt VdmOptions' {..} =
    _salt
      `Prelude.hashWithSalt` dashboardOptions
      `Prelude.hashWithSalt` guardianOptions

instance Prelude.NFData VdmOptions where
  rnf VdmOptions' {..} =
    Prelude.rnf dashboardOptions
      `Prelude.seq` Prelude.rnf guardianOptions

instance Data.ToJSON VdmOptions where
  toJSON VdmOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DashboardOptions" Data..=)
              Prelude.<$> dashboardOptions,
            ("GuardianOptions" Data..=)
              Prelude.<$> guardianOptions
          ]
      )
