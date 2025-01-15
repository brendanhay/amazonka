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
-- Module      : Amazonka.SESV2.Types.VdmAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.VdmAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.DashboardAttributes
import Amazonka.SESV2.Types.FeatureStatus
import Amazonka.SESV2.Types.GuardianAttributes

-- | The VDM attributes that apply to your Amazon SES account.
--
-- /See:/ 'newVdmAttributes' smart constructor.
data VdmAttributes = VdmAttributes'
  { -- | Specifies additional settings for your VDM configuration as applicable
    -- to the Dashboard.
    dashboardAttributes :: Prelude.Maybe DashboardAttributes,
    -- | Specifies additional settings for your VDM configuration as applicable
    -- to the Guardian.
    guardianAttributes :: Prelude.Maybe GuardianAttributes,
    -- | Specifies the status of your VDM configuration. Can be one of the
    -- following:
    --
    -- -   @ENABLED@ – Amazon SES enables VDM for your account.
    --
    -- -   @DISABLED@ – Amazon SES disables VDM for your account.
    vdmEnabled :: FeatureStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VdmAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dashboardAttributes', 'vdmAttributes_dashboardAttributes' - Specifies additional settings for your VDM configuration as applicable
-- to the Dashboard.
--
-- 'guardianAttributes', 'vdmAttributes_guardianAttributes' - Specifies additional settings for your VDM configuration as applicable
-- to the Guardian.
--
-- 'vdmEnabled', 'vdmAttributes_vdmEnabled' - Specifies the status of your VDM configuration. Can be one of the
-- following:
--
-- -   @ENABLED@ – Amazon SES enables VDM for your account.
--
-- -   @DISABLED@ – Amazon SES disables VDM for your account.
newVdmAttributes ::
  -- | 'vdmEnabled'
  FeatureStatus ->
  VdmAttributes
newVdmAttributes pVdmEnabled_ =
  VdmAttributes'
    { dashboardAttributes =
        Prelude.Nothing,
      guardianAttributes = Prelude.Nothing,
      vdmEnabled = pVdmEnabled_
    }

-- | Specifies additional settings for your VDM configuration as applicable
-- to the Dashboard.
vdmAttributes_dashboardAttributes :: Lens.Lens' VdmAttributes (Prelude.Maybe DashboardAttributes)
vdmAttributes_dashboardAttributes = Lens.lens (\VdmAttributes' {dashboardAttributes} -> dashboardAttributes) (\s@VdmAttributes' {} a -> s {dashboardAttributes = a} :: VdmAttributes)

-- | Specifies additional settings for your VDM configuration as applicable
-- to the Guardian.
vdmAttributes_guardianAttributes :: Lens.Lens' VdmAttributes (Prelude.Maybe GuardianAttributes)
vdmAttributes_guardianAttributes = Lens.lens (\VdmAttributes' {guardianAttributes} -> guardianAttributes) (\s@VdmAttributes' {} a -> s {guardianAttributes = a} :: VdmAttributes)

-- | Specifies the status of your VDM configuration. Can be one of the
-- following:
--
-- -   @ENABLED@ – Amazon SES enables VDM for your account.
--
-- -   @DISABLED@ – Amazon SES disables VDM for your account.
vdmAttributes_vdmEnabled :: Lens.Lens' VdmAttributes FeatureStatus
vdmAttributes_vdmEnabled = Lens.lens (\VdmAttributes' {vdmEnabled} -> vdmEnabled) (\s@VdmAttributes' {} a -> s {vdmEnabled = a} :: VdmAttributes)

instance Data.FromJSON VdmAttributes where
  parseJSON =
    Data.withObject
      "VdmAttributes"
      ( \x ->
          VdmAttributes'
            Prelude.<$> (x Data..:? "DashboardAttributes")
            Prelude.<*> (x Data..:? "GuardianAttributes")
            Prelude.<*> (x Data..: "VdmEnabled")
      )

instance Prelude.Hashable VdmAttributes where
  hashWithSalt _salt VdmAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` dashboardAttributes
      `Prelude.hashWithSalt` guardianAttributes
      `Prelude.hashWithSalt` vdmEnabled

instance Prelude.NFData VdmAttributes where
  rnf VdmAttributes' {..} =
    Prelude.rnf dashboardAttributes `Prelude.seq`
      Prelude.rnf guardianAttributes `Prelude.seq`
        Prelude.rnf vdmEnabled

instance Data.ToJSON VdmAttributes where
  toJSON VdmAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DashboardAttributes" Data..=)
              Prelude.<$> dashboardAttributes,
            ("GuardianAttributes" Data..=)
              Prelude.<$> guardianAttributes,
            Prelude.Just ("VdmEnabled" Data..= vdmEnabled)
          ]
      )
