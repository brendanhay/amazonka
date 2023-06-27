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
-- Module      : Amazonka.GuardDuty.Types.AddonDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.AddonDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the installed EKS add-on (GuardDuty security agent).
--
-- /See:/ 'newAddonDetails' smart constructor.
data AddonDetails = AddonDetails'
  { -- | Status of the installed EKS add-on.
    addonStatus :: Prelude.Maybe Prelude.Text,
    -- | Version of the installed EKS add-on.
    addonVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddonDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addonStatus', 'addonDetails_addonStatus' - Status of the installed EKS add-on.
--
-- 'addonVersion', 'addonDetails_addonVersion' - Version of the installed EKS add-on.
newAddonDetails ::
  AddonDetails
newAddonDetails =
  AddonDetails'
    { addonStatus = Prelude.Nothing,
      addonVersion = Prelude.Nothing
    }

-- | Status of the installed EKS add-on.
addonDetails_addonStatus :: Lens.Lens' AddonDetails (Prelude.Maybe Prelude.Text)
addonDetails_addonStatus = Lens.lens (\AddonDetails' {addonStatus} -> addonStatus) (\s@AddonDetails' {} a -> s {addonStatus = a} :: AddonDetails)

-- | Version of the installed EKS add-on.
addonDetails_addonVersion :: Lens.Lens' AddonDetails (Prelude.Maybe Prelude.Text)
addonDetails_addonVersion = Lens.lens (\AddonDetails' {addonVersion} -> addonVersion) (\s@AddonDetails' {} a -> s {addonVersion = a} :: AddonDetails)

instance Data.FromJSON AddonDetails where
  parseJSON =
    Data.withObject
      "AddonDetails"
      ( \x ->
          AddonDetails'
            Prelude.<$> (x Data..:? "addonStatus")
            Prelude.<*> (x Data..:? "addonVersion")
      )

instance Prelude.Hashable AddonDetails where
  hashWithSalt _salt AddonDetails' {..} =
    _salt
      `Prelude.hashWithSalt` addonStatus
      `Prelude.hashWithSalt` addonVersion

instance Prelude.NFData AddonDetails where
  rnf AddonDetails' {..} =
    Prelude.rnf addonStatus
      `Prelude.seq` Prelude.rnf addonVersion
