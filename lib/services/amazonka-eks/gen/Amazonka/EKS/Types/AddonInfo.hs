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
-- Module      : Amazonka.EKS.Types.AddonInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.AddonInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EKS.Types.AddonVersionInfo
import qualified Amazonka.Prelude as Prelude

-- | Information about an add-on.
--
-- /See:/ 'newAddonInfo' smart constructor.
data AddonInfo = AddonInfo'
  { -- | The type of the add-on.
    type' :: Prelude.Maybe Prelude.Text,
    -- | An object representing information about available add-on versions and
    -- compatible Kubernetes versions.
    addonVersions :: Prelude.Maybe [AddonVersionInfo],
    -- | The name of the add-on.
    addonName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddonInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'addonInfo_type' - The type of the add-on.
--
-- 'addonVersions', 'addonInfo_addonVersions' - An object representing information about available add-on versions and
-- compatible Kubernetes versions.
--
-- 'addonName', 'addonInfo_addonName' - The name of the add-on.
newAddonInfo ::
  AddonInfo
newAddonInfo =
  AddonInfo'
    { type' = Prelude.Nothing,
      addonVersions = Prelude.Nothing,
      addonName = Prelude.Nothing
    }

-- | The type of the add-on.
addonInfo_type :: Lens.Lens' AddonInfo (Prelude.Maybe Prelude.Text)
addonInfo_type = Lens.lens (\AddonInfo' {type'} -> type') (\s@AddonInfo' {} a -> s {type' = a} :: AddonInfo)

-- | An object representing information about available add-on versions and
-- compatible Kubernetes versions.
addonInfo_addonVersions :: Lens.Lens' AddonInfo (Prelude.Maybe [AddonVersionInfo])
addonInfo_addonVersions = Lens.lens (\AddonInfo' {addonVersions} -> addonVersions) (\s@AddonInfo' {} a -> s {addonVersions = a} :: AddonInfo) Prelude.. Lens.mapping Lens.coerced

-- | The name of the add-on.
addonInfo_addonName :: Lens.Lens' AddonInfo (Prelude.Maybe Prelude.Text)
addonInfo_addonName = Lens.lens (\AddonInfo' {addonName} -> addonName) (\s@AddonInfo' {} a -> s {addonName = a} :: AddonInfo)

instance Core.FromJSON AddonInfo where
  parseJSON =
    Core.withObject
      "AddonInfo"
      ( \x ->
          AddonInfo'
            Prelude.<$> (x Core..:? "type")
            Prelude.<*> (x Core..:? "addonVersions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "addonName")
      )

instance Prelude.Hashable AddonInfo where
  hashWithSalt _salt AddonInfo' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` addonVersions
      `Prelude.hashWithSalt` addonName

instance Prelude.NFData AddonInfo where
  rnf AddonInfo' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf addonVersions
      `Prelude.seq` Prelude.rnf addonName
