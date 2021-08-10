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
-- Module      : Network.AWS.EKS.Types.AddonInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.AddonInfo where

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types.AddonVersionInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an add-on.
--
-- /See:/ 'newAddonInfo' smart constructor.
data AddonInfo = AddonInfo'
  { -- | An object that represents information about available add-on versions
    -- and compatible Kubernetes versions.
    addonVersions :: Prelude.Maybe [AddonVersionInfo],
    -- | The name of the add-on.
    addonName :: Prelude.Maybe Prelude.Text,
    -- | The type of the add-on.
    type' :: Prelude.Maybe Prelude.Text
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
-- 'addonVersions', 'addonInfo_addonVersions' - An object that represents information about available add-on versions
-- and compatible Kubernetes versions.
--
-- 'addonName', 'addonInfo_addonName' - The name of the add-on.
--
-- 'type'', 'addonInfo_type' - The type of the add-on.
newAddonInfo ::
  AddonInfo
newAddonInfo =
  AddonInfo'
    { addonVersions = Prelude.Nothing,
      addonName = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | An object that represents information about available add-on versions
-- and compatible Kubernetes versions.
addonInfo_addonVersions :: Lens.Lens' AddonInfo (Prelude.Maybe [AddonVersionInfo])
addonInfo_addonVersions = Lens.lens (\AddonInfo' {addonVersions} -> addonVersions) (\s@AddonInfo' {} a -> s {addonVersions = a} :: AddonInfo) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the add-on.
addonInfo_addonName :: Lens.Lens' AddonInfo (Prelude.Maybe Prelude.Text)
addonInfo_addonName = Lens.lens (\AddonInfo' {addonName} -> addonName) (\s@AddonInfo' {} a -> s {addonName = a} :: AddonInfo)

-- | The type of the add-on.
addonInfo_type :: Lens.Lens' AddonInfo (Prelude.Maybe Prelude.Text)
addonInfo_type = Lens.lens (\AddonInfo' {type'} -> type') (\s@AddonInfo' {} a -> s {type' = a} :: AddonInfo)

instance Core.FromJSON AddonInfo where
  parseJSON =
    Core.withObject
      "AddonInfo"
      ( \x ->
          AddonInfo'
            Prelude.<$> (x Core..:? "addonVersions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "addonName")
            Prelude.<*> (x Core..:? "type")
      )

instance Prelude.Hashable AddonInfo

instance Prelude.NFData AddonInfo
