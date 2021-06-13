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
-- Module      : Network.AWS.EKS.Types.AddonVersionInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.AddonVersionInfo where

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types.Compatibility
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an add-on version.
--
-- /See:/ 'newAddonVersionInfo' smart constructor.
data AddonVersionInfo = AddonVersionInfo'
  { -- | An object that represents the compatibilities of a version.
    compatibilities :: Prelude.Maybe [Compatibility],
    -- | The architectures that the version supports.
    architecture :: Prelude.Maybe [Prelude.Text],
    -- | The version of the add-on.
    addonVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddonVersionInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compatibilities', 'addonVersionInfo_compatibilities' - An object that represents the compatibilities of a version.
--
-- 'architecture', 'addonVersionInfo_architecture' - The architectures that the version supports.
--
-- 'addonVersion', 'addonVersionInfo_addonVersion' - The version of the add-on.
newAddonVersionInfo ::
  AddonVersionInfo
newAddonVersionInfo =
  AddonVersionInfo'
    { compatibilities =
        Prelude.Nothing,
      architecture = Prelude.Nothing,
      addonVersion = Prelude.Nothing
    }

-- | An object that represents the compatibilities of a version.
addonVersionInfo_compatibilities :: Lens.Lens' AddonVersionInfo (Prelude.Maybe [Compatibility])
addonVersionInfo_compatibilities = Lens.lens (\AddonVersionInfo' {compatibilities} -> compatibilities) (\s@AddonVersionInfo' {} a -> s {compatibilities = a} :: AddonVersionInfo) Prelude.. Lens.mapping Lens._Coerce

-- | The architectures that the version supports.
addonVersionInfo_architecture :: Lens.Lens' AddonVersionInfo (Prelude.Maybe [Prelude.Text])
addonVersionInfo_architecture = Lens.lens (\AddonVersionInfo' {architecture} -> architecture) (\s@AddonVersionInfo' {} a -> s {architecture = a} :: AddonVersionInfo) Prelude.. Lens.mapping Lens._Coerce

-- | The version of the add-on.
addonVersionInfo_addonVersion :: Lens.Lens' AddonVersionInfo (Prelude.Maybe Prelude.Text)
addonVersionInfo_addonVersion = Lens.lens (\AddonVersionInfo' {addonVersion} -> addonVersion) (\s@AddonVersionInfo' {} a -> s {addonVersion = a} :: AddonVersionInfo)

instance Core.FromJSON AddonVersionInfo where
  parseJSON =
    Core.withObject
      "AddonVersionInfo"
      ( \x ->
          AddonVersionInfo'
            Prelude.<$> ( x Core..:? "compatibilities"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "architecture" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "addonVersion")
      )

instance Prelude.Hashable AddonVersionInfo

instance Prelude.NFData AddonVersionInfo
