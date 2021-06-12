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

-- | Information about an add-on version.
--
-- /See:/ 'newAddonVersionInfo' smart constructor.
data AddonVersionInfo = AddonVersionInfo'
  { -- | An object that represents the compatibilities of a version.
    compatibilities :: Core.Maybe [Compatibility],
    -- | The architectures that the version supports.
    architecture :: Core.Maybe [Core.Text],
    -- | The version of the add-on.
    addonVersion :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { compatibilities = Core.Nothing,
      architecture = Core.Nothing,
      addonVersion = Core.Nothing
    }

-- | An object that represents the compatibilities of a version.
addonVersionInfo_compatibilities :: Lens.Lens' AddonVersionInfo (Core.Maybe [Compatibility])
addonVersionInfo_compatibilities = Lens.lens (\AddonVersionInfo' {compatibilities} -> compatibilities) (\s@AddonVersionInfo' {} a -> s {compatibilities = a} :: AddonVersionInfo) Core.. Lens.mapping Lens._Coerce

-- | The architectures that the version supports.
addonVersionInfo_architecture :: Lens.Lens' AddonVersionInfo (Core.Maybe [Core.Text])
addonVersionInfo_architecture = Lens.lens (\AddonVersionInfo' {architecture} -> architecture) (\s@AddonVersionInfo' {} a -> s {architecture = a} :: AddonVersionInfo) Core.. Lens.mapping Lens._Coerce

-- | The version of the add-on.
addonVersionInfo_addonVersion :: Lens.Lens' AddonVersionInfo (Core.Maybe Core.Text)
addonVersionInfo_addonVersion = Lens.lens (\AddonVersionInfo' {addonVersion} -> addonVersion) (\s@AddonVersionInfo' {} a -> s {addonVersion = a} :: AddonVersionInfo)

instance Core.FromJSON AddonVersionInfo where
  parseJSON =
    Core.withObject
      "AddonVersionInfo"
      ( \x ->
          AddonVersionInfo'
            Core.<$> (x Core..:? "compatibilities" Core..!= Core.mempty)
            Core.<*> (x Core..:? "architecture" Core..!= Core.mempty)
            Core.<*> (x Core..:? "addonVersion")
      )

instance Core.Hashable AddonVersionInfo

instance Core.NFData AddonVersionInfo
