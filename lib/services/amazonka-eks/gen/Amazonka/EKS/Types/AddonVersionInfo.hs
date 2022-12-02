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
-- Module      : Amazonka.EKS.Types.AddonVersionInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.AddonVersionInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types.Compatibility
import qualified Amazonka.Prelude as Prelude

-- | Information about an add-on version.
--
-- /See:/ 'newAddonVersionInfo' smart constructor.
data AddonVersionInfo = AddonVersionInfo'
  { -- | The version of the add-on.
    addonVersion :: Prelude.Maybe Prelude.Text,
    -- | An object representing the compatibilities of a version.
    compatibilities :: Prelude.Maybe [Compatibility],
    -- | The architectures that the version supports.
    architecture :: Prelude.Maybe [Prelude.Text]
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
-- 'addonVersion', 'addonVersionInfo_addonVersion' - The version of the add-on.
--
-- 'compatibilities', 'addonVersionInfo_compatibilities' - An object representing the compatibilities of a version.
--
-- 'architecture', 'addonVersionInfo_architecture' - The architectures that the version supports.
newAddonVersionInfo ::
  AddonVersionInfo
newAddonVersionInfo =
  AddonVersionInfo'
    { addonVersion = Prelude.Nothing,
      compatibilities = Prelude.Nothing,
      architecture = Prelude.Nothing
    }

-- | The version of the add-on.
addonVersionInfo_addonVersion :: Lens.Lens' AddonVersionInfo (Prelude.Maybe Prelude.Text)
addonVersionInfo_addonVersion = Lens.lens (\AddonVersionInfo' {addonVersion} -> addonVersion) (\s@AddonVersionInfo' {} a -> s {addonVersion = a} :: AddonVersionInfo)

-- | An object representing the compatibilities of a version.
addonVersionInfo_compatibilities :: Lens.Lens' AddonVersionInfo (Prelude.Maybe [Compatibility])
addonVersionInfo_compatibilities = Lens.lens (\AddonVersionInfo' {compatibilities} -> compatibilities) (\s@AddonVersionInfo' {} a -> s {compatibilities = a} :: AddonVersionInfo) Prelude.. Lens.mapping Lens.coerced

-- | The architectures that the version supports.
addonVersionInfo_architecture :: Lens.Lens' AddonVersionInfo (Prelude.Maybe [Prelude.Text])
addonVersionInfo_architecture = Lens.lens (\AddonVersionInfo' {architecture} -> architecture) (\s@AddonVersionInfo' {} a -> s {architecture = a} :: AddonVersionInfo) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AddonVersionInfo where
  parseJSON =
    Data.withObject
      "AddonVersionInfo"
      ( \x ->
          AddonVersionInfo'
            Prelude.<$> (x Data..:? "addonVersion")
            Prelude.<*> ( x Data..:? "compatibilities"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "architecture" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AddonVersionInfo where
  hashWithSalt _salt AddonVersionInfo' {..} =
    _salt `Prelude.hashWithSalt` addonVersion
      `Prelude.hashWithSalt` compatibilities
      `Prelude.hashWithSalt` architecture

instance Prelude.NFData AddonVersionInfo where
  rnf AddonVersionInfo' {..} =
    Prelude.rnf addonVersion
      `Prelude.seq` Prelude.rnf compatibilities
      `Prelude.seq` Prelude.rnf architecture
